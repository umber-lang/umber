open Import
open Names

(* TODO: Do codegen directly to x86 assembly rather than going through LLVM.
   
   I feel that implementing a runtime for algebraic effects might be too
   difficult/annoying with LLVM. The coroutine support might be able to do it, but
   unclear. If I control everything then I can work at the bare metal. The difficulty will
   involve more low-level concerns rather than trying to twist LLVM's APIs to do something
   they were really not designed for.

   Some problems:
   - I have to rewrite the whole codegen backend
   - I don't know asm much at all
   - I need to think about register allocation
   - I need to think about calling conventions and implement the C calling convention in
     addition to my own one
   - I can't get optimizations for free

   Maybe I could try doing the coroutine thing and see if it works?
*)

open struct
  open Asm_program
  module Label_name = Label_name
  module Call_conv = Call_conv
  module Value = Value
  module Instr = Instr
  module Basic_block = Basic_block
  module Size = Size
  module Data_decl = Data_decl
  module Bss_decl = Bss_decl
  module Global_decl = Global_decl
end

module Unique_counter () : sig
  type t [@@deriving compare, equal, sexp_of]

  val to_string : t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  module Counter : sig
    type id := t
    type t

    val create : unit -> t
    val next : t -> id
  end
end = struct
  include Int

  module Counter = struct
    type t = int ref

    let create () = ref 0

    let next t =
      let id = !t in
      incr t;
      id
    ;;
  end
end

module Virtual_register = Unique_counter ()

module Register = struct
  module T = struct
    type t =
      | Real of Asm_program.Register.t
      | Virtual of Virtual_register.t
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Reg_alloc : sig
  val allocate
    :  Register.t Basic_block.t list
    -> Asm_program.Register.t Basic_block.t list
end = struct
  (* TODO: Handle spilling. Per https://en.wikipedia.org/wiki/Brooks%27_theorem, 
     it's sufficient to check that no node has degree >= # of usable registers. *)

  (* FIXME: Let's just do something very dumb - never reuse any registers within a given
     function. Trying to be clever and re-use registers by handling lifetimes can wait
     until later. It's not clear to me the best way to structure the code, though it feels
     very painful to do without SSA form letting me compute a CFG to then get the
     interference graph.
     
     Ok, this can't work - it means we can never call another function!
   *)

  module Cfg_node = struct
    type t =
      | Label of Label_name.t
      | Ret
    [@@deriving sexp_of, compare, equal, hash]
  end

  module Cfg = struct
    module G = Graph.Imperative.Digraph.Concrete (Cfg_node)
    include G
    module Topologically_sorted_components = Graph.Components.Make (G)
  end

  let create_cfg ~(basic_blocks : _ Basic_block.t list) =
    let cfg = Cfg.create () in
    List.iter basic_blocks ~f:(fun { label; code = _; terminal } ->
      Cfg.add_vertex cfg (Label label);
      let other_nodes : Cfg_node.t list =
        match terminal with
        | Ret -> [ Ret ]
        | Jump label -> [ Label label ]
        | Jump_if { cond = _; then_; else_ } -> [ Label then_; Label else_ ]
      in
      List.iter other_nodes ~f:(fun node -> Cfg.add_edge cfg (Label label) node));
    cfg
  ;;

  let fold_cfg_backwards ~cfg ~init ~f =
    Cfg.Topologically_sorted_components.scc_array cfg
    |> Array.fold ~init ~f:(fun acc component ->
         match component with
         | [ cfg_node ] -> f acc cfg_node
         | _ :: _ :: _ ->
           (* TODO: Think about how liveness analysis should handle cycles. *)
           failwith "TODO: Cycles in CFG"
         | [] -> compiler_bug [%message "Empty cfg component"])
  ;;

  module Interference_graph = struct
    module G = Graph.Imperative.Graph.Concrete (Register)
    include G
    include Graph.Coloring.Make (G)
  end

  module Register_op = struct
    (* FIXME: Add Clobber here *)
    type t =
      | Use
      | Assignment
  end

  (* FIXME: Handle Call properly! 
     It uses the arg registers (for its arity).
     It assigns to the return register.
     It may assign to other caller-saved registers (they are clobbered)
     => I guess that counts as an assignment but we need extra handling to maintain the
        correct register state afterward

     We need to know the calling convention and arity of the function being called.
  *)
  (* TODO: Move to Instr module? *)
  let instr_register_ops (instr : _ Instr.Nonterminal.t)
    : (Register_op.t * _ Value.t) list
    =
    match instr with
    | Mov { dst; src } | Lea { dst; src } -> [ Use, src; Assignment, dst ]
    | And { dst; src } ->
      (* NOTE: It's important that [Use, dst] comes before [Assignment, dst] for the logic
         calculating lifetimes to work. This reflectts the reality that the register is
         used *before* being assigned to. *)
      [ Use, src; Use, dst; Assignment, dst ]
    | Call { fun_; call_conv; arity } ->
      let args =
        Call_conv.arg_registers call_conv
        |> Nonempty.to_list
        |> Fn.flip List.take arity
        |> List.map ~f:(fun reg : (Register_op.t * Register.t Value.t) ->
             Use, Register (Real reg))
      in
      let return_value : Register_op.t * Register.t Value.t =
        Assignment, Register (Real (Call_conv.return_value_register call_conv))
      in
      (Use, fun_) :: (args @ [ return_value ])
    | Cmp (a, b) | Test (a, b) -> [ Use, a; Use, b ]
  ;;

  let create_interference_graph ~cfg ~(basic_blocks : _ Basic_block.t list) =
    (* To create a lifetime interference graph from a control-flow graph (CFG), we have
       to do a backwards traversal over the control flow graph, starting at the return
       points and moving backwards through jumps. At each step, whenever we encounter a
       use of a register, we know it must be live. We can trace each use back to an
       assignment to find the lifetime of the value in the register. *)
    let basic_blocks =
      List.map basic_blocks ~f:(fun bb -> bb.label, bb) |> Label_name.Table.of_alist_exn
    in
    let graph = Interference_graph.create () in
    let record_register_op live_registers (op : Register_op.t) value =
      match op with
      | Use ->
        (* When a variable is used, we know it must be live at this point. *)
        let live_registers = Value.fold_registers value ~init:live_registers ~f:Set.add in
        Set.iter live_registers ~f:(fun reg1 ->
          Set.iter live_registers ~f:(fun reg2 ->
            if not (Register.equal reg1 reg2)
            then Interference_graph.add_edge graph reg1 reg2));
        live_registers
      | Assignment ->
        (* When a variable is assigned to, it doesn't have to be live before this
           point anymore. There is a "hole" in the lifetime between the assignment
           and the previous use where the register isn't needed. *)
        Value.fold_registers value ~init:live_registers ~f:Set.remove
    in
    let (_ : Register.Set.t) =
      fold_cfg_backwards ~cfg ~init:Register.Set.empty ~f:(fun live_registers cfg_node ->
        eprint_s
          [%here]
          [%lazy_message
            "Walking cfg node" (cfg_node : Cfg_node.t) (live_registers : Register.Set.t)];
        match cfg_node with
        | Ret ->
          (* Ret implicitly uses the return register. *)
          (* FIXME: Decide if it actually matters if we track this or not.  *)
          (* record_value
            live_registers
            Use
            (Register (Real (Call_conv.return_value_register Umber))) *)
          live_registers
        | Label label ->
          let ({ label = _; code; terminal = _ } : _ Basic_block.t) =
            Hashtbl.find_exn basic_blocks label
          in
          List.fold_right code ~init:live_registers ~f:(fun instr live_registers ->
            List.fold_right
              (instr_register_ops instr)
              ~init:live_registers
              ~f:(fun (op, value) live_registers ->
              record_register_op live_registers op value)))
    in
    graph
  ;;

  (** Find cases where multiple registers do not interfere with each other and are
      related by moves, and coalesce them into one virtual register.*)
  let coalesce_registers
    ~(basic_blocks : Register.t Basic_block.t list)
    ~interference_graph
    =
    let coalesced_registers = Virtual_register.Table.create () in
    List.iter basic_blocks ~f:(fun { label = _; code; terminal = _ } ->
      List.iter code ~f:(fun instr ->
        match instr with
        | Mov { src = Register src_reg; dst = Register dst_reg } ->
          (match src_reg, dst_reg with
           | Real _, Real _ -> ()
           | (Virtual reg_to_coalesce as node), ((Real _ | Virtual _) as other_reg)
           | (Real _ as other_reg), (Virtual reg_to_coalesce as node) ->
             if not (Interference_graph.mem_edge interference_graph src_reg dst_reg)
             then (
               ignore
                 (Hashtbl.add coalesced_registers ~key:reg_to_coalesce ~data:other_reg
                   : [ `Ok | `Duplicate ]);
               if Interference_graph.mem_vertex interference_graph node
               then (
                 let neighbours = Interference_graph.succ interference_graph node in
                 List.iter neighbours ~f:(fun neighbour ->
                   Interference_graph.add_edge interference_graph other_reg neighbour);
                 Interference_graph.remove_vertex interference_graph node)))
        | _ -> ()));
    let basic_blocks =
      List.map basic_blocks ~f:(fun { label; code; terminal } : _ Basic_block.t ->
        let code =
          List.filter_map code ~f:(fun instr ->
            let instr =
              Instr.Nonterminal.map_args instr ~f:(fun value ->
                Value.map_registers value ~f:(fun reg ->
                  match reg with
                  | Real _ -> reg
                  | Virtual virtual_reg ->
                    Hashtbl.find coalesced_registers virtual_reg
                    |> Option.value ~default:reg))
            in
            (* Drop no-op moves which may have appeared due to coalescing. *)
            match instr with
            | Mov { src = Register src_reg; dst = Register dst_reg }
              when Register.equal src_reg dst_reg -> None
            | _ -> Some instr)
        in
        { label; code; terminal })
    in
    eprint_s
      [%here]
      [%lazy_message
        "After coalescing"
          (coalesced_registers : Register.t Virtual_register.Table.t)
          (basic_blocks : Register.t Basic_block.t list)];
    basic_blocks
  ;;

  let color_and_allocate
    ~(basic_blocks : Register.t Basic_block.t list)
    ~interference_graph
    =
    let all_available_registers =
      Call_conv.all_available_registers Umber |> Asm_program.Register.Set.of_list
    in
    let coloring =
      Interference_graph.coloring interference_graph (Set.length all_available_registers)
    in
    (* Once we have a coloring, we need to associate colors to real registers. All the
       real registers need to be processed first, since those constraints are fixed.  *)
    let real_registers_by_color = Int.Table.create () in
    let available_registers =
      Interference_graph.fold_vertex
        (fun node available_registers ->
          match node with
          | Real reg ->
            (* FIXME: We can't have multiple real registers with the same color, right?
               Or maybe it's ok, it just means we can pick an arbitrary one? 
               
               Could this mess things up? *)
            let color = Interference_graph.H.find coloring node in
            Hashtbl.update real_registers_by_color color ~f:(function
              | Some existing -> Nonempty.cons reg existing
              | None -> [ reg ]);
            assert_or_compiler_bug (Set.mem available_registers reg) ~here:[%here];
            Set.remove available_registers reg
          | Virtual _ -> available_registers)
        interference_graph
        all_available_registers
    in
    let allocate_register available_registers (reg : Register.t) =
      match reg with
      | Real reg -> available_registers, reg
      | Virtual _ as node ->
        (match Interference_graph.H.find_opt coloring node with
         | Some color ->
           (* FIXME: Rather than picking randomly, we could try to pick registers we
              already want to move to? i.e. go looking for moves to that register or
              something.
              
              Look at https://en.wikipedia.org/wiki/Register_allocation

              Maybe we want the "Coalesce" step here to merge lifetimes of registers
              related by coping?
              *)
           let (reg_to_use :: _other_regs) =
             Hashtbl.find_or_add real_registers_by_color color ~default:(fun () ->
               [ Set.choose_exn available_registers ])
           in
           Set.remove available_registers reg_to_use, reg_to_use
         | None ->
           (* If the vertex was not part of the interference graph and hence didn't get a
              color, we can pick any register. *)
           available_registers, Set.choose_exn all_available_registers)
    in
    (* TODO: Do an extra pass to remove useless [Mov]s. Can probably do other similar
       pinhole optimizations as well. *)
    let (_ : Asm_program.Register.Set.t), basic_blocks =
      List.fold_map
        basic_blocks
        ~init:available_registers
        ~f:(fun available_registers { label; code; terminal } ->
        let available_registers, code =
          List.fold_map
            code
            ~init:available_registers
            ~f:(fun available_registers instr ->
            Instr.Nonterminal.fold_map_args
              instr
              ~init:available_registers
              ~f:(fun available_registers value ->
              Value.fold_map_registers
                value
                ~init:available_registers
                ~f:allocate_register))
        in
        available_registers, ({ label; code; terminal } : _ Basic_block.t))
    in
    basic_blocks
  ;;

  let allocate (basic_blocks : Register.t Basic_block.t list) =
    eprint_s [%here] [%lazy_message (basic_blocks : Register.t Basic_block.t list)];
    let cfg = create_cfg ~basic_blocks in
    eprint_s
      [%here]
      [%lazy_message
        ""
          ~cfg:
            (Cfg.fold_edges (fun label1 label2 acc -> (label1, label2) :: acc) cfg []
              : (Cfg_node.t * Cfg_node.t) list)];
    let interference_graph = create_interference_graph ~cfg ~basic_blocks in
    (* FIXME: Check in advance if there is any vertex with degree > N - 1, and pick things
       to spill before trying to color. *)
    eprint_s
      [%here]
      [%lazy_message
        ""
          ~interference_graph:
            (Interference_graph.fold_edges
               (fun reg1 reg2 acc -> (reg1, reg2) :: acc)
               interference_graph
               []
              : (Register.t * Register.t) list)];
    let basic_blocks = coalesce_registers ~basic_blocks ~interference_graph in
    color_and_allocate ~basic_blocks ~interference_graph
  ;;
end

module Extern = struct
  type t =
    | C_function of Label_name.t
    | Umber_function
end

module Function_builder : sig
  type t

  val create : Label_name.t -> t
  val add_code : t -> Register.t Instr.Nonterminal.t -> unit
  val add_terminal : t -> Instr.Terminal.t -> unit

  (* TODO: Consider if this API makes any sense *)
  val move_values_for_call
    :  t
    -> call_conv:Call_conv.t
    -> args:Register.t Value.t Nonempty.t
    -> unit

  val add_local : t -> Mir_name.t -> Register.t Value.t -> unit
  val find_local : t -> Mir_name.t -> Register.t Value.t option
  val current_label : t -> Label_name.t
  val position_at_label : t -> Label_name.t -> unit
  val create_label : t -> ('a, unit, string, Label_name.t) format4 -> 'a
  val pick_register : t -> Register.t Value.t
  val pick_register' : t -> Virtual_register.t
  val name : t -> Label_name.t
  val basic_blocks : t -> Asm_program.Register.t Basic_block.t list
end = struct
  module Block_builder = struct
    type t =
      { code : Register.t Instr.Nonterminal.t Queue.t
      ; terminal : Instr.Terminal.t Set_once.t
      }
    [@@deriving sexp_of]

    let create () = { code = Queue.create (); terminal = Set_once.create () }
  end

  module Label_id = Unique_counter ()

  type t =
    { fun_name : Label_name.t
    ; label_counter : Label_id.Counter.t
    ; register_counter : Virtual_register.Counter.t
    ; locals : Register.t Value.t Mir_name.Table.t
    ; basic_blocks : Block_builder.t Label_name.Hash_queue.t
    ; mutable current_label : Label_name.t
    }

  let name t = t.fun_name
  let current_label t = t.current_label
  let position_at_label t label_name = t.current_label <- label_name
  let pick_register' t = Virtual_register.Counter.next t.register_counter
  let pick_register t : Register.t Value.t = Register (Virtual (pick_register' t))

  let get_bb t =
    match Hash_queue.lookup t.basic_blocks t.current_label with
    | Some bb -> bb
    | None ->
      let bb = Block_builder.create () in
      Hash_queue.enqueue_back_exn t.basic_blocks t.current_label bb;
      bb
  ;;

  let add_code t (instr : _ Instr.Nonterminal.t) =
    let bb = get_bb t in
    (* Catch memory-to-memory operations and use a temporary register. *)
    match instr with
    | Mov { src = Memory (size, _) as src; dst = Memory (size', _) as dst } ->
      assert_or_compiler_bug (Size.equal size size') ~here:[%here];
      let tmp = pick_register t in
      Queue.enqueue bb.code (Mov { src; dst = tmp });
      Queue.enqueue bb.code (Mov { src = tmp; dst })
    | _ -> Queue.enqueue bb.code instr
  ;;

  let add_terminal t terminal =
    let bb = get_bb t in
    match Set_once.set bb.terminal [%here] terminal with
    | Ok () -> ()
    | Error error ->
      compiler_bug
        [%message
          "Tried to add multiple terminals to a block"
            ~new_terminal:(terminal : Instr.Terminal.t)
            ~existing_terminal:(bb.terminal : Instr.Terminal.t Set_once.t)
            ~current_label:(t.current_label : Label_name.t)
            (bb : Block_builder.t)
            (error : Error.t)]
  ;;

  let create fun_name =
    (* FIXME: callee saved registers must be marked Used *)
    let t =
      { fun_name
      ; label_counter = Label_id.Counter.create ()
      ; register_counter = Virtual_register.Counter.create ()
      ; locals = Mir_name.Table.create ()
      ; basic_blocks = Label_name.Hash_queue.create ()
      ; current_label = fun_name
      }
    in
    Hash_queue.enqueue_back_exn t.basic_blocks fun_name (Block_builder.create ());
    t
  ;;

  let add_local t name value = Hashtbl.add_exn t.locals ~key:name ~data:value
  let find_local t name = Hashtbl.find t.locals name

  let move_values_for_call t ~call_conv ~(args : Register.t Value.t Nonempty.t) =
    (* FIXME: Sort out the arguments - move them to where they need to be. Do a diff with
       the current and target register states. Be careful about ordering - we need to be
       careful not to clobber any of the args, so we can't just be like "for each arg,
       move it to where it should go". We might need to use extra registers to do swaps.
       
       e.g. how to handle this:

       Current: 1: a, 2: b, 3: c
       Target:  1: c, 2: a, 3: b

       We need:
       1 -> 4 (1: a, 2: b, 3: c, 4: a)
       3 -> 1 (1: c, 2: b, 3: c, 4: a)
       2 -> 3 (1: c, 2: b, 3: b, 4: a)
       4 -> 2 (1: c, 2: a, 3: b, 4: a)

       Also need to handle spilling onto the stack (and having values on the stack)
       if we run out of registers
       - we might need a bit more extra info than Value.t has e.g. what stack slock a
       variable is at 
       
       A different approach would be to *force* each of the subsequent generation functions
       to put their result in target register. Then other calls could either avoid using
       that register or spill it, then restore it (it'd be most efficient to restore only
       at the end, I think, to reduce traffic, but then we'd have to keep track of the
       fact that it's been spilled...). Maybe keeping a suggestion but being able to deal
       with whatever ended up happening is easier after all...

       Let's try something naive first.

       Actually, maybe we want the codegen to generate virtual registers first, then have
       a separate register allocation phase.

       The function counts as a value to move as well!

       TODO: Move this out out Function_builder, it doesn't need to be here.
    *)
    let args = Nonempty.to_array args in
    let arg_registers =
      Call_conv.arg_registers call_conv
      |> Nonempty.to_list
      |> Fn.flip List.take (Array.length args)
      |> List.to_array
    in
    if Array.length args > Array.length arg_registers
    then failwith "TODO: ran out of registers for args, use stack";
    (* Use a temporary to avoid cases where we'd overwrite something already in one of
       the target registers. *)
    let temporaries_to_move =
      Array.map args ~f:(fun arg ->
        let tmp = pick_register t in
        add_code t (Mov { src = arg; dst = tmp });
        tmp)
    in
    Array.iter2_exn temporaries_to_move arg_registers ~f:(fun tmp arg_register ->
      add_code t (Mov { src = tmp; dst = Register (Real arg_register) }))
  ;;

  let create_label t format =
    let id = Label_id.Counter.next t.label_counter in
    ksprintf (fun s -> Label_name.of_string [%string ".%{s}#%{id#Label_id}"]) format
  ;;

  let basic_blocks t =
    Hash_queue.to_alist t.basic_blocks
    |> List.map ~f:(fun (label, { code; terminal }) : _ Basic_block.t ->
         { label
         ; code = Queue.to_list code
         ; terminal = Set_once.get_exn terminal [%here]
         })
    |> Reg_alloc.allocate
  ;;
end

(* TODO: This only handles no-environment wrapper closures. MIR should handle this instead. *)
module Closure = struct
  type t =
    { closure_name : Label_name.t
    ; fun_name : Label_name.t
    }

  let of_fun_name fun_name =
    { fun_name
    ; closure_name = Label_name.of_string [%string "%{fun_name#Label_name}#closure"]
    }
  ;;
end

type t =
  { (* FIXME: name_table is unused *)
    name_table : Mir_name.Name_table.t
  ; bss_globals : Label_name.t Queue.t
  ; externs : Extern.t Mir_name.Table.t
  ; literals : Label_name.t Literal.Table.t
  ; closures : Closure.t Mir_name.Table.t
  ; functions : Function_builder.t Mir_name.Table.t
  ; main_function : Function_builder.t
  }

let constant_block ~tag ~len ~data_kind data : (Size.t * Data_decl.Payload.t) list =
  [ I16, Literal (Int (Cnstr_tag.to_int tag))
  ; I16, Literal (Int len)
  ; I32, Literal (Int 0) (* padding *)
  ; data_kind, data
  ]
;;

let constant_block_for_literal (literal : Literal.t) =
  match literal with
  | Int i -> constant_block ~tag:Cnstr_tag.int ~len:1 ~data_kind:I64 (Literal (Int i))
  | Float x ->
    constant_block ~tag:Cnstr_tag.float ~len:1 ~data_kind:I64 (Literal (Float x))
  | Char c ->
    (* TODO: We could just store Chars as immediate values, they are guaranteed to fit *)
    constant_block
      ~tag:Cnstr_tag.char
      ~len:1
      ~data_kind:I8
      (Literal (String (Ustring.of_uchar c)))
  | String s ->
    let s = Ustring.to_string s in
    let n_chars = String.length s in
    let n_words = (n_chars / 8) + 1 in
    let padded_s =
      String.init (n_words * 8) ~f:(fun char_index ->
        if char_index < n_chars
        then s.[char_index]
        else if char_index = (n_words * 8) - 1
        then (* Last byte *) Char.of_int_exn (7 - (n_chars % 8))
        else (* Padding null prefix of last word *) Char.of_int_exn 0)
    in
    constant_block
      ~tag:Cnstr_tag.string
      ~len:n_words
      ~data_kind:I8
      (Literal (String (Ustring.of_string_exn padded_s)))
;;

let constant_block_for_closure ~fun_name =
  constant_block ~tag:Cnstr_tag.closure ~len:1 ~data_kind:I64 (Label fun_name)
;;

let to_program
  { bss_globals; externs; literals; closures; functions; main_function; name_table = _ }
  : Asm_program.t
  =
  let uninitialized_globals = Queue.to_list bss_globals in
  let literals = Hashtbl.to_alist literals in
  let closures = Hashtbl.data closures in
  let functions = Hashtbl.data functions in
  { globals =
      { name = Function_builder.name main_function; strength = `Strong }
      :: (List.map uninitialized_globals ~f:(fun name : Global_decl.t ->
            { name; strength = `Strong })
          @ List.map functions ~f:(fun fun_builder : Global_decl.t ->
              { name = Function_builder.name fun_builder; strength = `Strong })
          @ List.map literals ~f:(fun ((_ : Literal.t), name) : Global_decl.t ->
              { name; strength = `Weak }))
  ; externs =
      Hashtbl.to_alist externs
      |> List.map ~f:(fun (mir_name, extern) ->
           match extern with
           | C_function name -> name
           | Umber_function -> Label_name.of_mir_name mir_name)
  ; text_section =
      Function_builder.basic_blocks main_function
      @ List.concat_map functions ~f:Function_builder.basic_blocks
  ; rodata_section =
      List.map literals ~f:(fun (literal, label) : Data_decl.t ->
        { label; payloads = constant_block_for_literal literal })
      @ List.map closures ~f:(fun { fun_name; closure_name } : Data_decl.t ->
          { label = closure_name; payloads = constant_block_for_closure ~fun_name })
  ; bss_section =
      List.map uninitialized_globals ~f:(fun name : Bss_decl.t ->
        { label = name; kind = `Words; size = 1 })
  }
;;

let create ~main_function_name =
  { name_table = Mir_name.Name_table.create ()
  ; bss_globals = Queue.create ()
  ; externs = Mir_name.Table.create ()
  ; literals = Literal.Table.create ()
  ; closures = Mir_name.Table.create ()
  ; functions = Mir_name.Table.create ()
  ; main_function = Function_builder.create main_function_name
  }
;;

let int_constant_tag tag : _ Value.t =
  (* Put the int63 into an int64 and make the bottom bit 1. *)
  Constant (Int (Int.shift_left (Cnstr_tag.to_int tag) 1 + 1))
;;

let declare_extern_c_function ?mir_name t label_name =
  let mir_name =
    match mir_name with
    | Some mir_name -> mir_name
    | None -> Label_name.to_mir_name label_name
  in
  ignore
    (Hashtbl.add t.externs ~key:mir_name ~data:(C_function label_name)
      : [ `Ok | `Duplicate ])
;;

(* FIXME: Handle file-local functions *)
(* FIXME: Handle values and function lookups differently (may need a closure)
   Hmm, except closures are already explicit in MIR, right? Do they actually need
   special handling? Oh, they do because MIR treats function pointers as reasonable values
*)
(* TODO: Amend MIR to treat function pointers and closures differently. *)
let lookup_name_for_value t name ~fun_builder : _ Value.t =
  match Function_builder.find_local fun_builder name with
  | Some value -> value
  | None ->
    (* FIXME: For everything except locals, we need to create a closure and possibly a
       wrapper function. We need to treat it as if wrapped in a [Make_block] 
        
       For now, treat externs and functions the same. This will break when implementing a
       proper calling convention besides copying C's.
    *)
    let closure =
      Hashtbl.find_or_add t.closures name ~default:(fun () ->
        let fun_name =
          (* FIXME: Code duplication *)
          match Hashtbl.find t.functions name with
          | Some fun_builder -> Function_builder.name fun_builder
          | None ->
            (match Hashtbl.find t.externs name with
             | Some (C_function name) -> name
             | Some Umber_function -> Label_name.of_mir_name name
             | None -> Label_name.of_mir_name name)
        in
        Closure.of_fun_name fun_name)
    in
    Global (closure.closure_name, Other)
;;

let mem_or_temporary fun_builder value =
  match Value.mem_of_value value with
  | Some mem -> mem
  | None ->
    let tmp : Register.t = Virtual (Function_builder.pick_register' fun_builder) in
    Function_builder.add_code fun_builder (Mov { src = value; dst = Register tmp });
    Register tmp
;;

let mem_offset_value fun_builder value size offset =
  Value.mem_offset (mem_or_temporary fun_builder value) size offset
;;

let lookup_name_for_fun_call t name ~fun_builder : _ Value.t * Call_conv.t =
  (* FIXME: Handle calling closures *)
  match Function_builder.find_local fun_builder name with
  | Some closure ->
    (* We are calling a closure. Load the function pointer from the first field. *)
    mem_offset_value fun_builder closure I64 1, Umber
  | None ->
    (match Hashtbl.find t.functions name with
     | Some fun_builder -> Global (Function_builder.name fun_builder, Other), Umber
     | None ->
       (match Hashtbl.find t.externs name with
        | Some (C_function name) -> Global (name, Extern_proc), C
        | Some Umber_function -> Global (Label_name.of_mir_name name, Extern_proc), Umber
        | None -> Global (Label_name.of_mir_name name, Other), Umber))
;;

(* FIXME: Have to save and restore any caller-save registers. *)
let codegen_fun_call t fun_name args ~fun_builder =
  let fun_, call_conv = lookup_name_for_fun_call t fun_name ~fun_builder in
  Function_builder.move_values_for_call fun_builder ~call_conv ~args;
  Function_builder.add_code
    fun_builder
    (Call { fun_; call_conv; arity = Nonempty.length args });
  let output_register = Function_builder.pick_register' fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = Register (Real (Call_conv.return_value_register call_conv))
       ; dst = Register (Virtual output_register)
       });
  output_register
;;

let box t ~tag ~fields ~fun_builder =
  let block_field_num = Nonempty.length fields in
  let heap_pointer_reg =
    let allocation_size = 8 * (block_field_num + 1) in
    let extern_name = Label_name.of_string "umber_gc_alloc" in
    let mir_name = Label_name.to_mir_name extern_name in
    declare_extern_c_function t ~mir_name extern_name;
    codegen_fun_call t mir_name [ Constant (Int allocation_size) ] ~fun_builder
  in
  let heap_pointer : Register.t Value.memory_expr = Register (Virtual heap_pointer_reg) in
  Function_builder.add_code
    fun_builder
    (Mov { dst = Memory (I16, heap_pointer); src = Constant (Int (Cnstr_tag.to_int tag)) });
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Value.mem_offset heap_pointer I16 1; src = Constant (Int block_field_num) });
  Nonempty.iteri fields ~f:(fun i field_value ->
    Function_builder.add_code
      fun_builder
      (Mov { dst = Value.mem_offset heap_pointer I64 (i + 1); src = field_value }));
  heap_pointer_reg
;;

(* TODO: Depending on how the constant value is used, we could just return a [Constant]
   directly, and we might not even need to generate the literal in the first place. This
   might be easier to handle in a later inlining pass. *)
let codegen_literal t (literal : Literal.t) : _ Value.t =
  let name =
    Hashtbl.find_or_add t.literals literal ~default:(fun () ->
      let name =
        match literal with
        | Int i -> sprintf "int.%d" i
        | Float x -> sprintf "float.%f" x
        | String s ->
          (* TODO: I think this is a 31-bit hash which is pretty sus considering the
             posibility of collisions. *)
          sprintf "string.%d" (Ustring.hash s)
        | Char c -> sprintf "char.%d" (Uchar.to_int c)
      in
      Label_name.of_string name)
  in
  Global (name, Other)
;;

let get_block_field fun_builder value index =
  mem_offset_value fun_builder value I64 (Mir.Block_index.to_int index + 1)
;;

let check_value_is_block fun_builder value =
  (* Check if this value is a pointer to a block. Pointers always have bottom bit 0.
     This is done by checking (in C syntax) `(value & 1) == 0`. *)
  Function_builder.add_code fun_builder (Test (value, Constant (Int 1)))
;;

(* FIXME: Simplify with constraints? *)
(** Insert code such that all the values end up in the same place. *)
let merge_branches
  fun_builder
  (label_a, (value_a : _ Value.t))
  (label_b, (value_b : _ Value.t))
  ~merge_label
  =
  let value =
    match value_a, value_b with
    | Register a, Register b ->
      (* FIXME: Unclear if a might be used for something else useful in the b case? *)
      if not (Register.equal a b)
      then (
        Function_builder.position_at_label fun_builder label_b;
        Function_builder.add_code fun_builder (Mov { dst = value_a; src = value_b }));
      value_a
    | Register _, (Memory _ | Global _ | Constant _) ->
      Function_builder.position_at_label fun_builder label_b;
      Function_builder.add_code fun_builder (Mov { dst = value_a; src = value_b });
      value_a
    | (Memory _ | Global _ | Constant _), Register _ ->
      Function_builder.position_at_label fun_builder label_a;
      Function_builder.add_code fun_builder (Mov { dst = value_b; src = value_a });
      value_b
    | (Memory _ | Global _ | Constant _), (Memory _ | Global _ | Constant _) ->
      let dst = Function_builder.pick_register fun_builder in
      Function_builder.position_at_label fun_builder label_a;
      Function_builder.add_code fun_builder (Mov { dst; src = value_a });
      Function_builder.position_at_label fun_builder label_b;
      Function_builder.add_code fun_builder (Mov { dst; src = value_b });
      dst
  in
  (* Finally, finish at a single "merge" instr group. It's important we always finish at
     the same place for composability - code generated after this can just continue here,
     rather than having to deal with multiple branch points. *)
  (* TODO: This often generates "useless" jumps to either the next line or a block that
     just contains "Ret". We could try to avoid doing this or add an extra pass to optimize
     these. Unclear if it's worth much. *)
  Function_builder.position_at_label fun_builder label_a;
  Function_builder.add_terminal fun_builder (Jump merge_label);
  Function_builder.position_at_label fun_builder label_b;
  Function_builder.add_terminal fun_builder (Jump merge_label);
  Function_builder.position_at_label fun_builder merge_label;
  value
;;

let rec codegen_expr t (expr : Mir.Expr.t) ~(fun_builder : Function_builder.t) =
  match expr with
  | Primitive literal -> codegen_literal t literal
  | Name name -> lookup_name_for_value t name ~fun_builder
  | Let (name, expr, body) ->
    let expr = codegen_expr t expr ~fun_builder in
    Function_builder.add_local fun_builder name expr;
    codegen_expr t body ~fun_builder
  | Fun_call (fun_name, args) ->
    (* TODO: Maybe we could do something cleverer like suggest a good place to put the
       expression value (to reduce the need for moves) *)
    let args = Nonempty.map args ~f:(codegen_expr t ~fun_builder) in
    Register (Virtual (codegen_fun_call t fun_name args ~fun_builder))
  | Make_block { tag; fields } ->
    (match Nonempty.of_list fields with
     | None -> int_constant_tag tag
     | Some fields ->
       let fields = Nonempty.map fields ~f:(codegen_expr t ~fun_builder) in
       Register (Virtual (box t ~tag ~fields ~fun_builder)))
  | Get_block_field (field_index, block) ->
    let block = codegen_expr t block ~fun_builder in
    get_block_field fun_builder block field_index
  | Cond_assign { vars; conds; body; if_none_matched } ->
    (* FIXME: Need a way to mint new label names. Guess a name table isn't awful
       Might be easier to just use a counter or something though
       We can prefix with . to get namespacing, kinda relying on nasm a lot with that tho
    *)
    let n_conds = Nonempty.length conds in
    let cond_labels =
      Array.init n_conds ~f:(fun i ->
        Function_builder.create_label fun_builder "cond_assign.cond%d" i)
    in
    let vars_labels =
      Array.init n_conds ~f:(fun i ->
        Function_builder.create_label fun_builder "cond_assign.vars%d" i)
    in
    let vars =
      List.map vars ~f:(fun var ->
        let tmp = Function_builder.pick_register fun_builder in
        Function_builder.add_local fun_builder var tmp;
        tmp)
    in
    let body_label = Function_builder.create_label fun_builder "cond_assign.body" in
    let if_none_matched_label =
      Function_builder.create_label fun_builder "cond_assign.if_none_matched"
    in
    let assign_vars_and_jump_to_body var_exprs =
      List.iter2_exn vars var_exprs ~f:(fun var var_expr ->
        let var_value = codegen_expr t var_expr ~fun_builder in
        (* Move to a register which is consistent in all branches. This is important for
           the correctness of [add_local], which will be used for all future branches. *)
        Function_builder.add_code fun_builder (Mov { src = var_value; dst = var }));
      Function_builder.add_terminal fun_builder (Jump body_label)
    in
    Nonempty.iteri conds ~f:(fun i (cond, var_exprs) ->
      if i <> 0 then Function_builder.position_at_label fun_builder cond_labels.(i);
      codegen_cond t cond ~fun_builder;
      let next_label =
        if i < n_conds - 1 then cond_labels.(i + 1) else if_none_matched_label
      in
      Function_builder.add_terminal
        fun_builder
        (Jump_if { cond = `Nonzero; then_ = next_label; else_ = vars_labels.(i) });
      Function_builder.position_at_label fun_builder vars_labels.(i);
      (* If we didn't jump, the condition succeeded, so set the variables. *)
      assign_vars_and_jump_to_body var_exprs);
    (match if_none_matched with
     | Use_bindings var_exprs ->
       Function_builder.position_at_label fun_builder if_none_matched_label;
       assign_vars_and_jump_to_body var_exprs;
       Function_builder.position_at_label fun_builder body_label;
       let body_value = codegen_expr t body ~fun_builder in
       body_value
     | Otherwise otherwise_expr ->
       Function_builder.position_at_label fun_builder body_label;
       let body_value = codegen_expr t body ~fun_builder in
       let body_end_label = Function_builder.current_label fun_builder in
       Function_builder.position_at_label fun_builder if_none_matched_label;
       let otherwise_value = codegen_expr t otherwise_expr ~fun_builder in
       let otherwise_end_label = Function_builder.current_label fun_builder in
       merge_branches
         fun_builder
         (body_end_label, body_value)
         (otherwise_end_label, otherwise_value)
         ~merge_label:(Function_builder.create_label fun_builder "cond_assign.merge"))

and codegen_cond t cond ~fun_builder =
  let cmp value value' = Function_builder.add_code fun_builder (Cmp (value, value')) in
  match cond with
  | Equals (expr, literal) ->
    let expr_value = codegen_expr t expr ~fun_builder in
    let literal_value = codegen_literal t literal in
    (* FIXME: Handle conditionally loading values from the stack into registers *)
    let load_expr ~expr_value =
      get_block_field fun_builder expr_value (Mir.Block_index.of_int 0)
    in
    let load_literal ~literal_value =
      get_block_field fun_builder literal_value (Mir.Block_index.of_int 0)
    in
    (match literal with
     | Int _ | Char _ -> cmp (load_expr ~expr_value) (load_literal ~literal_value)
     | Float _ -> failwith "TODO: float equality in patterns"
     | String _ -> failwith "TODO: string equality in patterns")
  | Constant_tag_equals (expr, tag) ->
    cmp (codegen_expr t expr ~fun_builder) (int_constant_tag tag)
  | Non_constant_tag_equals (expr, tag) ->
    let value = codegen_expr t expr ~fun_builder in
    codegen_and
      ~fun_builder
      ~codegen_cond1:(fun () -> check_value_is_block fun_builder value)
      ~cond2_label:
        (Function_builder.create_label fun_builder "non_constant_tag_equals.is_block")
      ~codegen_cond2:(fun () ->
        Function_builder.add_code
          fun_builder
          (Cmp
             ( mem_offset_value fun_builder value I16 0
             , Constant (Int (Cnstr_tag.to_int tag)) )))
      ~end_label:(Function_builder.create_label fun_builder "non_constant_tag_equals.end")
  | And (cond1, cond2) ->
    codegen_and
      ~fun_builder
      ~codegen_cond1:(fun () -> codegen_cond t ~fun_builder cond1)
      ~codegen_cond2:(fun () -> codegen_cond t ~fun_builder cond2)
      ~cond2_label:(Function_builder.create_label fun_builder "and.cond2")
      ~end_label:(Function_builder.create_label fun_builder "and.end")

and codegen_and ~fun_builder ~codegen_cond1 ~codegen_cond2 ~cond2_label ~end_label =
  (* We use the reasoning that `x and y` is the same as `if x then y else x`. *)
  codegen_cond1 ();
  Function_builder.add_terminal
    fun_builder
    (Jump_if { cond = `Zero; then_ = cond2_label; else_ = end_label });
  Function_builder.position_at_label fun_builder cond2_label;
  codegen_cond2 ();
  Function_builder.add_terminal fun_builder (Jump end_label);
  Function_builder.position_at_label fun_builder end_label
;;

let set_global t global expr ~fun_builder =
  Queue.enqueue t.bss_globals global;
  let expr_location = codegen_expr t expr ~fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov { dst = Memory (I64, Global (global, Other)); src = expr_location })
;;

let define_function t ~fun_name ~args ~body =
  (* TODO: Need a function prelude for e.g. the frame pointer *)
  let call_conv : Call_conv.t = Umber in
  let fun_builder = Function_builder.create (Label_name.of_mir_name fun_name) in
  Hashtbl.add_exn t.functions ~key:fun_name ~data:fun_builder;
  let result =
    Nonempty.iter2 args (Call_conv.arg_registers call_conv) ~f:(fun arg_name reg ->
      let virtual_reg = Function_builder.pick_register fun_builder in
      Function_builder.add_code
        fun_builder
        (Mov { src = Register (Real reg); dst = virtual_reg });
      Function_builder.add_local fun_builder arg_name virtual_reg)
  in
  (match result with
   | Same_length | Right_trailing _ -> ()
   | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack");
  let return_value = codegen_expr t body ~fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = return_value
       ; dst = Register (Real (Call_conv.return_value_register call_conv))
       });
  Function_builder.add_terminal fun_builder Ret
;;

let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    (* TODO: We could recognize constant expressions in globals (and locals too) and not
       have to do runtime initialization, instead just keeping them in rodata. *)
    set_global t (Label_name.of_mir_name name) expr ~fun_builder:t.main_function
  | Fun_def { fun_name; args; body } ->
    (* FIXME: Need a preprocessing step like the llvm codegen to handle mutually
       recursive functions. *)
    define_function t ~fun_name ~args ~body
  | Fun_decl { name; arity = _ } ->
    Hashtbl.add_exn t.externs ~key:name ~data:Umber_function
  | Extern_decl { name; extern_name; arity = _ } ->
    (* TODO: Do we even need arity anymore? Also applies to [Fun_decl] above. *)
    declare_extern_c_function t ~mir_name:name (Label_name.of_extern_name extern_name)
;;

let main_function_name ~(module_path : Module_path.Absolute.t) =
  Label_name.of_string [%string "umber_main#%{module_path#Module_path}"]
;;

let convert_mir ~module_path mir =
  let t = create ~main_function_name:(main_function_name ~module_path) in
  List.iter mir ~f:(codegen_stmt t);
  Function_builder.add_terminal t.main_function Ret;
  to_program t
;;

(* TODO: Move this utility somewhere better. *)
let with_tempfile ~prefix ~suffix ~f =
  let tempfile = Filename_unix.temp_file prefix suffix in
  protect ~f:(fun () -> f tempfile) ~finally:(fun () -> Sys_unix.remove tempfile)
;;

let compile_to_object_file program ~output_file =
  with_tempfile ~prefix:"umber" ~suffix:".asm" ~f:(fun tempfile ->
    Out_channel.with_file tempfile ~f:(fun out ->
      Asm_program.pp (Format.formatter_of_out_channel out) program);
    Asm_helpers.compile_to_object_file ~input_file:tempfile ~output_file)
;;

let compile_entry_module ~module_paths ~entry_file =
  let t = create ~main_function_name:(Label_name.of_string "main") in
  let umber_gc_init = Label_name.of_string "umber_gc_init" in
  declare_extern_c_function t umber_gc_init;
  Function_builder.add_code
    t.main_function
    (Call { fun_ = Global (umber_gc_init, Extern_proc); call_conv = C; arity = 0 });
  List.iter module_paths ~f:(fun module_path ->
    let fun_name = main_function_name ~module_path in
    Hashtbl.add_exn t.externs ~key:(Label_name.to_mir_name fun_name) ~data:Umber_function;
    Function_builder.add_code
      t.main_function
      (Call { fun_ = Global (fun_name, Extern_proc); call_conv = Umber; arity = 0 }));
  Function_builder.add_terminal t.main_function Ret;
  compile_to_object_file (to_program t) ~output_file:entry_file
;;

let%expect_test "hello world" =
  let hello_world : Asm_program.t =
    { globals = [ { name = Label_name.of_string "main"; strength = `Strong } ]
    ; externs = [ Label_name.of_string "puts" ]
    ; text_section =
        [ { label = Label_name.of_string "main"
          ; code =
              [ Lea
                  { dst = Register Rdi
                  ; src = Memory (I64, Global (Label_name.of_string "message", Other))
                  }
              ; Call
                  { fun_ = Global (Label_name.of_string "puts", Extern_proc)
                  ; call_conv = C
                  ; arity = 1
                  }
              ]
          ; terminal = Ret
          }
        ]
    ; rodata_section =
        [ { label = Label_name.of_string "message"
          ; payloads =
              [ I8, Literal (String (Ustring.of_string_exn "Hello, world!"))
              ; I8, Literal (Int 10)
              ]
          }
        ]
    ; bss_section = []
    }
  in
  Asm_program.pp Format.std_formatter hello_world;
  [%expect
    {|
               default   rel
               global    main
               extern    puts

               section   .text

    main:
               lea       rdi, qword [message]
               call      puts wrt ..plt
               ret

               section   .rodata
               sectalign 8
    message:
               db        `Hello, world!`
               db        10 |}]
;;

let%expect_test "hello world, from MIR" =
  let name_table = Mir_name.Name_table.create () in
  let print_name =
    Mir_name.create_exportable_name
      (Value_name.Absolute.of_relative_unchecked
         (Value_name.Relative.of_string "Std.Prelude.print"))
  in
  let hello_world : Mir.t =
    [ Extern_decl
        { name = print_name
        ; extern_name = Extern_name.of_string_exn "umber_print_endline"
        ; arity = 1
        }
    ; Value_def
        ( Mir_name.create_value_name
            name_table
            (Value_name.Absolute.of_string "HelloWorld.#binding")
        , Fun_call
            (print_name, [ Primitive (String (Ustring.of_string_exn "Hello, world!")) ])
        )
    ]
  in
  let module_path =
    Module_path.Absolute.of_relative_unchecked
      (Module_path.Relative.of_ustrings_exn [ Ustring.of_string_exn "HelloWorld" ])
  in
  Asm_program.pp Format.std_formatter (convert_mir ~module_path hello_world);
  [%expect
    {|
               default   rel
               global    umber_main#HelloWorld
               global    HelloWorld.#binding.1
               global    string.210886959:weak
               extern    umber_print_endline

               section   .text

    umber_main#HelloWorld:
               mov       rdi, string.210886959
               call      umber_print_endline wrt ..plt
               mov       qword [HelloWorld.#binding.1], rax
               ret

               section   .rodata
               sectalign 8
    string.210886959:
               dw        32772
               dw        2
               dd        0
               db        `Hello, world!\x0\x0\x2`

               section   .bss
               sectalign 8
    HelloWorld.#binding.1:
               resq      1 |}]
;;
