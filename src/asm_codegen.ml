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
  module Value = Value
  module Instr = Instr
  module Basic_block = Basic_block
  module Size = Size
  module Data_decl = Data_decl
  module Bss_decl = Bss_decl
  module Global_decl = Global_decl
end

module Call_conv = struct
  type t =
    | C
    | Umber

  (* TODO: Decide on an umber calling convention. For now, let's just copy C's calling
     convention since we have to implement that anyway. We can probably switch to
     basically copying OCaml's after that. *)

  (* TODO: Handle further arguments with the stack *)
  let arg_registers t : Asm_program.Register.t Nonempty.t =
    match t with
    | C | Umber -> [ Rdi; Rsi; Rdx; Rcx; R8; R9 ]
  ;;

  let return_value_register : t -> Asm_program.Register.t = function
    | C | Umber -> Rax
  ;;

  (* FIXME: figure out what to do with this *)
  (* let reserved_registers : t -> Asm_program.Register.t list = function
    | C | Umber -> [ (* Stack pointer *) Rsp; (* Frame pointer *) Rbp ]
  ;; *)
end

module Virtual_register : sig
  type t [@@deriving compare, equal, sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

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

(* FIXME: Consider using this representation where you're allowed to use real registers
   directly rather than input/ouptut constraints. Writing a Mov between real and virtual
   registers feels like it expresses the intent more naturally, and we have to understand
   Mov anyway to understand uses and assignments. *)
(* module Register = struct
  type t =
    | Real of Asm_program.Register.t
    | Virtual of Virtual_register.t
end *)

module Reg_alloc : sig
  val allocate
    :  Virtual_register.t Basic_block.t list
    -> register_input_constraints:Asm_program.Register.t Virtual_register.Table.t
    -> register_output_constraints:Asm_program.Register.t Virtual_register.Table.t
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

  module Cfg = Graph.Imperative.Digraph.Concrete (Cfg_node)

  let create_cfg ~(basic_blocks : _ Basic_block.t list) =
    let cfg = Cfg.create () in
    List.iter basic_blocks ~f:(fun { label; code = _; terminal } ->
      Cfg.add_vertex cfg (Label label);
      Cfg.add_edge
        cfg
        (Label label)
        (match terminal with
         | Jmp label | Jz label | Jnz label -> Label label
         | Ret -> Ret));
    cfg
  ;;

  let fold_cfg_backwards ~cfg node ~init ~f =
    let rec loop ~cfg node ~init ~f =
      let init = f init node in
      List.iter (Cfg.pred cfg node) ~f:(loop ~cfg ~init ~f)
    in
    loop ~cfg node ~init ~f
  ;;

  module Interference_graph = struct
    module G = Graph.Imperative.Graph.Concrete (Virtual_register)
    include G
    include Graph.Coloring.Make (G)
  end

  module Arg_kind = struct
    type t =
      | Use
      | Assignment
  end

  (* FIXME: Maybe we need to insert an implicit use of return registers? *)
  (* FIXME: "int" can't really properly represent paths through the program - what about
     jumps? *)
  (* module Lifetime : sig
    (** A lifetime is a region of time where a value in a register is live. It corresponds
        to number of paths of execution through a program, beginning with an initial
        assignment and ending with a final use (with intermediate assignments and uses in
        between). *)
    type t

    val create : unit -> t
    val add : t -> Arg_kind.t -> int -> unit
    val is_overlapping : t -> t -> bool
  end = struct
    type t =
      { assignments : Int.Hash_set.t
      ; uses : Int.Hash_set.t
      }

    let create () =
      { assignments = Int.Hash_set.create (); uses = Int.Hash_set.create () }
    ;;

    let add t kind i =
      match kind with
      | Use -> Hash_set.add t.uses i
      | Assignment -> Hash_set.add t.assignments i
    ;;

    let is_overlapping t t' = not (Hash_se !t !t')
  end *)

  (* TODO: Move to Instr module? *)
  let instr_args : _ Instr.Nonterminal.t -> (Arg_kind.t * _ Value.t) list = function
    | Mov { dst; src } | Lea { dst; src } -> [ Use, src; Assignment, dst ]
    | And { dst; src } ->
      (* NOTE: It's important that [Use, dst] comes before [Assignment, dst] for the logic
         calculating lifetimes to work. This reflectts the reality that the register is
         used *before* being assigned to. *)
      [ Use, src; Use, dst; Assignment, dst ]
    | Call a | Setz a -> [ Use, a ]
    | Cmp (a, b) | Test (a, b) -> [ Use, a; Use, b ]
  ;;

  (* FIXME: You have to think about register lifetimes across jumps! Just an index doesn't
     work? Maybe I need some kind of SSA form...? Atm we never jump backwards so it might
     be ok...?
     
     Problems:
     - You need to traverse the program while understanding control flow (jumps) and
       variable lifetimes (assignments, uses). Doing that over asm requires understanding
       every single instruction. It is likely easier to do with a simpler IR.
     - I think we kinda need a proper control flow graph to understand lifetimes? Whatever
       ad-hoc approach I use will likely be a bad re-implementation of that

     Options:
     - Make a decent effort of doing this now on asm
     - Do something really trivial like always use the stack and store/load on every use,
       except for C functions which need arguments in registers.
  *)
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
    fold_cfg_backwards
      ~cfg
      Ret
      ~init:Virtual_register.Set.empty
      ~f:(fun live_registers cfg_node ->
      match cfg_node with
      | Ret -> live_registers
      | Label label ->
        let ({ label = _; code; terminal = _ } : _ Basic_block.t) =
          Hashtbl.find_exn basic_blocks label
        in
        List.fold_right code ~init:live_registers ~f:(fun instr live_registers ->
          List.fold_right
            (instr_args instr)
            ~init:live_registers
            ~f:(fun (kind, value) live_registers ->
            match kind with
            | Use ->
              (* When a variable is used, we know it must be live at this point. *)
              let live_registers =
                Value.fold_registers value ~init:live_registers ~f:Set.add
              in
              Set.iter live_registers ~f:(fun reg1 ->
                Set.iter live_registers ~f:(fun reg2 ->
                  Interference_graph.add_edge graph reg1 reg2));
              live_registers
            | Assignment ->
              (* When a variable is assigned to, it doesn't have to be live before this
                 point anymore. There is a "hole" in the lifetime between the assignment
                 and the previous use where the register isn't needed. *)
              Value.fold_registers value ~init:live_registers ~f:Set.remove)));
    graph
  ;;

  let allocate
    (basic_blocks : Virtual_register.t Basic_block.t list)
    ~register_input_constraints
    ~register_output_constraints
    =
    (* Input constraints mean that the virtual register will use the given real register,
       so immediate take those allocations as given. *)
    let allocations = Hashtbl.copy register_input_constraints in
    (* Output constraints mean that the value in the virtual register must end up in the
       given real register. We can either set the virtual register to be that real
       register, or do a move. *)
    let allocate_register
      :  Virtual_register.t
      -> Asm_program.Register.t Instr.Nonterminal.t option * Asm_program.Register.t
      =
     fun virtual_reg ->
      match Hashtbl.find allocations virtual_reg with
      | Some real_reg -> None, real_reg
      | None ->
        (* FIXME: For output constraints, catch when multiple registers match? *)
        (match Hashtbl.find register_output_constraints virtual_reg with
         | None ->
           (* failwith
             "TODO: pick any available register - but avoid picking one we'll want to \
              use somewhere else?" *)
           None, Rax
         | Some output_reg -> None, output_reg)
    in
    (* FIXME: I think this probably needs to understand Mov, at least? Or maybe we should
       encode all of that in the constraints. 
       
       Ok, let's just go implement some standard algorithm instead of naively rolling my
       own thing here. Can use graph coloring from ocamlgraph which should make this
       pretty easy? Just encode the instructions as a graph.

       Nodes are lifetimes of variables (virtual registers). Can use instruction index
       ranges. Edges are lifetimes that overlap. Need some pre-coloring for input/output
       constraints.

       PROBLEM: How do you detect when you need to spill?
    *)
    List.map basic_blocks ~f:(fun { label; code; terminal } : _ Basic_block.t ->
      let code =
        List.concat_map code ~f:(fun instr ->
          let added_instrs, instr =
            Instr.Nonterminal.fold_map_args instr ~init:[] ~f:(fun acc arg ->
              Value.fold_map_registers arg ~init:acc ~f:(fun acc virtual_reg ->
                let added_instr, reg = allocate_register virtual_reg in
                Option.to_list added_instr @ acc, reg))
          in
          List.rev (instr :: added_instrs))
      in
      { label; code; terminal })
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
  val add_code : t -> Virtual_register.t Instr.Nonterminal.t -> unit
  val add_terminal : t -> Instr.Terminal.t -> unit

  (* TODO: Consider if this API makes any sense *)
  val move_values_for_call
    :  t
    -> call_conv:Call_conv.t
    -> args:Virtual_register.t Value.t Nonempty.t
    -> unit

  val add_local : t -> Mir_name.t -> Virtual_register.t Value.t -> unit
  val find_local : t -> Mir_name.t -> Virtual_register.t Value.t option
  val position_at_label : t -> Label_name.t -> unit
  val pick_register : t -> Virtual_register.t
  val pick_register' : t -> Virtual_register.t Value.t
  val constrain_register_input : t -> Virtual_register.t -> Asm_program.Register.t -> unit
  val constrain_output : t -> Virtual_register.t Value.t -> Asm_program.Register.t -> unit
  val name : t -> Label_name.t
  val basic_blocks : t -> Asm_program.Register.t Basic_block.t list
end = struct
  module Block_builder = struct
    type t =
      { code : Virtual_register.t Instr.Nonterminal.t Queue.t
      ; terminal : Instr.Terminal.t Set_once.t
      }

    let create () = { code = Queue.create (); terminal = Set_once.create () }
  end

  (* TODO: Represents a partial codegened function. We need to consider the state of each
     register. *)
  type t =
    { fun_name : Label_name.t
    ; register_counter : Virtual_register.Counter.t
        (* FIXME: Aren't these constraints time-based? Maybe they need to exist in-line
           with the code, to make the lifetimes explicit? *)
    ; register_input_constraints : Asm_program.Register.t Virtual_register.Table.t
    ; register_output_constraints : Asm_program.Register.t Virtual_register.Table.t
    ; locals : Virtual_register.t Value.t Mir_name.Table.t
    ; basic_blocks : Block_builder.t Label_name.Hash_queue.t
    ; mutable current_label : Label_name.t
    }

  let name t = t.fun_name
  let pick_register t = Virtual_register.Counter.next t.register_counter

  let pick_register' t : _ Value.t =
    Register (Virtual_register.Counter.next t.register_counter)
  ;;

  let get_bb t =
    match Hash_queue.lookup t.basic_blocks t.current_label with
    | Some bb -> bb
    | None ->
      let bb = Block_builder.create () in
      Hash_queue.enqueue_back_exn t.basic_blocks t.current_label bb;
      bb
  ;;

  let add_code t instr = Queue.enqueue (get_bb t).code instr
  let add_terminal t terminal = Set_once.set_exn (get_bb t).terminal [%here] terminal

  let constrain_register_input t virtual_reg real_reg =
    Hashtbl.add_exn t.register_input_constraints ~key:virtual_reg ~data:real_reg
  ;;

  let constrain_register_output t virtual_reg real_reg =
    Hashtbl.update t.register_output_constraints virtual_reg ~f:(function
      | None -> real_reg
      | Some existing ->
        if Asm_program.Register.equal existing real_reg
        then real_reg
        else
          raise_s
            [%message
              "Duplicate output constraint"
                (virtual_reg : Virtual_register.t)
                (real_reg : Asm_program.Register.t)
                (existing : Asm_program.Register.t)])
  ;;

  let constrain_output t (value : Virtual_register.t Value.t) real_reg =
    match value with
    | Register reg -> constrain_register_output t reg real_reg
    | Memory _ | Global _ | Constant _ ->
      let target_reg = pick_register t in
      constrain_register_output t target_reg real_reg;
      add_code t (Mov { src = value; dst = Register target_reg })
  ;;

  let create fun_name =
    (* FIXME: callee saved registers must be marked Used *)
    let t =
      { fun_name
      ; register_counter = Virtual_register.Counter.create ()
      ; register_input_constraints = Virtual_register.Table.create ()
      ; register_output_constraints = Virtual_register.Table.create ()
      ; locals = Mir_name.Table.create ()
      ; basic_blocks = Label_name.Hash_queue.create ()
      ; current_label = fun_name
      }
    in
    Hash_queue.enqueue_back_exn t.basic_blocks fun_name (Block_builder.create ());
    t
  ;;

  let position_at_label t label_name = t.current_label <- label_name
  let add_local t name value = Hashtbl.add_exn t.locals ~key:name ~data:value
  let find_local t name = Hashtbl.find t.locals name

  let move_values_for_call t ~call_conv ~(args : Virtual_register.t Value.t Nonempty.t) =
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
    Array.iter2_exn args arg_registers ~f:(fun arg arg_register ->
      constrain_output t arg arg_register)
  ;;

  (* FIXME: cleanup *)
  (* Set all argument registers as used up-front so we don't try to use them as
       temporary registers. *)
  (* Array.iter arg_registers ~f:(fun reg -> set_register_state t reg Used);
    Array.iteri (Array.zip_exn args arg_registers) ~f:(fun _i (current_loc, target_reg) ->
      match current_loc with
      | Register current_reg ->
        if Register.equal current_reg target_reg
        then (* Already in the right place. Nothing to do. *) ()
        else (
          match
            Array.findi args ~f:(fun (_ : int) -> function
              | Register reg -> Register.equal reg target_reg
              | _ -> false)
          with
          | Some (other_arg, (_ : Value.t)) ->
            (* One of the arguments is sitting in the register we need. Move it elsewhere. *)
            (* FIXME: It's just a matter of making sure we don't clobber a register which is
             currently being used for something. 
             
             Although, it'd be easier if we could just express the register constraints
             and put it into some kind of solver which would work forwards and backwards.
            *)
            let tmp = pick_available_reg_or_stack t in
            add_code t (Mov { src = Register target_reg; dst = tmp });
            args.(other_arg) <- tmp;
            add_code t (Mov { src = Register current_reg; dst = Register target_reg })
          | None ->
            add_code t (Mov { src = Register current_reg; dst = Register target_reg }))
      | (Global _ | Constant _) as src ->
        add_code t (Mov { src; dst = Register target_reg })
      | Memory _ -> failwith "TODO: memory arg location") *)

  let basic_blocks t =
    let basic_blocks =
      Hash_queue.to_alist t.basic_blocks
      |> List.map ~f:(fun (label, { code; terminal }) : _ Basic_block.t ->
           { label
           ; code = Queue.to_list code
           ; terminal = Set_once.get_exn terminal [%here]
           })
    in
    Reg_alloc.allocate
      basic_blocks
      ~register_input_constraints:t.register_output_constraints
      ~register_output_constraints:t.register_output_constraints
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

let pp fmt t = Asm_program.pp fmt (to_program t)

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

let lookup_name_for_fun_call t name ~fun_builder : _ Value.t * Call_conv.t =
  (* FIXME: Handle calling closures *)
  match Function_builder.find_local fun_builder name with
  | Some closure ->
    (* We are calling a closure. Load the function pointer from the first field. *)
    Value.mem_offset closure I64 1, Umber
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
let codegen_fun_call t fun_name args ~fun_builder : _ Value.t =
  let fun_, call_conv = lookup_name_for_fun_call t fun_name ~fun_builder in
  Function_builder.move_values_for_call fun_builder ~call_conv ~args;
  Function_builder.add_code fun_builder (Call fun_);
  let output_register = Function_builder.pick_register fun_builder in
  Function_builder.constrain_register_input
    fun_builder
    output_register
    (Call_conv.return_value_register call_conv);
  Register output_register
;;

let box t ~tag ~fields ~fun_builder =
  let block_field_num = Nonempty.length fields in
  let heap_pointer =
    let allocation_size = 8 * (block_field_num + 1) in
    let extern_name = Label_name.of_string "umber_gc_alloc" in
    let mir_name = Label_name.to_mir_name extern_name in
    declare_extern_c_function t ~mir_name extern_name;
    codegen_fun_call t mir_name [ Constant (Int allocation_size) ] ~fun_builder
  in
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Memory (I16, Value heap_pointer)
       ; src = Constant (Int (Cnstr_tag.to_int tag))
       });
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Value.mem_offset heap_pointer I16 2; src = Constant (Int block_field_num) });
  Nonempty.iteri fields ~f:(fun i field_value ->
    Function_builder.add_code
      fun_builder
      (Mov { dst = Value.mem_offset heap_pointer I64 (i + 1); src = field_value }));
  heap_pointer
;;

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

let get_block_field value index =
  Value.mem_offset value I64 (Mir.Block_index.to_int index + 1)
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
      if not (Virtual_register.equal a b)
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
      let dst = Function_builder.pick_register' fun_builder in
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
  Function_builder.add_terminal fun_builder (Jmp merge_label);
  Function_builder.position_at_label fun_builder label_b;
  Function_builder.add_terminal fun_builder (Jmp merge_label);
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
    codegen_fun_call t fun_name args ~fun_builder
  | Make_block { tag; fields } ->
    (match Nonempty.of_list fields with
     | None -> int_constant_tag tag
     | Some fields ->
       let fields = Nonempty.map fields ~f:(codegen_expr t ~fun_builder) in
       box t ~tag ~fields ~fun_builder)
  | Get_block_field (field_index, block) ->
    let block = codegen_expr t block ~fun_builder in
    get_block_field block field_index
  | Cond_assign { vars; conds; body; if_none_matched } ->
    (* FIXME: Need a way to mint new label names. Guess a name table isn't awful
       Might be easier to just use a counter or something though
       We can prefix with . to get namespacing, kinda relying on nasm a lot with that tho
    *)
    (* Setup is like 
       
       if cond1 then goto cond_binding1
       else if cond2 then goto cond_binding2
       else goto if_none_matched

       cond_binding1:
         a =..
         b =..
         goto body

       body:
         ...

       if_none_matched:
         ...
    *)
    let cond_label i = Label_name.of_string [%string "cond_assign.cond%{i#Int}"] in
    let vars_label i = Label_name.of_string [%string "cond_assign.vars%{i#Int}"] in
    let body_label = Label_name.of_string "cond_assign.body" in
    let if_none_matched_label = Label_name.of_string "cond_assign.if_none_matched" in
    let n_conds = Nonempty.length conds in
    let assign_vars_and_jump_to_body var_exprs =
      List.iter2_exn vars var_exprs ~f:(fun var var_expr ->
        let var_value = codegen_expr t var_expr ~fun_builder in
        (* FIXME: add_local assumes we know where the local is, but we don't. But to
           generate code properly, we need to make sure it ends up in the same place
           every time. Enforce it's always in the same place. *)
        Function_builder.add_local fun_builder var var_value);
      Function_builder.add_terminal fun_builder (Jmp body_label)
    in
    Nonempty.iteri conds ~f:(fun i (cond, var_exprs) ->
      if i <> 0 then Function_builder.position_at_label fun_builder (cond_label i);
      codegen_cond t cond ~fun_builder;
      let next_label =
        if i < n_conds - 1 then cond_label (i + 1) else if_none_matched_label
      in
      Function_builder.add_terminal fun_builder (Jnz next_label);
      Function_builder.position_at_label fun_builder (vars_label i);
      (* If we didn't jump, the condition succeeded, so set the variables. *)
      assign_vars_and_jump_to_body var_exprs);
    (* TODO: We kinda have to represent something like phi nodes, where the branches
       "merge" back into one value. We could either let [Value.t] represent values in
       possibly different places (some kind of union), or enforce that the values end up
       in the same place. Enforcing they're in the same place seems easier. *)
    Function_builder.position_at_label fun_builder if_none_matched_label;
    let otherwise_value =
      match if_none_matched with
      | Use_bindings var_exprs ->
        assign_vars_and_jump_to_body var_exprs;
        None
      | Otherwise otherwise_expr -> Some (codegen_expr t otherwise_expr ~fun_builder)
    in
    (* FIXME: Need to merge the values of the vars *)
    if n_conds > 1 && List.length vars > 0 then failwith "FIXME: merge vars";
    Function_builder.position_at_label fun_builder body_label;
    let body_value = codegen_expr t body ~fun_builder in
    (match otherwise_value with
     | None -> body_value
     | Some otherwise_value ->
       merge_branches
         fun_builder
         (body_label, body_value)
         (if_none_matched_label, otherwise_value)
         ~merge_label:(Label_name.of_string [%string "cond_assign.merge"]))

and codegen_cond t cond ~fun_builder =
  let cmp value value' = Function_builder.add_code fun_builder (Cmp (value, value')) in
  match cond with
  | Equals (expr, literal) ->
    let expr_value = codegen_expr t expr ~fun_builder in
    let literal_value = codegen_literal t literal in
    (* FIXME: Handle conditionally loading values from the stack into registers *)
    let load_expr ~expr_value = get_block_field expr_value (Mir.Block_index.of_int 0) in
    let load_literal ~literal_value =
      get_block_field literal_value (Mir.Block_index.of_int 0)
    in
    (match literal with
     | Int _ | Char _ -> cmp (load_expr ~expr_value) (load_literal ~literal_value)
     | Float _ -> failwith "TODO: float equality in patterns"
     | String _ -> failwith "TODO: string equality in patterns")
  | Constant_tag_equals (expr, tag) ->
    cmp (codegen_expr t expr ~fun_builder) (int_constant_tag tag)
  | Non_constant_tag_equals (expr, tag) ->
    let value = codegen_expr t expr ~fun_builder in
    check_value_is_block fun_builder value;
    (* FIXME: mint new label names *)
    let is_block_label = Label_name.of_string "non_constant_tag_equals.is_block" in
    let end_label = Label_name.of_string "non_constant_tag_equals.end" in
    Function_builder.add_terminal fun_builder (Jz is_block_label);
    (* If it's not a block, we want to "return false" (ZF = 0). This is already the case. *)
    Function_builder.add_terminal fun_builder (Jmp end_label);
    (* If it's a block, check the tag. *)
    Function_builder.position_at_label fun_builder is_block_label;
    Function_builder.add_code
      fun_builder
      (Cmp (Memory (I16, Value value), Constant (Int (Cnstr_tag.to_int tag))));
    Function_builder.position_at_label fun_builder end_label
  | And (cond1, cond2) ->
    codegen_cond t cond1 ~fun_builder;
    let cond1_result = Function_builder.pick_register' fun_builder in
    Function_builder.add_code fun_builder (Setz cond1_result);
    codegen_cond t cond2 ~fun_builder;
    let cond2_result = Function_builder.pick_register' fun_builder in
    Function_builder.add_code fun_builder (Setz cond2_result);
    Function_builder.add_code fun_builder (Test (cond1_result, cond2_result))
;;

let set_global t global expr ~fun_builder =
  Queue.enqueue t.bss_globals global;
  let expr_location = codegen_expr t expr ~fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov { dst = Memory (I64, Value (Global (global, Other))); src = expr_location })
;;

let define_function t ~fun_name ~args ~body =
  (* TODO: Need a function prelude for e.g. the frame pointer *)
  let call_conv : Call_conv.t = Umber in
  let fun_builder = Function_builder.create (Label_name.of_mir_name fun_name) in
  Hashtbl.add_exn t.functions ~key:fun_name ~data:fun_builder;
  let result =
    Nonempty.iter2 args (Call_conv.arg_registers call_conv) ~f:(fun arg_name reg ->
      let virtual_reg = Function_builder.pick_register fun_builder in
      Function_builder.constrain_register_input fun_builder virtual_reg reg;
      Function_builder.add_local fun_builder arg_name (Register virtual_reg))
  in
  (match result with
   | Same_length | Right_trailing _ -> ()
   | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack");
  let return_value = codegen_expr t body ~fun_builder in
  Function_builder.constrain_output
    fun_builder
    return_value
    (Call_conv.return_value_register call_conv);
  Function_builder.add_terminal fun_builder Ret
;;

let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    (* TODO: values *)
    set_global t (Label_name.of_mir_name name) expr ~fun_builder:t.main_function
  | Fun_def { fun_name; args; body } -> define_function t ~fun_name ~args ~body
  | Fun_decl { name; arity = _ } ->
    Hashtbl.add_exn t.externs ~key:name ~data:Umber_function
  | Extern_decl { name; extern_name; arity = _ } ->
    (* TODO: Do we even need arity anymore? Also applies to [Fun_decl] above. *)
    declare_extern_c_function t ~mir_name:name (Label_name.of_extern_name extern_name)
;;

(* FIXME: Decide what to do with this.

   Having umber_apply be a thing at all seems somewhat sad? I guess we need something to
   translate an Umber function call into something the Rust runtime can call though.

   But having to check every function call first is unfortunate. It might be better to
   enforce that constant block wrappers are created for function values (possibly lazily,
   when they're first used.) How about this - when we look up a function name for a call,
   we inline it specially, but other times, the block representation is used.
*)
(* let codegen_runtime_required_functions t =
  (* Ensure that functions the runtime needs are codegened. See closure.rs in the runtime. *)
  (* codegen_umber_apply_fun t ~n_args:2 *)
  set_global
    t
    (Label_name.of_string "umber_apply2")
    (Primitive (Int 0))
    ~fun_builder:t.main_function
;; *)

let main_function_name ~(module_path : Module_path.Absolute.t) =
  Label_name.of_string [%string "umber_main#%{module_path#Module_path}"]
;;

let of_mir ~module_path mir =
  let t = create ~main_function_name:(main_function_name ~module_path) in
  List.iter mir ~f:(codegen_stmt t);
  Function_builder.add_terminal t.main_function Ret;
  t
;;

(* TODO: Move this utility somewhere better. *)
let with_tempfile ~prefix ~suffix ~f =
  let tempfile = Filename_unix.temp_file prefix suffix in
  protect ~f:(fun () -> f tempfile) ~finally:(fun () -> Sys_unix.remove tempfile)
;;

let compile_to_object_file t ~output_file =
  with_tempfile ~prefix:"umber" ~suffix:".asm" ~f:(fun tempfile ->
    Out_channel.with_file tempfile ~f:(fun out ->
      pp (Format.formatter_of_out_channel out) t);
    Shell.run "nasm" [ "-felf64"; tempfile; "-o"; output_file ])
;;

let compile_entry_module ~module_paths ~entry_file =
  let t = create ~main_function_name:(Label_name.of_string "main") in
  let umber_gc_init = Label_name.of_string "umber_gc_init" in
  declare_extern_c_function t umber_gc_init;
  Function_builder.add_code t.main_function (Call (Global (umber_gc_init, Extern_proc)));
  List.iter module_paths ~f:(fun module_path ->
    let fun_name = main_function_name ~module_path in
    Hashtbl.add_exn t.externs ~key:(Label_name.to_mir_name fun_name) ~data:Umber_function;
    Function_builder.add_code t.main_function (Call (Global (fun_name, Extern_proc))));
  Function_builder.add_terminal t.main_function Ret;
  compile_to_object_file t ~output_file:entry_file
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
                  ; src =
                      Memory (I64, Value (Global (Label_name.of_string "message", Other)))
                  }
              ; Call (Global (Label_name.of_string "puts", Extern_proc))
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
  Asm_program.pp Format.std_formatter (to_program (of_mir ~module_path hello_world));
  [%expect
    {|
               default   rel
               global    umber_main#HelloWorld
               global    HelloWorld.#binding.1
               global    umber_apply2
               global    string.210886959:weak
               global    int.0:weak
               extern    umber_print_endline

               section   .text
    umber_main#HelloWorld:
               mov       rdi, string.210886959
               call      umber_print_endline wrt ..plt
               mov       qword [HelloWorld.#binding.1], rax
               mov       qword [umber_apply2], int.0
               ret

               section   .rodata
               sectalign 8
    string.210886959:
               dw        32772
               dw        2
               dd        0
               db        `Hello, world!\x0\x0\x2`
    int.0:
               dw        32769
               dw        1
               dd        0
               dq        0

               section   .bss
               sectalign 8
    HelloWorld.#binding.1:
               resq      1
    umber_apply2:
               resq      1 |}]
;;
