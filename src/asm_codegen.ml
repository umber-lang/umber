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
  module Asm_literal = Asm_literal
  module Call_conv = Call_conv
  module Simple_value = Simple_value
  module Memory = Memory
  module Value = Value
  module Register_op = Register_op
  module Instr = Instr
  module Basic_block = Basic_block
  module Size = Size
  module Data_decl = Data_decl
  module Bss_decl = Bss_decl
  module Global_decl = Global_decl
end

let mir_name_of_label_name label_name =
  Mir_name.create_exportable_name
    (Value_name.Absolute.of_relative_unchecked
       (Value_name.Relative.of_string (Label_name.to_string label_name)))
;;

module Unique_counter () : sig
  type t [@@deriving compare, equal, sexp_of]

  val to_string : t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  module Counter : sig
    type id := t
    type t [@@deriving sexp_of]

    val create : unit -> t
    val next : t -> id
  end
end = struct
  include Int

  module Counter = struct
    type t = int ref [@@deriving sexp_of]

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
  include Hashable.Make_plain (T)
end

module Reg_alloc : sig
  val allocate
    :  basic_blocks:Register.t Basic_block.t list
    -> register_counter:Virtual_register.Counter.t
    -> Asm_program.Register.t Basic_block.t list
end = struct
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
        | Jump jump_target ->
          (match jump_target with
           | Simple_value (Global (label, _)) -> [ Label label ]
           | _ ->
             (* Indirect jumps are used for performing effects. We can treat them
                similarly to calls in that understanding their control flow isn't needed
                for register allocation. *)
             [])
        | Jump_if { cond = _; then_; else_ } -> [ Label then_; Label else_ ]
      in
      List.iter other_nodes ~f:(fun node -> Cfg.add_edge cfg (Label label) node));
    cfg
  ;;

  (* TODO: This logic doesn't seem quite right - it will put in some arbitrary ordering
     between parallel branches on whose assignments go first, which isn't correct. I'm not
     sure exactly what the right thing to do is here, but might involve multiple passes
     where you initially track what each block uses/assigns and then union the information.
     
     Lookup "dominator tree". *)
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

  module Interference_graph = Graph.Imperative.Graph.Concrete (Register)

  module Instr_register_ops = struct
    let use_mem =
      Memory.fold_simple_values ~init:[] ~f:(fun acc v -> (Register_op.Use, v) :: acc)
    ;;

    let record (value : Register.t Value.t) ~op =
      match value with
      | Simple_value value -> [ op, value ]
      | Memory mem ->
        (* Memory only uses registers, it can't assign to them. *)
        use_mem mem
    ;;

    let use = record ~op:Use
    let assign = record ~op:Assignment
    let use_and_assign = record ~op:Use_and_assignment

    let of_nonterminal (instr : _ Instr.Nonterminal.t) =
      match instr with
      | Pop x -> assign x
      | Push x -> use x
      | Cmp (a, b) | Test (a, b) -> use a @ use b
      | Mov { dst; src } -> use src @ assign dst
      | And { dst; src } | Add { dst; src } | Sub { dst; src } ->
        use src @ use_and_assign dst
      | Lea { dst; src } -> use_mem src @ assign (Simple_value (Register dst))
      | Call { fun_; call_conv; arity } ->
        let args, clobbered_arg_registers =
          Call_conv.arg_registers call_conv
          |> Nonempty.to_list
          |> Fn.flip List.split_n arity
        in
        let arg_uses =
          List.map args ~f:(fun reg : (Register_op.t * Register.t Simple_value.t) ->
            Use, Register (Real reg))
        in
        let clobbered_register_assignments, clobbered_register_uses =
          List.map
            (clobbered_arg_registers @ Call_conv.non_arg_caller_save_registers call_conv)
            ~f:(fun reg : ((Register_op.t * _) * (Register_op.t * _)) ->
              let reg : Register.t Simple_value.t = Register (Real reg) in
              (Assignment, reg), (Use, reg))
          |> List.unzip
        in
        let return_value_assignment : Register_op.t * Register.t Simple_value.t =
          Assignment, Register (Real (Call_conv.return_value_register call_conv))
        in
        List.concat
          [ use fun_
          ; clobbered_register_assignments
          ; arg_uses
          ; clobbered_register_uses
          ; [ return_value_assignment ]
          ]
    ;;

    let of_terminal (instr : _ Instr.Terminal.t) =
      match instr with
      | Jump target -> use target
      | Jump_if { cond = _; then_ = _; else_ = _ } | Ret -> []
    ;;
  end

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
    let record_register_use live_registers value =
      (* When a variable is used, we know it must be live at this point. *)
      let live_registers =
        Simple_value.fold_registers value ~init:live_registers ~f:Set.add
      in
      Set.iter live_registers ~f:(fun reg1 ->
        Set.iter live_registers ~f:(fun reg2 ->
          if not (Register.equal reg1 reg2)
          then Interference_graph.add_edge graph reg1 reg2));
      live_registers
    in
    let record_register_assignment live_registers value =
      (* TODO: We should identify when a non-live variable is assigned to. That means the
         assignment is useless or we have some other bug. Either raise or remove the
         assignment. *)
      (* When a variable is assigned to, it doesn't have to be live before this point
         anymore. There is a "hole" in the lifetime between the assignment and the
         previous use where the register isn't needed. *)
      Simple_value.fold_registers value ~init:live_registers ~f:Set.remove
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
          (* FIXME: Decide if it actually matters if we track this or not. If it does,
             construct a test case showing it. *)
          record_register_use
            live_registers
            (Register (Real (Call_conv.return_value_register Umber)))
        | Label label ->
          let ({ label = _; code; terminal } : _ Basic_block.t) =
            Hashtbl.find_exn basic_blocks label
          in
          let record_register_ops live_registers ops =
            List.fold_right ops ~init:live_registers ~f:(fun (op, value) live_registers ->
              match (op : Register_op.t) with
              | Use -> record_register_use live_registers value
              | Assignment -> record_register_assignment live_registers value
              | Use_and_assignment ->
                let live_registers = record_register_assignment live_registers value in
                record_register_use live_registers value)
          in
          let live_registers =
            record_register_ops live_registers (Instr_register_ops.of_terminal terminal)
          in
          List.fold_right code ~init:live_registers ~f:(fun instr live_registers ->
            record_register_ops live_registers (Instr_register_ops.of_nonterminal instr)))
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
        | Mov
            { src = Simple_value (Register src_reg)
            ; dst = Simple_value (Register dst_reg)
            } ->
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
                Simple_value.map_registers value ~f:(fun reg ->
                  match reg with
                  | Real _ -> reg
                  | Virtual virtual_reg ->
                    Hashtbl.find coalesced_registers virtual_reg
                    |> Option.value ~default:reg))
            in
            (* Drop no-op moves which may have appeared due to coalescing. *)
            match instr with
            | Mov
                { src = Simple_value (Register src_reg)
                ; dst = Simple_value (Register dst_reg)
                }
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

  let color_interference_graph
    ~interference_graph
    ~all_available_registers
    ~already_spilled
    =
    (* See https://web.eecs.umich.edu/~mahlke/courses/583f12/reading/chaitin82.pdf for
       a description of the algorithm we are roughly following. *)
    let k = Set.length all_available_registers in
    let vertex_states = Virtual_register.Table.create () in
    let ignored_stack = Stack.create () in
    let degree vertex =
      List.count (Interference_graph.succ interference_graph vertex) ~f:(function
        | Real _ -> true
        | Virtual v ->
          (match Hashtbl.find vertex_states v with
           | None -> true
           | Some (`Ignored | `Spilled) -> false))
    in
    let rec remove_low_degree_vertices () =
      let any_removed = ref false in
      Interference_graph.iter_vertex
        (function
         | Real _ -> ()
         | Virtual virtual_reg as vertex ->
           (match Hashtbl.find vertex_states virtual_reg with
            | Some (`Ignored | `Spilled) -> ()
            | None ->
              if degree vertex < k
              then (
                (* The vertex has [degree < k]. It doesn't affect colorability and can be
                   trivially given a color later. *)
                any_removed := true;
                Stack.push ignored_stack virtual_reg;
                Hashtbl.set vertex_states ~key:virtual_reg ~data:`Ignored)
              else
                (* The vertex has a [degree >= k]. It may still be possible to k-color
                   the graph if some of its neighbours share colors. *)
                ()))
        interference_graph;
      if !any_removed then remove_low_degree_vertices ()
    in
    (* TODO: Could use better heuristics to pick a register to spill rather, considering
       the cost of spilling. See how Chaitin did it. *)
    let pick_a_register_to_spill () =
      Interference_graph.fold_vertex
        (fun vertex current_max ->
          match vertex with
          | Real _ -> current_max
          | Virtual virtual_reg as vertex ->
            (match Hashtbl.find vertex_states virtual_reg with
             | Some (`Ignored | `Spilled) -> current_max
             | None ->
               assert_or_compiler_bug (degree vertex >= k) ~here:[%here];
               (* Do not spill registers that were already spilled. This is important to avoid
                a death spiral of nontermination. *)
               if Hash_set.mem already_spilled virtual_reg
               then current_max
               else (
                 eprint_s
                   [%here]
                   [%lazy_message
                     "Considering spilling"
                       (virtual_reg : Virtual_register.t)
                       (current_max : (Virtual_register.t * int) option)];
                 match current_max with
                 | None -> Some (virtual_reg, degree vertex)
                 | Some (_, max_degree) ->
                   let this_degree = degree vertex in
                   if this_degree > max_degree
                   then Some (virtual_reg, max_degree)
                   else current_max)))
        interference_graph
        None
    in
    let rec loop () =
      remove_low_degree_vertices ();
      (* All remaining nodes have degree >= k. While this remains true, spill. *)
      match pick_a_register_to_spill () with
      | Some (reg_to_spill, (_degree : int)) ->
        eprint_s
          [%here]
          [%lazy_message "Decided to spill" (reg_to_spill : Virtual_register.t)];
        Hashtbl.set vertex_states ~key:reg_to_spill ~data:`Spilled;
        loop ()
      | None -> ()
    in
    loop ();
    let spilled_registers =
      Hashtbl.to_alist vertex_states
      |> List.filter_map ~f:(fun (virtual_reg, state) ->
           match state with
           | `Ignored -> None
           | `Spilled -> Some virtual_reg)
    in
    match Nonempty.of_list spilled_registers with
    | Some spilled_registers ->
      (* If some registers were spilled, fail, prompting the caller to spill them and then
         retry. *)
      Error spilled_registers
    | None ->
      (* Color the trivial nodes. *)
      let coloring = Virtual_register.Table.create () in
      let vertex_color (reg : Register.t) =
        match reg with
        | Real reg -> Some reg
        | Virtual virtual_reg -> Hashtbl.find coloring virtual_reg
      in
      Stack.iter ignored_stack ~f:(fun virtual_reg ->
        (* Pick a color. *)
        let neighbouring_colors =
          Interference_graph.succ interference_graph (Virtual virtual_reg)
          |> List.filter_map ~f:vertex_color
          |> Asm_program.Register.Set.of_list
        in
        let color =
          Set.choose_exn (Set.diff all_available_registers neighbouring_colors)
        in
        Hashtbl.add_exn coloring ~key:virtual_reg ~data:color);
      Ok coloring
  ;;

  (** Replace all mentions of spilled registers with memory loads and stores. *)
  let update_code_to_spill_registers
    ~(basic_blocks : Register.t Basic_block.t list)
    ~newly_spilled_registers
    ~already_spilled_count
    ~create_register
    =
    (* TODO: We don't necessarily need to give each spilled register its own stack slot.
       If their lifetimes don't overlap, two variables could use the same one. But this is
       pretty hard to think about. *)
    let spilled_memory_locations =
      Nonempty.to_list newly_spilled_registers
      |> List.mapi ~f:(fun i reg ->
           let rsp : Register.t Simple_value.t = Register (Real Rsp) in
           reg, Memory.offset rsp I64 (i + already_spilled_count))
      |> Virtual_register.Map.of_alist_exn
    in
    List.map basic_blocks ~f:(fun { label; code; terminal } : _ Basic_block.t ->
      let code =
        List.concat_map code ~f:(fun instr ->
          let (spilled_uses, spilled_assignments), instr =
            Instr.Nonterminal.fold_map_args instr ~init:([], []) ~f:(fun acc value ~op ->
              Simple_value.fold_map_registers value ~init:acc ~f:(fun acc reg ->
                match reg with
                | Real _ -> acc, reg
                | Virtual vreg ->
                  if Nonempty.mem
                       newly_spilled_registers
                       vreg
                       ~equal:Virtual_register.equal
                  then (
                    let tmp : Register.t = Virtual (create_register ()) in
                    let spilled_uses, spilled_assignments = acc in
                    match op with
                    | Use -> ((vreg, tmp) :: spilled_uses, spilled_assignments), tmp
                    | Assignment ->
                      (spilled_uses, (vreg, tmp) :: spilled_assignments), tmp
                    | Use_and_assignment ->
                      ( ((vreg, tmp) :: spilled_uses, (vreg, tmp) :: spilled_assignments)
                      , tmp ))
                  else acc, reg))
          in
          (* TODO: We could be smarter and elide some of these loads if they are read from
             multiple times. That would require reasoning about lifetimes here. *)
          (* TODO: We could elide loads if the value is still in the register and hasn't
             been overwritten yet. *)
          let loads =
            List.map spilled_uses ~f:(fun (reg, tmp) : _ Instr.Nonterminal.t ->
              Mov
                { src = Memory (Map.find_exn spilled_memory_locations reg)
                ; dst = Simple_value (Register tmp)
                })
          in
          let stores =
            List.map spilled_assignments ~f:(fun (reg, tmp) : _ Instr.Nonterminal.t ->
              Mov
                { src = Simple_value (Register tmp)
                ; dst = Memory (Map.find_exn spilled_memory_locations reg)
                })
          in
          List.concat [ loads; [ instr ]; stores ])
      in
      { label; code; terminal })
  ;;

  (* TODO: Could add frame pointer handling here (enter/leave). *)
  (* TODO: It's not required for leaf functions (functions with no calls) to have the
     stack be 16-byte aligned. We could omit that. We also don't necessarily need to
     maintain this invariant all the time - it's only needed when calling C functions. *)
  (** Add code to allocate space for stack variables and release it later. Also ensure
      that the stack is 16-byte aligned within the function body, which is required for
      the C calling convention. *)
  let add_function_prologue_and_epilogue
    ~(basic_blocks : _ Basic_block.t list)
    ~spilled_count
    =
    let stack_space =
      (* When entering a function, rsp will be misaligned due to the call instruction
         pushing the return address. So we need to add an odd number of words to it to get
         back to an even alignment. *)
      let n = if spilled_count mod 2 = 1 then spilled_count else spilled_count + 1 in
      8 * n
    in
    match basic_blocks with
    | [] -> compiler_bug [%message "Empty function"]
    | entry_bb :: all_but_entry ->
      let prologue : Asm_program.Register.t Instr.Nonterminal.t list =
        [ Push (Simple_value (Register Rbp))
        ; Mov { dst = Simple_value (Register Rbp); src = Simple_value (Register Rsp) }
        ; Sub
            { dst = Simple_value (Register Rsp)
            ; src = Simple_value (Constant (Int stack_space))
            }
        ]
      in
      let epilogue : Asm_program.Register.t Instr.Nonterminal.t list =
        [ Add
            { dst = Simple_value (Register Rsp)
            ; src = Simple_value (Constant (Int stack_space))
            }
        ; Pop (Simple_value (Register Rbp))
        ]
      in
      (match List.split_last all_but_entry with
       | None ->
         [ { entry_bb with code = List.concat [ prologue; entry_bb.code; epilogue ] } ]
       | Some (other_bbs, exit_bb) ->
         List.concat
           [ [ { entry_bb with code = prologue @ entry_bb.code } ]
           ; other_bbs
           ; [ { exit_bb with code = exit_bb.code @ epilogue } ]
           ])
  ;;

  let allocate ~(basic_blocks : Register.t Basic_block.t list) ~register_counter =
    eprint_s
      [%here]
      [%lazy_message
        "Starting register allocation" (basic_blocks : Register.t Basic_block.t list)];
    let all_available_registers =
      Call_conv.all_available_registers Umber |> Asm_program.Register.Set.of_list
    in
    let try_find_coloring ~cfg ~basic_blocks ~already_spilled =
      eprint_s
        [%here]
        [%lazy_message
          ""
            ~cfg:
              (Cfg.fold_edges (fun label1 label2 acc -> (label1, label2) :: acc) cfg []
                : (Cfg_node.t * Cfg_node.t) list)];
      let interference_graph = create_interference_graph ~cfg ~basic_blocks in
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
      (* FIXME: Not sure what happened but this interference graph definitely doesn't
         match the code after coalescing. Broken merge logic? *)
      let interference_graph = create_interference_graph ~cfg ~basic_blocks in
      eprint_s
        [%here]
        [%lazy_message
          "Interference graph after coalescing"
            ~interference_graph:
              (Interference_graph.fold_edges
                 (fun reg1 reg2 acc -> (reg1, reg2) :: acc)
                 interference_graph
                 []
                : (Register.t * Register.t) list)];
      ( color_interference_graph
          ~interference_graph
          ~all_available_registers
          ~already_spilled
      , basic_blocks )
    in
    let new_temp_registers = Virtual_register.Hash_set.create () in
    let rec loop ~cfg ~basic_blocks ~already_spilled_count =
      let result, basic_blocks =
        try_find_coloring ~cfg ~basic_blocks ~already_spilled:new_temp_registers
      in
      match result with
      | Error newly_spilled_registers ->
        eprint_s
          [%here]
          [%lazy_message
            "Coloring failed, spilling registers"
              (newly_spilled_registers : Virtual_register.t Nonempty.t)];
        let basic_blocks =
          update_code_to_spill_registers
            ~basic_blocks
            ~newly_spilled_registers
            ~already_spilled_count
            ~create_register:(fun () ->
            let tmp = Virtual_register.Counter.next register_counter in
            Hash_set.add new_temp_registers tmp;
            tmp)
        in
        let already_spilled_count =
          already_spilled_count + Nonempty.length newly_spilled_registers
        in
        loop ~cfg ~basic_blocks ~already_spilled_count
      | Ok coloring ->
        eprint_s
          [%here]
          [%lazy_message
            "Coloring succeeded"
              (coloring : Asm_program.Register.t Virtual_register.Table.t)];
        let basic_blocks =
          List.map basic_blocks ~f:(fun bb ->
            Basic_block.map_registers bb ~f:(function
              | Real reg -> reg
              | Virtual virtual_reg ->
                (match Hashtbl.find coloring virtual_reg with
                 | Some reg -> reg
                 | None ->
                   (* If a register wasn't in the interference graph, we can safely pick
                      any register. *)
                   Set.choose_exn all_available_registers)))
        in
        add_function_prologue_and_epilogue
          ~basic_blocks
          ~spilled_count:already_spilled_count
    in
    loop ~cfg:(create_cfg ~basic_blocks) ~basic_blocks ~already_spilled_count:0
  ;;
end

module Function_builder : sig
  type t [@@deriving sexp_of]

  val create : Label_name.t -> arity:int -> t
  val add_code : t -> Register.t Instr.Nonterminal.t -> unit
  val add_terminal : t -> Register.t Instr.Terminal.t -> unit
  val add_local : t -> Mir_name.t -> Register.t Value.t -> unit
  val find_local : t -> Mir_name.t -> Register.t Value.t option
  val current_label : t -> Label_name.t
  val position_at_label : t -> Label_name.t -> unit
  val create_label : ?global:bool -> t -> ('a, unit, string, Label_name.t) format4 -> 'a
  val pick_register : t -> Register.t Value.t
  val pick_register' : t -> Virtual_register.t
  val name : t -> Label_name.t
  val arity : t -> int

  (* TODO: This could be Nonempty because we always at least have an entry block. *)
  val basic_blocks : t -> Asm_program.Register.t Basic_block.t list
end = struct
  module Block_builder = struct
    type t =
      { code : Register.t Instr.Nonterminal.t Queue.t
      ; terminal : Register.t Instr.Terminal.t Set_once.t
      }
    [@@deriving sexp_of]

    let create () = { code = Queue.create (); terminal = Set_once.create () }
  end

  module Label_id = Unique_counter ()

  type t =
    { fun_name : Label_name.t
    ; arity : int
    ; label_counter : Label_id.Counter.t
    ; register_counter : Virtual_register.Counter.t
    ; locals : Register.t Value.t Mir_name.Table.t
    ; basic_blocks : Block_builder.t Label_name.Hash_queue.t
    ; mutable current_label : Label_name.t
    }
  [@@deriving sexp_of]

  let name t = t.fun_name
  let arity t = t.arity
  let current_label t = t.current_label

  let position_at_label t label_name =
    t.current_label <- label_name;
    if not (Hash_queue.mem t.basic_blocks label_name)
    then
      Hash_queue.enqueue_back_exn t.basic_blocks t.current_label (Block_builder.create ())
  ;;

  let pick_register' t = Virtual_register.Counter.next t.register_counter

  let pick_register t : Register.t Value.t =
    Simple_value (Register (Virtual (pick_register' t)))
  ;;

  let get_bb t = Hash_queue.lookup_exn t.basic_blocks t.current_label

  let prevent_memory_to_memory_instr (instr : _ Instr.Nonterminal.t) ~make_tmp
    : Register.t Instr.Nonterminal.t
    =
    let both_memory (a : _ Value.t) (b : _ Value.t) =
      match a, b with
      | Memory _, Memory _ -> true
      | _ -> false
    in
    let dst_src (variant : _ Variant.t) ~dst ~src =
      if both_memory dst src then variant.constructor ~dst ~src:(make_tmp src) else instr
    in
    let a_b (variant : _ Variant.t) a b =
      if both_memory a b then variant.constructor a (make_tmp b) else instr
    in
    Instr.Nonterminal.Variants.map
      instr
      ~add:dst_src
      ~and_:dst_src
      ~mov:dst_src
      ~sub:dst_src
      ~cmp:a_b
      ~test:a_b
      ~lea:(fun _ ~dst:(_ : Register.t) ~src:(_ : _ Memory.t) -> instr)
      ~call:(fun _ ~fun_:_ ~call_conv:_ ~arity:_ -> instr)
      ~pop:(fun _ _ -> instr)
      ~push:(fun _ _ -> instr)
  ;;

  let add_code t (instr : _ Instr.Nonterminal.t) =
    let bb = get_bb t in
    let instr =
      prevent_memory_to_memory_instr instr ~make_tmp:(fun src ->
        let tmp = pick_register t in
        Queue.enqueue bb.code (Mov { src; dst = tmp });
        tmp)
    in
    Queue.enqueue bb.code instr
  ;;

  let add_terminal t terminal =
    let bb = get_bb t in
    match Set_once.set bb.terminal [%here] terminal with
    | Ok () -> ()
    | Error error ->
      compiler_bug
        [%message
          "Tried to add multiple terminals to a block"
            ~new_terminal:(terminal : Register.t Instr.Terminal.t)
            ~existing_terminal:(bb.terminal : Register.t Instr.Terminal.t Set_once.t)
            ~current_label:(t.current_label : Label_name.t)
            (bb : Block_builder.t)
            (error : Error.t)]
  ;;

  let create fun_name ~arity =
    let t =
      { fun_name
      ; arity
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

  let create_label ?(global = false) t format =
    let prefix = if global then Label_name.to_string t.fun_name else "" in
    let id = Label_id.Counter.next t.label_counter in
    ksprintf
      (fun s -> Label_name.of_string [%string "%{prefix}.%{s}#%{id#Label_id}"])
      format
  ;;

  let basic_blocks t =
    let basic_blocks =
      Hash_queue.to_alist t.basic_blocks
      |> List.map ~f:(fun (label, { code; terminal }) : _ Basic_block.t ->
           { label
           ; code = Queue.to_list code
           ; terminal = Set_once.get_exn terminal [%here]
           })
    in
    Reg_alloc.allocate ~basic_blocks ~register_counter:t.register_counter
  ;;
end

(* TODO: This only handles no-environment wrapper closures. MIR should handle this instead. *)
module Closure = struct
  type t =
    { closure_name : Label_name.t
    ; fun_name : Label_name.t
    ; closure_wrapper_fun_name : Label_name.t
    ; arity : int
    }
  [@@deriving sexp_of]

  let create ~fun_name ~arity =
    { fun_name
    ; closure_wrapper_fun_name =
        Label_name.of_string [%string "%{fun_name#Label_name}#closure_wrapper_fun"]
    ; closure_name = Label_name.of_string [%string "%{fun_name#Label_name}#closure"]
    ; arity
    }
  ;;
end

module Global = struct
  module This_file = struct
    type t =
      | Global_variable of Label_name.t
      | Function of Function_builder.t
    [@@deriving sexp_of]

    let label_name = function
      | Global_variable label -> label
      | Function fun_builder -> Function_builder.name fun_builder
    ;;
  end

  module Other_file = struct
    type t =
      | Global_variable of Label_name.t
      | Umber_function of
          { label_name : Label_name.t
          ; arity : int
          }
      | C_function of
          { label_name : Label_name.t
          ; arity : int
          ; local_wrapper_function : Function_builder.t option
          }
    [@@deriving sexp_of]

    let label_name = function
      | Global_variable label_name
      | Umber_function { label_name; _ }
      | C_function { label_name; _ } -> label_name
    ;;
  end

  type t =
    | This_file of This_file.t
    | Other_file of Other_file.t
  [@@deriving sexp_of]

  let label_name = function
    | This_file this -> This_file.label_name this
    | Other_file other -> Other_file.label_name other
  ;;
end

type t =
  { globals : Global.t Mir_name.Table.t
  ; literals : Label_name.t Literal.Table.t
  ; closures : Closure.t Label_name.Table.t
  ; main_function : Function_builder.t
  ; mutable umber_resume_wrapper : Function_builder.t option
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
      ~data_kind:I64
      (Literal (Int (Uchar.to_int c)))
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

let move_values_for_call fun_builder ~call_conv ~(args : Register.t Value.t list) =
  let n_args = List.length args in
  let arg_registers, clobbered_arg_registers =
    Call_conv.arg_registers call_conv |> Nonempty.to_list |> Fn.flip List.split_n n_args
  in
  let _clobbered_registers =
    clobbered_arg_registers @ Call_conv.non_arg_caller_save_registers call_conv
  in
  if n_args > List.length arg_registers
  then failwith "TODO: ran out of registers for args, use stack";
  (* Use a temporary to avoid cases where we'd overwrite something already in one of the
     target registers. It's important to move to all the temporaries first - otherwise,
     by writing to one of the real registers, it's possible we could lose one of the arg
     values if it was already in that register. *)
  let temporaries_to_move =
    List.map args ~f:(fun arg ->
      let tmp = Function_builder.pick_register fun_builder in
      Function_builder.add_code fun_builder (Mov { src = arg; dst = tmp });
      tmp)
  in
  List.iter2_exn temporaries_to_move arg_registers ~f:(fun tmp arg_register ->
    Function_builder.add_code
      fun_builder
      (Mov { src = tmp; dst = Simple_value (Register (Real arg_register)) }))
;;

(* FIXME: I'm not sure if this is actually needed - does it even do anything? The new
     virtual registers will always be unused so die just immediately. But it creates a lot
     more work for register allocation.
     
     I suspect this might not have done anything, except maybe given regalloc an out to
     move to other registers between live ranges.

     Oh, maybe we have to insert moves between the live ranges??
  *)
(* In case we are using any clobbered registers, move from them to new temporary
     registers. Any useless moves will get removed by the register allocator later. *)
(* List.iter clobbered_registers ~f:(fun reg ->
    let tmp = Function_builder.pick_register fun_builder in
    Function_builder.add_code
      fun_builder
      (Mov { src = Simple_value (Register (Real reg)); dst = tmp })) *)

let codegen_fun_call_internal fun_builder ~fun_ ~call_conv ~args =
  move_values_for_call fun_builder ~call_conv ~args;
  Function_builder.add_code
    fun_builder
    (Call { fun_; call_conv; arity = List.length args });
  let output_register = Function_builder.pick_register' fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = Simple_value (Register (Real (Call_conv.return_value_register call_conv)))
       ; dst = Simple_value (Register (Virtual output_register))
       });
  output_register
;;

let constant_block_for_closure ~closure_wrapper_fun_name =
  constant_block
    ~tag:Cnstr_tag.closure
    ~len:1
    ~data_kind:I64
    (Label closure_wrapper_fun_name)
;;

(* Define a wrapper that adds an extra argument (the empty closure env) and ignores it. *)
let define_no_env_closure_wrapper_fun ~closure_wrapper_fun_name ~fun_name ~arity =
  let fun_builder = Function_builder.create closure_wrapper_fun_name ~arity:(arity + 1) in
  let args =
    Call_conv.arg_registers Umber
    |> Nonempty.tl (* Ignore first arg. *)
    |> Fn.flip List.take arity
    |> List.map ~f:(fun reg ->
         let virtual_reg = Function_builder.pick_register fun_builder in
         Function_builder.add_code
           fun_builder
           (Mov { src = Simple_value (Register (Real reg)); dst = virtual_reg });
         virtual_reg)
  in
  (* FIXME: Handle using stack for args. *)
  let return_value =
    codegen_fun_call_internal
      fun_builder
      ~fun_:(Simple_value (Global (fun_name, Other)))
      ~call_conv:Umber
      ~args
  in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = Simple_value (Register (Virtual return_value))
       ; dst = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
       });
  Function_builder.add_terminal fun_builder Ret;
  Function_builder.basic_blocks fun_builder
;;

let to_program { globals; literals; closures; main_function; umber_resume_wrapper }
  : Asm_program.t
  =
  let program =
    Hashtbl.fold globals ~init:Asm_program.empty ~f:(fun ~key:_ ~data:global program ->
      let label_name = Global.label_name global in
      match global with
      | This_file this_file ->
        let program =
          match this_file with
          | Global_variable label_name ->
            { program with
              bss_section =
                { label = label_name; kind = `Words; size = 1 } :: program.bss_section
            }
          | Function fun_builder ->
            { program with
              text_section =
                Function_builder.basic_blocks fun_builder @ program.text_section
            }
        in
        { program with
          globals = { name = label_name; strength = `Strong } :: program.globals
        }
      | Other_file other_file ->
        let program =
          match other_file with
          | C_function { local_wrapper_function = Some fun_builder; _ } ->
            { program with
              globals =
                { name = Function_builder.name fun_builder; strength = `Weak }
                :: program.globals
            ; text_section =
                Function_builder.basic_blocks fun_builder @ program.text_section
            }
          | C_function { local_wrapper_function = None; _ }
          | Global_variable _ | Umber_function _ -> program
        in
        { program with externs = label_name :: program.externs })
  in
  let literals = Hashtbl.to_alist literals in
  let closures = Hashtbl.data closures in
  { program with
    globals =
      { name = Function_builder.name main_function; strength = `Strong }
      :: (Option.to_list
            (Option.map umber_resume_wrapper ~f:(fun fun_builder : Global_decl.t ->
               { name = Function_builder.name fun_builder; strength = `Weak }))
          @ program.globals
          @ List.concat_map
              closures
              ~f:(fun { closure_name; closure_wrapper_fun_name; _ } : Global_decl.t list
                 ->
              [ { name = closure_name; strength = `Weak }
              ; { name = closure_wrapper_fun_name; strength = `Weak }
              ])
          @ List.map literals ~f:(fun ((_ : Literal.t), name) : Global_decl.t ->
              { name; strength = `Weak }))
  ; text_section =
      Function_builder.basic_blocks main_function
      @ program.text_section
      @ List.concat_map
          closures
          ~f:(fun { fun_name; closure_wrapper_fun_name; arity; _ } ->
          define_no_env_closure_wrapper_fun ~closure_wrapper_fun_name ~fun_name ~arity)
      @ Option.value_map umber_resume_wrapper ~f:Function_builder.basic_blocks ~default:[]
  ; rodata_section =
      List.map literals ~f:(fun (literal, label) : Data_decl.t ->
        { label; payloads = constant_block_for_literal literal })
      @ List.map
          closures
          ~f:(fun { closure_name; closure_wrapper_fun_name; _ } : Data_decl.t ->
          { label = closure_name
          ; payloads = constant_block_for_closure ~closure_wrapper_fun_name
          })
  }
;;

let create ~main_function_name =
  { globals = Mir_name.Table.create ()
  ; literals = Literal.Table.create ()
  ; closures = Label_name.Table.create ()
  ; main_function = Function_builder.create main_function_name ~arity:0
  ; umber_resume_wrapper = None
  }
;;

let int_constant_tag tag : _ Value.t =
  (* Put the int63 into an int64 and make the bottom bit 1. *)
  Simple_value (Constant (Int (Int.shift_left (Cnstr_tag.to_int tag) 1 + 1)))
;;

(* TODO: Amend MIR to treat function pointers and closures differently. *)
let lookup_name_for_value t name ~fun_builder : _ Value.t =
  let wrapper_closure fun_name ~arity : _ Value.t =
    let closure =
      Hashtbl.find_or_add t.closures fun_name ~default:(fun () ->
        Closure.create ~fun_name ~arity)
    in
    Simple_value (Global (closure.closure_name, Other))
  in
  match Function_builder.find_local fun_builder name with
  | Some value -> value
  | None ->
    (match Hashtbl.find_exn t.globals name with
     | This_file (Global_variable label_name) | Other_file (Global_variable label_name) ->
       (* Deference memory to get the value of global variables. *)
       Memory (I64, Value (Global (label_name, Other)))
     | This_file (Function fun_builder) ->
       wrapper_closure
         (Function_builder.name fun_builder)
         ~arity:(Function_builder.arity fun_builder)
     | Other_file
         (Umber_function { label_name; arity } | C_function { label_name; arity; _ }) ->
       wrapper_closure label_name ~arity)
;;

let load_mem_offset fun_builder (value : _ Value.t) size offset : _ Value.t =
  let simple_value =
    match value with
    | Simple_value v -> v
    | Memory mem ->
      let tmp : Register.t Simple_value.t =
        Register (Virtual (Function_builder.pick_register' fun_builder))
      in
      Function_builder.add_code
        fun_builder
        (Mov { dst = Simple_value tmp; src = Memory mem });
      tmp
  in
  Memory (Memory.offset simple_value size offset)
;;

let lookup_name_for_fun_call t name ~fun_builder
  : [ `Closure | `Function ] * _ Value.t * Call_conv.t
  =
  match Function_builder.find_local fun_builder name with
  | Some closure -> `Closure, closure, Umber
  | None ->
    (match Hashtbl.find_exn t.globals name with
     | This_file (Function fun_builder) ->
       `Function, Simple_value (Global (Function_builder.name fun_builder, Other)), Umber
     | Other_file (Umber_function { label_name; _ }) ->
       `Function, Simple_value (Global (label_name, Extern_proc)), Umber
     | Other_file (C_function { label_name; _ }) ->
       `Function, Simple_value (Global (label_name, Extern_proc)), C
     | This_file (Global_variable label_name) | Other_file (Global_variable label_name) ->
       `Closure, Memory (I64, Value (Global (label_name, Other))), Umber)
;;

let codegen_fun_call t fun_name args ~fun_builder =
  (* TODO: When [fun_] is a closure, we'll load the function pointer from memory again,
     but we might already know what it is - we could inline that. If we stored more info
     other than just [Value.t] in the locals table, [lookup_name_for_fun_call] could do
     that. *)
  let kind, fun_, call_conv = lookup_name_for_fun_call t fun_name ~fun_builder in
  let fun_, args =
    match kind with
    | `Function -> fun_, args
    | `Closure ->
      (* We are calling a closure. Load the function pointer from the first field, and
         pass the closure itself as the first argument. *)
      load_mem_offset fun_builder fun_ I64 1, fun_ :: args
  in
  codegen_fun_call_internal fun_builder ~fun_ ~call_conv ~args
;;

let declare_and_call_extern_c_function t ~fun_builder ~fun_name:label_name ~args =
  let arity = List.length args in
  ignore
    (Hashtbl.add
       t.globals
       ~key:(mir_name_of_label_name label_name)
       ~data:
         (Other_file (C_function { label_name; arity; local_wrapper_function = None }))
      : [ `Ok | `Duplicate ]);
  codegen_fun_call_internal
    fun_builder
    ~fun_:(Simple_value (Global (label_name, Extern_proc)))
    ~call_conv:C
    ~args
;;

let box t ~tag ~fields ~fun_builder =
  let block_field_num = Nonempty.length fields in
  let heap_pointer_reg =
    let allocation_size = 8 * (block_field_num + 1) in
    declare_and_call_extern_c_function
      t
      ~fun_builder
      ~fun_name:(Label_name.of_string "umber_gc_alloc")
      ~args:[ Simple_value (Constant (Int allocation_size)) ]
  in
  let heap_pointer : Register.t Simple_value.t = Register (Virtual heap_pointer_reg) in
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Memory (I16, Value heap_pointer)
       ; src = Simple_value (Constant (Int (Cnstr_tag.to_int tag)))
       });
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Memory (Memory.offset heap_pointer I16 1)
       ; src = Simple_value (Constant (Int block_field_num))
       });
  Nonempty.iteri fields ~f:(fun i field_value ->
    Function_builder.add_code
      fun_builder
      (Mov { dst = Memory (Memory.offset heap_pointer I64 (i + 1)); src = field_value }));
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
  Simple_value (Global (name, Other))
;;

(* TODO: When loading multiple fields, we could just do the load once rather than loading
   to temporary registers multiple times. Alternatively we could inline the memory
   references with each use, but that would require some way to express that. *)
let load_block_field fun_builder value index =
  load_mem_offset fun_builder value I64 (Mir.Block_index.to_int index + 1)
;;

let check_value_is_block fun_builder value =
  (* Check if this value is a pointer to a block. Pointers always have bottom bit 0.
     This is done by checking (in C syntax) `(value & 1) == 0`. *)
  Function_builder.add_code fun_builder (Test (value, Simple_value (Constant (Int 1))));
  `Zero_flag
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
    | Simple_value (Register a), Simple_value (Register b) ->
      (* FIXME: Unclear if a might be used for something else useful in the b case? *)
      if not (Register.equal a b)
      then (
        Function_builder.position_at_label fun_builder label_b;
        Function_builder.add_code fun_builder (Mov { dst = value_a; src = value_b }));
      value_a
    | Simple_value (Register _), (Simple_value (Global _ | Constant _) | Memory _) ->
      Function_builder.position_at_label fun_builder label_b;
      Function_builder.add_code fun_builder (Mov { dst = value_a; src = value_b });
      value_a
    | (Simple_value (Global _ | Constant _) | Memory _), Simple_value (Register _) ->
      Function_builder.position_at_label fun_builder label_a;
      Function_builder.add_code fun_builder (Mov { dst = value_b; src = value_a });
      value_b
    | ( (Simple_value (Global _ | Constant _) | Memory _)
      , (Simple_value (Global _ | Constant _) | Memory _) ) ->
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
  Function_builder.add_terminal
    fun_builder
    (Jump (Simple_value (Global (merge_label, Other))));
  Function_builder.position_at_label fun_builder label_b;
  Function_builder.add_terminal
    fun_builder
    (Jump (Simple_value (Global (merge_label, Other))));
  Function_builder.position_at_label fun_builder merge_label;
  value
;;

(** This needs to be kept in sync with the definition of [Fiber] in [effects.rs]*)
module Fiber_layout : sig
  val current : Register.t Simple_value.t
  val parent : Register.t Simple_value.t -> Register.t Memory.t
  val saved_rbp : Register.t Simple_value.t -> Register.t Memory.t
  val saved_rsp : Register.t Simple_value.t -> Register.t Memory.t
  val total_size : Register.t Simple_value.t -> Register.t Memory.t
  val handler_count : Register.t Simple_value.t -> Register.t Memory.t
  val get_handler_effect_op_id : Register.t Simple_value.t -> int -> Register.t Memory.t
  val get_handler_pointer : Register.t Simple_value.t -> int -> Register.t Memory.t
end = struct
  let current : Register.t Simple_value.t = Register (Real Call_conv.Umber.fiber_register)
  let get_field fiber i = Memory.offset fiber I64 i
  let parent fiber = get_field fiber 0
  let saved_rbp fiber = get_field fiber 1
  let saved_rsp fiber = get_field fiber 2
  let total_size fiber = get_field fiber 3
  let handler_count fiber = get_field fiber 4
  let number_of_header_fields = 5

  let get_handler_effect_op_id fiber i =
    get_field fiber (number_of_header_fields + (2 * i))
  ;;

  let get_handler_pointer fiber i = get_field fiber (number_of_header_fields + (2 * i) + 1)
end

let set_rsp_for_fiber_just_created ~fun_builder =
  let fiber_size = Function_builder.pick_register' fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Simple_value (Register (Virtual fiber_size))
       ; src = Memory (Fiber_layout.total_size Fiber_layout.current)
       });
  Function_builder.add_code
    fun_builder
    (Lea
       { dst = Real Rsp
       ; src = I8, Add (Value Fiber_layout.current, Value (Register (Virtual fiber_size)))
       })
;;

(* FIXME: Stack variables can't live across this point - how can we prevent this? *)
(* FIXME: Need to push the return instruction pointer. *)
let switch_to_fiber ~fun_builder ~new_fiber ~operation =
  (* Save the stack pointer of the current fiber, unless it's about to be destroyed. *)
  (match operation with
   | `Create_child | `Normal_switch ->
     Function_builder.add_code
       fun_builder
       (Mov
          { dst = Memory (Fiber_layout.saved_rbp Fiber_layout.current)
          ; src = Simple_value (Register (Real Rbp))
          });
     Function_builder.add_code
       fun_builder
       (Mov
          { dst = Memory (Fiber_layout.saved_rsp Fiber_layout.current)
          ; src = Simple_value (Register (Real Rsp))
          })
   | `Destroy_child -> ());
  (* Switch to the new fiber. *)
  Function_builder.add_code
    fun_builder
    (Mov { dst = Simple_value Fiber_layout.current; src = new_fiber });
  match operation with
  | `Create_child ->
    (* For newly created fibers, set the stack pointer to the end of the fiber. *)
    set_rsp_for_fiber_just_created ~fun_builder
  | `Destroy_child | `Normal_switch ->
    (* For resuming existing fibers, load the previously saved stack pointer. *)
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Simple_value (Register (Real Rbp))
         ; src = Memory (Fiber_layout.saved_rbp Fiber_layout.current)
         });
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Simple_value (Register (Real Rsp))
         ; src = Memory (Fiber_layout.saved_rsp Fiber_layout.current)
         })
;;

let retrieve_arguments ~fun_builder ~call_conv ~args =
  match Nonempty.of_list args with
  | None -> ()
  | Some args ->
    let result =
      Nonempty.iter2 args (Call_conv.arg_registers call_conv) ~f:(fun arg_name reg ->
        let virtual_reg = Function_builder.pick_register fun_builder in
        Function_builder.add_code
          fun_builder
          (Mov { src = Simple_value (Register (Real reg)); dst = virtual_reg });
        Function_builder.add_local fun_builder arg_name virtual_reg)
    in
    (match result with
     | Same_length | Right_trailing _ -> ()
     | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack")
;;

(* TODO: It might make more sense for MIR to take on the responsibility for representing
   continuations as blocks. *)
(* TODO: We could define this function once in the entry module rather than generating it
   multiple times. *)
(* FIXME: This doesn't quite work:
   - We adjust rsp at the end of the function, but after switching fibers, this is out of
     sync.
   - After resuming, we're going to run the inner computation some more but then eventually
     exit out of the original handler block back into that code - wrong!
   - We need calling resume to actually return back to the same place. Resume returns the
     final result of calling the handler on the whole rest of the computation, including
     the value branch. I think we might need to do a proper call/ret setup? The
     recursive handling is kinda mind-bending to think about.
   - Resume needs to go back to the perform call
   
   We need to build up a call stack with each resume, and end_label needs to return up the
   stack. So the original return point also needs to be pushed.
*)
let define_umber_resume_wrapper t =
  match t.umber_resume_wrapper with
  | Some fun_builder -> Function_builder.name fun_builder
  | None ->
    let arity = 2 in
    let fun_builder =
      Function_builder.create (Label_name.of_string "umber_resume_wrapper") ~arity
    in
    (* TODO: code duplication with process_arguments and extern c wrapper generation *)
    let args =
      Call_conv.arg_registers Umber
      |> Nonempty.to_list
      |> Fn.flip List.take arity
      |> List.map ~f:(fun reg ->
           let virtual_reg = Function_builder.pick_register fun_builder in
           Function_builder.add_code
             fun_builder
             (Mov { src = Simple_value (Register (Real reg)); dst = virtual_reg });
           virtual_reg)
    in
    let continuation, arg =
      match args with
      | [ continuation; arg ] -> continuation, arg
      | _ -> compiler_bug [%message "Resumptions should have 2 args"]
    in
    (* TODO: Centralize logic for continuation representation somewhere. *)
    let fiber_to_resume =
      load_block_field fun_builder continuation (Mir.Block_index.of_int 1)
    in
    let resume_address =
      load_block_field fun_builder continuation (Mir.Block_index.of_int 2)
    in
    let (_ : Virtual_register.t) =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_fiber_reparent")
        ~args:[ fiber_to_resume; Simple_value Fiber_layout.current ]
    in
    (* Before switching fibers, push the place the return to. *)
    let end_label = Function_builder.create_label fun_builder "end" in
    Function_builder.add_code
      fun_builder
      (Push (Simple_value (Global (end_label, Other))));
    (* FIXME: What about the return address of the resumption? If we don't perform any
     effects, we need to somehow end up back here. How? Ah, I think reparenting the fibers
     should do it. One problem - when a function returns normally, we need to make sure it
     correctly goes back to the parent fiber. Check for fiber stack underflow? Maybe
     just store the correct return addresses on the stack? *)
    switch_to_fiber ~fun_builder ~new_fiber:fiber_to_resume ~operation:`Normal_switch;
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
         ; src = arg
         });
    (* FIXME: We never ret or add to rsp at the end of this function - where exactly are
       we jumping back to? I think there might just be wasted stack space here.
       
       Changed it to use Call, consider if we even want to ever use indirect jumps. Maybe
       it's just too annoying to get it working. Ugh, call doesn't work either because
       we switch fibers during the function and don't switch back. Maybe we need to switch
       back?
       
       This code only works as written because rax is the return value register and the
       first argument register. Very sketchy.
    *)
    Function_builder.add_terminal fun_builder (Jump resume_address);
    Function_builder.position_at_label fun_builder end_label;
    Function_builder.add_terminal fun_builder Ret;
    t.umber_resume_wrapper <- Some fun_builder;
    Function_builder.name fun_builder
;;

let rec codegen_expr t (expr : Mir.Expr.t) ~(fun_builder : Function_builder.t) =
  match expr with
  | Primitive literal -> codegen_literal t literal
  | Name name -> lookup_name_for_value t name ~fun_builder
  | Let (name, expr, body) ->
    (* TODO: The way [Function_builder.add_local] works means that it kinda effectively
       inlines any parts of the [expr] that don't result in code immediately getting
       emitted e.g. any memory references are loaded in all the callers. This just seems a
       bit suspicious and will produce more loads than we need, though using fewer
       registers. A quick way to hack around this would be for [add_local] to move into a
       register if the value is a memory reference. The better long-term approach is
       probably to represent loads/stores as explicit instructions (needed for ARM
       support). *)
    let expr = codegen_expr t expr ~fun_builder in
    Function_builder.add_local fun_builder name expr;
    codegen_expr t body ~fun_builder
  | Fun_call (fun_name, args) ->
    (* TODO: Maybe we could do something cleverer like suggest a good place to put the
       expression value (to reduce the need for moves) *)
    let args = Nonempty.map args ~f:(codegen_expr t ~fun_builder) |> Nonempty.to_list in
    Simple_value (Register (Virtual (codegen_fun_call t fun_name args ~fun_builder)))
  | Make_block { tag; fields } ->
    (* TODO: THis is a dirty hack. I think we really should adjust Mir to represent
       closures and functions differently. But it can wait a bit *)
    if Cnstr_tag.equal tag Cnstr_tag.closure
    then (
      match fields with
      | Name closure_name :: fields ->
        let fields : _ Value.t Nonempty.t =
          Simple_value (Global (Label_name.of_mir_name closure_name, Other))
          :: List.map fields ~f:(codegen_expr t ~fun_builder)
        in
        Simple_value (Register (Virtual (box t ~tag ~fields ~fun_builder)))
      | _ -> compiler_bug [%message "Unexpected closure representation"])
    else (
      match Nonempty.of_list fields with
      | None -> int_constant_tag tag
      | Some fields ->
        let fields = Nonempty.map fields ~f:(codegen_expr t ~fun_builder) in
        Simple_value (Register (Virtual (box t ~tag ~fields ~fun_builder))))
  | Get_block_field (field_index, block) ->
    let block = codegen_expr t block ~fun_builder in
    load_block_field fun_builder block field_index
  | Cond_assign { vars; conds; body; if_none_matched } ->
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
      Function_builder.add_terminal
        fun_builder
        (Jump (Simple_value (Global (body_label, Other))))
    in
    Nonempty.iteri conds ~f:(fun i (cond, var_exprs) ->
      if i <> 0 then Function_builder.position_at_label fun_builder cond_labels.(i);
      let next_label =
        if i < n_conds - 1 then cond_labels.(i + 1) else if_none_matched_label
      in
      match codegen_cond t cond ~fun_builder with
      | `Constant true ->
        (* Unconditonally set the variables. We could also skip generating the rest of the
           code, but it's not worth the trouble right now. *)
        assign_vars_and_jump_to_body var_exprs
      | `Constant false ->
        (* Don't bother checking a condition which will never succeed, or generating code
           to run if it passes. Just go to the next condition. *)
        Function_builder.add_terminal
          fun_builder
          (Jump (Simple_value (Global (next_label, Other))))
      | `Zero_flag ->
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
  | Handle_effects { vars; value_handler; effect_handlers; expr = inner_expr } ->
    (* FIXME: Make sure the new stack ends up 16-byte aligned. Ah, I think we'll actually
       be fine because each handler takes up 16 bytes. *)
    (* FIXME: Need to add function prologues to check for stack overflow. *)
    (* Allocate a new fiber, with its parent set to the current fiber. *)
    let new_fiber =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_fiber_create")
        ~args:[ Simple_value (Register (Real Call_conv.Umber.fiber_register)) ]
    in
    let new_fiber : Register.t Simple_value.t = Register (Virtual new_fiber) in
    (* [parent] and [total_size] are set by the runtime. We need to set [handler_count]
       and [parent_rsp]. *)
    let effect_handler_count = Nonempty.length effect_handlers in
    let effect_handler_labels =
      Array.init effect_handler_count ~f:(fun i ->
        Function_builder.create_label fun_builder "handle_effects.handler%d" i)
    in
    let effect_handlers_end_label =
      Function_builder.create_label fun_builder "handle_effects.end_handlers"
    in
    let end_label = Function_builder.create_label fun_builder "handle_effects.end" in
    (* Make is so the value branch can return to the end label. *)
    (* FIXME: This will completely mess up any previous stack variables and also break the
       stack's alignment. Maybe we could teach reg alloc to understand it though.
       
       We could manually fix the alignment here, and could use rbp-relative indexing to
       make stack variable tracking easier?
    *)
    Function_builder.add_code
      fun_builder
      (Push (Simple_value (Global (end_label, Other))));
    (* Set up the handlers. *)
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Memory (Fiber_layout.handler_count new_fiber)
         ; src = Simple_value (Constant (Int effect_handler_count))
         });
    Nonempty.iteri
      effect_handlers
      ~f:(fun i { effect_op; args = _; resume = _; handler = _ } ->
      Function_builder.add_code
        fun_builder
        (Mov
           { dst = Memory (Fiber_layout.get_handler_effect_op_id new_fiber i)
           ; src = Simple_value (Constant (Int (Mir.Effect_op_id.to_int effect_op)))
           });
      Function_builder.add_code
        fun_builder
        (Mov
           { dst = Memory (Fiber_layout.get_handler_pointer new_fiber i)
           ; src = Simple_value (Global (effect_handler_labels.(i), Other))
           }));
    (* Set up the arguments to pass to the inner expression. It's important that we move
       the values *before* switching fibers so any references to the stack are correct. *)
    let args_for_call, args_for_inner =
      List.map vars ~f:(fun (original_var, new_var) ->
        lookup_name_for_value t original_var ~fun_builder, new_var)
      |> List.unzip
    in
    move_values_for_call fun_builder ~call_conv:Umber ~args:args_for_call;
    (* Switch to the newly created fiber and run the inner expression. *)
    switch_to_fiber
      ~fun_builder
      ~new_fiber:(Simple_value new_fiber)
      ~operation:`Create_child;
    let inner_fun =
      define_sub_function
        t
        ~fun_builder
        ~args:args_for_inner
        ~body:inner_expr
        ("handle_effects.inner" : (_, unit, string, Label_name.t) format4)
    in
    Function_builder.add_code
      fun_builder
      (Call
         { fun_ = Simple_value (Global (inner_fun, Other))
         ; call_conv = Umber
         ; arity = List.length args_for_inner
         });
    let inner_result = Function_builder.pick_register fun_builder in
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = inner_result
         ; src = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
         });
    (* FIXME: Create functions for the value and effect handler branches. Needed to make
       it easier for reg alloc to understand the control flow - we need "ret". Doesn't make
       much difference otherwise. Ah, but have to make sure places that get here via jumps
       instead of "call" have the correct stack alignment.
       
       Problem: I don't think they just be regular function calls, we need to do some
       funky stuff.

       - Set up and switch to child fiber
       - Call inner on child fiber
       - Perform effect: switch to parent fiber and call handler
       - Resume continuation (but add 1 after)
       - Perform effect
       - Resume continuation (but add 1 after)
       - Return value -> go back up the stack and return 7

       Parent fiber : effect_handler_add_1, effect_handler_add_1
       Child fiber  : value_handler

       -- with value/effect handler functions ... ???
       Parent fiber : ??, effect_handler_add_1, ??, effect_handler_add_1, ??, ...
       Child fiber  : value_handler

       Possibly we can get away with just using ret and the reg alloc will be a little
       weird (it'll think it exits the function) but maybe it'll be ok?). Can try. Where
       does the value branch return to? the end label! Push that.

       I think we need functions... otherwise how do we explain to reg alloc that any reg
       could have been clobbered? Oh, also we need to store the closed-over vars somewhere.
       Can put them all on the stack? Seems reasonable. Need to track vars used by the
       inner expr to be passed as vars and vars used by the handlers to be put on the
       parent fiber stack.
       => Well, wait, actually it might sort of work accidentally because we call the
       inner expr function, I think it can figure out that everything gets clobbered there.
       Very sketchy though. Also not sure what the cfg looks like if it can't see the jumps.

       To fix:
       - rsp to rbp based indexing and/or make handlers functions. Using rbp seems a bit
         rough actually since it means we also have to fix that whenever we switch fibers,
         right? We'd need to save it in the fiber as well. Another option is make reg alloc
         understand push/pop. Might not be that bad... Could traverse the cfg and fail on
         inconsistencies
         => ok, we make them functions, should be fine then? Might have to add an option
            to say whether the function needs alignment or not (the handlers don't)
            Wait, the problem with making them functions is Ret won't work properly....
            But, they can't share stack addresses with the parent, it's dynamic, can't be
            done without rbp.
         => Which is easier? adding rbp on every fun prologue/epilogue and fiber switch
            OR making these functions and... passing the args how? In a closure could work
            Registers won't work. Stack could work but it's dynamic again. Maybe rbp is the
            way. 

       Mitigated:
       - umber_resume_wrapper needs to put its own address on the parent stack (done)
       - What do we do with the return value from the return value branch? Oh, just return
         it in rax!
    *)
    (* At this point, since the inner function has returned a regular value, it won't be
       resumed again. Destroy the fiber and switch back to the parent fiber. *)
    let (_ : Virtual_register.t) =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_fiber_destroy")
        ~args:[ Simple_value Fiber_layout.current ]
    in
    switch_to_fiber
      ~fun_builder
      ~new_fiber:(Memory (Fiber_layout.parent Fiber_layout.current))
      ~operation:`Destroy_child;
    (* Run the return value handler, if there is one. *)
    let value_handler_result =
      match value_handler with
      | None -> inner_result
      | Some (arg, value_handler_expr) ->
        Function_builder.add_local fun_builder arg inner_result;
        codegen_expr t value_handler_expr ~fun_builder
    in
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
         ; src = value_handler_result
         });
    (* Return back up the stack of handler calls. *)
    Function_builder.add_terminal fun_builder Ret;
    (* Generate the effect handlers. *)
    let continuation = Function_builder.pick_register fun_builder in
    Nonempty.iteri effect_handlers ~f:(fun i { effect_op = _; args; resume; handler } ->
      (* FIXME: I think the codegen is maybe gonna do something dodgy with re-using
         registers before the [handle] and in the handler branches. e.g. it might confuse
         whatever rax, rbx, etc. previously stored with the new values loaded here. *)
      Function_builder.position_at_label fun_builder effect_handler_labels.(i);
      retrieve_arguments
        ~fun_builder
        ~args:(resume :: Nonempty.to_list args)
        ~call_conv:Umber;
      let handler_value = codegen_expr t handler ~fun_builder in
      Function_builder.add_code
        fun_builder
        (Mov
           { dst = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
           ; src = handler_value
           });
      Function_builder.add_code
        fun_builder
        (Mov
           { dst = continuation
           ; src =
               Function_builder.find_local fun_builder resume
               |> Option.value_exn ~here:[%here]
           });
      Function_builder.add_terminal fun_builder Ret);
    (* FIXME: Stack variables are invalidated after this point, because rsp has changed.
       How can we get the codegen to understand this? Technically the stack variables are
       still accessible, we just need to find the old rsp. Maybe we could put in a special
       instruction to make the code load from a parent fiber? Or to not hold variables in
       the stack across this point?
       
       Oh, we should treat creating a new fiber like calling a function!
       - Have a set calling convention for passing arguments (?)
       - That means we'd have to identify all the variables needed to pass...
       - Variables passed on the stack would be a bit tricky - I guess we could copy them
         to the new fiber stack? Or in the child fiber, we could rewrite the memory
         accesses to first load the parent's rsp (seems maybe better)
       - A regular call wouldn't quite work though, since we need to put the return address
         on the new stack.

       So, Handle_effects: [requires seeing all captured variables]
       - Allocate new fiber
       - Set fiber's parent to this fiber
       - Set up arguments for function call
       - Switch to new fiber (save rsp, set r14, set rsp)
       - Call inner expr ()
       - In the inner expr, when loading stack arguments, first load from the parent's
         saved rsp
       - (After inner expr return) destroy the fiber, switch to parent (load rsp, set r14)

       Normal return:
       - Normal function return leads back to the correct place

       One problem: shouldn't the return address change if you call the continuation from
       somewhere else?
       - Maybe we need to edit the return address when reparenting?
       - Hmm, the resume wrapper could edit it? That's the place that should be returned to
       - reparenting could do it if we pass in a label to return to
     *)
    (* FIXME: All this messing around with rsp doesn't work properly. e.g. we do
       `sub rsp, 8` at function start, then save/load it when switching fibers, then
       `add rsp, 8` at function end. This doesn't sync up, and it makes the stack get
       kinda messed up and then `ret` jumps to somewhere garbage. Need to properly think
       about how this should work.
       
       The rsp we save needs to be an appropriate place to reload from. One thing is we
       need to ensure proper 16-byte alignment. That's relatively easy. The main problem
       is that we maintain the invariant by adding to rsp at function end. If we switch to
       another fiber I think we also need to reset that state - we aren't ever going to
       jump back to the same instruction location. 
       
       Ah, we should put the pc to return to on the fiber stack so ret between fibers works.
       *)
    (* After a handler expression returns, destroy the child fiber. *)
    Function_builder.position_at_label fun_builder effect_handlers_end_label;
    (* FIXME: To run the effect handler, we should have already switched to the parent
       fiber. Think about how perform should handle this - we don't actually know where
       the child fiber is to destroy it. Maybe load it out of the resumption? *)
    let child_fiber =
      load_block_field fun_builder continuation (Mir.Block_index.of_int 1)
    in
    let (_ : Virtual_register.t) =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_fiber_destroy")
        ~args:[ child_fiber ]
    in
    Function_builder.add_terminal
      fun_builder
      (Jump (Simple_value (Global (end_label, Other))));
    Function_builder.position_at_label fun_builder end_label;
    (* FIXME: Need to return back to somewhere dynamically. I guess we should codegen all
       the handlers like functions too, maybe? It's tricky to keep track of the stack
       state. We can't just push stuff on the stack because it's mess up local variable
       addressing. Although, I guess we could use the frame pointer for that! *)
    Simple_value (Register (Real (Call_conv.return_value_register Umber)))
  | Perform_effect { effect_op; args } ->
    (* Evaluate the arguments before doing anything. *)
    let args = Nonempty.map args ~f:(codegen_expr t ~fun_builder) in
    (* Search for a matching handler, and detach the list of fibers traversed. We mess
       with the stack here, so it's important that the following call doesn't use the
       stack for any other arguments. We need to subtract 16 to avoid misaligning the
       stack. (Needs to be 16-byte aligned for a C call.) *)
    Function_builder.add_code
      fun_builder
      (Sub
         { dst = Simple_value (Register (Real Rsp))
         ; src = Simple_value (Constant (Int 16))
         });
    let handler =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_find_handler")
        ~args:
          [ Simple_value Fiber_layout.current
          ; Simple_value (Constant (Int (Mir.Effect_op_id.to_int effect_op)))
          ; Simple_value (Register (Real Rsp))
          ]
    in
    let fiber_to_switch_to = Function_builder.pick_register fun_builder in
    Function_builder.add_code
      fun_builder
      (Mov { dst = fiber_to_switch_to; src = Memory (I64, Value (Register (Real Rsp))) });
    Function_builder.add_code
      fun_builder
      (Add
         { dst = Simple_value (Register (Real Rsp))
         ; src = Simple_value (Constant (Int 16))
         });
    (* Once a handler is found, put args in the arg registers and jump to it. We don't use
       a "Call" instruction since we don't want to put the return address on the stack. *)
    let continuation_label =
      Function_builder.create_label fun_builder "perform_effect.resume_here"
    in
    (* FIXME: continuations are first-class, and are coercible to functions. So they need
       closures. The closures can be constant though. 
       
       Another problem is that calling a continuation isn't the same as calling a regular
       function. Maybe we need wrapper functions.

       Alternatively, we could identify when [resume] escapes and only allocate the
       closure then. But that seems tricky.
       
       Ah, resume also needs a pointer to the source fiber - store that in the
       contiuation. This is dynamic so they need to be dynamically allocated and created.
       There should be some flag we set in the continuation to prevent it getting called
       again. (Panic in that case).

       I think we need a proper wrapper function to do the nontrivial resumption logic.
    *)
    let continuation =
      box
        t
        ~fun_builder
        ~tag:Cnstr_tag.continuation
        ~fields:
          [ Simple_value (Global (define_umber_resume_wrapper t, Other))
          ; Simple_value Fiber_layout.current
          ; Simple_value (Global (continuation_label, Other))
          ]
    in
    (* FIXME: Stack arguments won't work with the updated rsp. Maybe reg alloc could
       notice when we set rsp and then adjust later spills to look up the parent stack? *)
    move_values_for_call
      fun_builder
      ~call_conv:Umber
      ~args:(Simple_value (Register (Virtual continuation)) :: Nonempty.to_list args);
    switch_to_fiber ~fun_builder ~new_fiber:fiber_to_switch_to ~operation:`Normal_switch;
    (* FIXME: This value got allocated to a nonsense register and somehow the move was
       deleted. Very sus. Maybe coalescing went wrong? *)
    Function_builder.add_terminal
      fun_builder
      (Jump (Simple_value (Register (Virtual handler))));
    (* Set up the logic to run upon resuming. *)
    Function_builder.position_at_label fun_builder continuation_label;
    let output_register = Function_builder.pick_register fun_builder in
    Function_builder.add_code
      fun_builder
      (Mov
         { src = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
         ; dst = output_register
         });
    output_register
  | Resume _ ->
    (* FIXME: I don't think there's much point in [Resume] being special since it needs to
       be callable like a regular closure anyway. I guess maybe it lets the backend decide
       the representation? *)
    failwith "TODO: resume in asm"

and codegen_cond t cond ~fun_builder =
  (* TODO: This just does basic constant folding so we don't generate invalid instructions
     like `cmp 3, 3` when codegening `if True`. This wouldn't be strictly needed if we
     had some more sophisticated constant folding. *)
  let cmp (a : _ Value.t) (b : _ Value.t) =
    match a, b with
    | Simple_value (Constant a), Simple_value (Constant b) ->
      `Constant (Asm_literal.equal a b)
    | _ ->
      Function_builder.add_code fun_builder (Cmp (a, b));
      `Zero_flag
  in
  match cond with
  | Equals (expr, literal) ->
    let expr_value = codegen_expr t expr ~fun_builder in
    let literal_value = codegen_literal t literal in
    let load_expr ~expr_value =
      load_block_field fun_builder expr_value (Mir.Block_index.of_int 0)
    in
    let load_literal ~literal_value =
      load_block_field fun_builder literal_value (Mir.Block_index.of_int 0)
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
        cmp
          (load_mem_offset fun_builder value I16 0)
          (Simple_value (Constant (Int (Cnstr_tag.to_int tag)))))
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
  (* TODO: Having [codegen_cond] immediately add the code mutably makes this annoying. If
     it just returned the code to add this logic would be much more straightforward. *)
  match codegen_cond1 () with
  | `Constant false as constant -> constant
  | `Constant true -> codegen_cond2 ()
  | `Zero_flag ->
    (* FIXME: This is horrible because of the janky assumption that [else_] has to be the
     next basic block. Come up with a proper fix for that. Probably need a distinction
     between "abstract assembly" and the real assembly, with an explicit conversion process
     that could sort out details like this. *)
    let mid_label = Function_builder.create_label fun_builder "codegen_and.mid" in
    Function_builder.add_terminal
      fun_builder
      (Jump_if { cond = `Zero; then_ = cond2_label; else_ = mid_label });
    Function_builder.position_at_label fun_builder mid_label;
    Function_builder.add_terminal
      fun_builder
      (Jump (Simple_value (Global (end_label, Other))));
    Function_builder.position_at_label fun_builder cond2_label;
    let cond2_result = codegen_cond2 () in
    Function_builder.add_terminal
      fun_builder
      (Jump (Simple_value (Global (end_label, Other))));
    Function_builder.position_at_label fun_builder end_label;
    (match cond2_result with
     | `Constant false as constant ->
       (* TODO: The code we already generated for cond1 is wasted. For now, just
          unconditionally return false. *)
       constant
     | `Constant true | `Zero_flag -> `Zero_flag)

and define_function t ~fun_name ~args ~body =
  (* TODO: Need a function prelude for e.g. the frame pointer *)
  let call_conv : Call_conv.t = Umber in
  let fun_builder =
    match Hashtbl.find t.globals fun_name with
    | Some (This_file (Function fun_builder)) -> fun_builder
    | global ->
      compiler_bug
        [%message "Expected existing function builder" (global : Global.t option)]
  in
  retrieve_arguments ~fun_builder ~call_conv ~args;
  let return_value = codegen_expr t body ~fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = return_value
       ; dst = Simple_value (Register (Real (Call_conv.return_value_register call_conv)))
       });
  Function_builder.add_terminal fun_builder Ret

and define_sub_function t ~fun_builder ~args ~body format =
  ksprintf
    (fun fun_name ->
      let inner_label =
        Function_builder.create_label fun_builder ~global:true "%s" fun_name
      in
      let inner_fun_name = mir_name_of_label_name inner_label in
      let inner_fun_builder =
        Function_builder.create inner_label ~arity:(List.length args)
      in
      Hashtbl.add_exn
        t.globals
        ~key:inner_fun_name
        ~data:(This_file (Function inner_fun_builder));
      (* TODO: When handling stack arguments, this won't work correctly because it needs
         to use rsp relative to the parent fiber.*)
      define_function t ~fun_name:inner_fun_name ~args ~body;
      inner_label)
    format
;;

(* TODO: Share code with [define_function]? *)
(* TODO: Mir doesn't say whether we export a function or not. If we don't, this is isn't
   needed. A simple public/private distinction of "is this in the mli" should do. *)
(** Create a wrapper function for other files to use, if we export this extern. *)
let define_extern_wrapper_function ~fun_name ~extern_name ~arity =
  let outer_call_conv : Call_conv.t = Umber in
  let inner_call_conv : Call_conv.t = C in
  let fun_builder = Function_builder.create (Label_name.of_mir_name fun_name) ~arity in
  let args =
    Call_conv.arg_registers outer_call_conv
    |> Nonempty.to_list
    |> Fn.flip List.take arity
    |> List.map ~f:(fun reg ->
         let virtual_reg = Function_builder.pick_register fun_builder in
         Function_builder.add_code
           fun_builder
           (Mov { src = Simple_value (Register (Real reg)); dst = virtual_reg });
         virtual_reg)
  in
  (* FIXME: Handle using stack for args. *)
  (* (match result with
   | Same_length | Right_trailing _ -> ()
   | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack"); *)
  let return_value =
    codegen_fun_call_internal
      fun_builder
      ~fun_:(Simple_value (Global (Label_name.of_extern_name extern_name, Extern_proc)))
      ~call_conv:inner_call_conv
      ~args
  in
  Function_builder.add_code
    fun_builder
    (Mov
       { src = Simple_value (Register (Virtual return_value))
       ; dst =
           Simple_value
             (Register (Real (Call_conv.return_value_register outer_call_conv)))
       });
  Function_builder.add_terminal fun_builder Ret;
  fun_builder
;;

let preprocess_stmt t (stmt : Mir.Stmt.t) =
  match stmt with
  | Value_def (name, (_ : Mir.Expr.t)) ->
    (* TODO: We could recognize constant expressions in globals (and locals too) and not
       have to do runtime initialization, instead just keeping them in rodata. *)
    Hashtbl.add_exn
      t.globals
      ~key:name
      ~data:(This_file (Global_variable (Label_name.of_mir_name name)))
  | Fun_def { fun_name; args; body = _ } ->
    let fun_builder =
      Function_builder.create
        (Label_name.of_mir_name fun_name)
        ~arity:(Nonempty.length args)
    in
    Hashtbl.add_exn t.globals ~key:fun_name ~data:(This_file (Function fun_builder))
  | Fun_decl { name; arity } ->
    let label_name = Label_name.of_mir_name name in
    let global : Global.Other_file.t =
      if arity = 0
      then Global_variable label_name
      else Umber_function { label_name; arity }
    in
    Hashtbl.add_exn t.globals ~key:name ~data:(Other_file global)
  | Extern_decl { name; extern_name; arity } ->
    let label_name = Label_name.of_extern_name extern_name in
    if arity = 0
    then
      Hashtbl.add_exn t.globals ~key:name ~data:(Other_file (Global_variable label_name))
    else (
      let local_wrapper_function =
        define_extern_wrapper_function ~fun_name:name ~extern_name ~arity
      in
      let global : Global.Other_file.t =
        C_function
          { label_name; arity; local_wrapper_function = Some local_wrapper_function }
      in
      Hashtbl.add_exn t.globals ~key:name ~data:(Other_file global))
;;

let codegen_stmt t (stmt : Mir.Stmt.t) =
  match stmt with
  | Value_def (name, expr) ->
    (* TODO: We could recognize constant expressions in globals (and locals too) and not
       have to do runtime initialization, instead just keeping them in rodata. *)
    let name = Label_name.of_mir_name name in
    let expr_location = codegen_expr t expr ~fun_builder:t.main_function in
    Function_builder.add_code
      t.main_function
      (Mov { dst = Memory (I64, Value (Global (name, Other))); src = expr_location })
  | Fun_def { fun_name; args; body } ->
    define_function t ~fun_name ~args:(Nonempty.to_list args) ~body
  | Fun_decl _ | Extern_decl _ -> (* Already handled in the preprocessing step *) ()
;;

let main_function_name ~(module_path : Module_path.Absolute.t) =
  Label_name.of_string [%string "umber_main#%{module_path#Module_path}"]
;;

let convert_mir ~module_path mir =
  let t = create ~main_function_name:(main_function_name ~module_path) in
  List.iter mir ~f:(preprocess_stmt t);
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

let compile_entry_module_internal ~module_paths =
  let t = create ~main_function_name:(Label_name.of_string "main") in
  (* Set up the main fiber. Save the stack pointer as the address of the parent fiber. *)
  let main_fiber =
    declare_and_call_extern_c_function
      t
      ~fun_builder:t.main_function
      ~fun_name:(Label_name.of_string "umber_fiber_create")
      ~args:[ Simple_value (Register (Real Rsp)) ]
  in
  Function_builder.add_code
    t.main_function
    (Mov
       { dst = Simple_value Fiber_layout.current
       ; src = Simple_value (Register (Virtual main_fiber))
       });
  set_rsp_for_fiber_just_created ~fun_builder:t.main_function;
  ignore
    (declare_and_call_extern_c_function
       t
       ~fun_builder:t.main_function
       ~fun_name:(Label_name.of_string "umber_gc_init")
       ~args:[]
      : Virtual_register.t);
  (* Call the main functions of all linked modules. *)
  List.iter module_paths ~f:(fun module_path ->
    let fun_name = main_function_name ~module_path in
    Hashtbl.add_exn
      t.globals
      ~key:(mir_name_of_label_name fun_name)
      ~data:(Other_file (Umber_function { label_name = fun_name; arity = 0 }));
    Function_builder.add_code
      t.main_function
      (Call
         { fun_ = Simple_value (Global (fun_name, Extern_proc))
         ; call_conv = Umber
         ; arity = 0
         }));
  (* Clean up the main fiber and restore the stack pointer. *)
  Function_builder.add_code
    t.main_function
    (Mov
       { dst = Simple_value (Register (Real Rsp))
       ; src = Memory (Fiber_layout.parent Fiber_layout.current)
       });
  let (_ : Virtual_register.t) =
    declare_and_call_extern_c_function
      t
      ~fun_builder:t.main_function
      ~fun_name:(Label_name.of_string "umber_fiber_destroy")
      ~args:[ Simple_value Fiber_layout.current ]
  in
  (* Return 0 back to libc's _start. *)
  Function_builder.add_code
    t.main_function
    (Mov
       { dst = Simple_value (Register (Real (Call_conv.return_value_register C)))
       ; src = Simple_value (Constant (Int 0))
       });
  Function_builder.add_terminal t.main_function Ret;
  to_program t
;;

let compile_entry_module ~module_paths ~entry_file =
  compile_to_object_file
    (compile_entry_module_internal ~module_paths)
    ~output_file:entry_file
;;

let%expect_test "hello world" =
  let hello_world : Asm_program.t =
    { globals = [ { name = Label_name.of_string "main"; strength = `Strong } ]
    ; externs = [ Label_name.of_string "puts" ]
    ; text_section =
        [ { label = Label_name.of_string "main"
          ; code =
              [ Lea
                  { dst = Rdi
                  ; src = I64, Value (Global (Label_name.of_string "message", Other))
                  }
              ; Call
                  { fun_ =
                      Simple_value (Global (Label_name.of_string "puts", Extern_proc))
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
  let print_name = mir_name_of_label_name (Label_name.of_string "Std.Prelude.print") in
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
               global    Std.Prelude.print:weak
               global    string.210886959:weak
               extern    umber_print_endline

               section   .text

    umber_main#HelloWorld:
               push      rbp
               mov       rbp, rsp
               sub       rsp, 8
               mov       rdi, string.210886959
               call      umber_print_endline wrt ..plt
               mov       r9, rax
               mov       qword [HelloWorld.#binding.1], r9
               add       rsp, 8
               pop       rbp
               ret

    Std.Prelude.print:
               push      rbp
               mov       rbp, rsp
               sub       rsp, 8
               mov       r9, rax
               mov       rdi, r9
               call      umber_print_endline wrt ..plt
               add       rsp, 8
               pop       rbp
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

let%expect_test "entry module" =
  let prelude_module_path =
    Module_path.of_module_names_unchecked
      [ Module_name.of_string_unchecked "Std"; Module_name.of_string_unchecked "Prelude" ]
  in
  compile_entry_module_internal
    ~module_paths:
      [ prelude_module_path
      ; Module_path.of_module_names_unchecked [ Module_name.of_string_unchecked "Read" ]
      ]
  |> Asm_program.pp Format.std_formatter;
  [%expect
    {|
               default   rel
               global    main
               extern    umber_main#Std.Prelude
               extern    umber_fiber_destroy
               extern    umber_fiber_create
               extern    umber_gc_init
               extern    umber_main#Read

               section   .text

    main:
               push      rbp
               mov       rbp, rsp
               sub       rsp, 8
               mov       rdi, rsp
               call      umber_fiber_create wrt ..plt
               mov       r14, rax
               mov       r9, qword [r14 + 24]
               lea       rsp, byte [r14 + r9]
               call      umber_gc_init wrt ..plt
               call      umber_main#Std.Prelude wrt ..plt
               call      umber_main#Read wrt ..plt
               mov       rsp, qword [r14]
               mov       rdi, r14
               call      umber_fiber_destroy wrt ..plt
               mov       rax, 0
               add       rsp, 8
               pop       rbp
               ret |}]
;;
