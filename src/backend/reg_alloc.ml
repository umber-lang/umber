open! Core
open! Import

open struct
  open Asm_program
  module Label_name = Label_name
  module Call_conv = Call_conv
  module Simple_value = Simple_value
  module Memory = Memory
  module Value = Value
  module Register_op = Register_op
  module Instr = Instr
  module Basic_block = Basic_block
end

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
        if not (Register.equal reg1 reg2) then Interference_graph.add_edge graph reg1 reg2));
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
        (* TODO: Decide if it actually matters if we track this or not. If it does,
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
let coalesce_registers ~(basic_blocks : Register.t Basic_block.t list) ~interference_graph
  =
  let coalesced_registers = Register.Virtual.Table.create () in
  List.iter basic_blocks ~f:(fun { label = _; code; terminal = _ } ->
    List.iter code ~f:(fun instr ->
      match instr with
      | Mov
          { src = Simple_value (Register src_reg); dst = Simple_value (Register dst_reg) }
        ->
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
    (* FIXME: Not handling registers in terminal instrs *)
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
        (coalesced_registers : Register.t Register.Virtual.Table.t)
        (basic_blocks : Register.t Basic_block.t list)];
  basic_blocks
;;

let color_interference_graph
  ~interference_graph
  ~all_available_registers
  ~registers_not_to_spill
  =
  (* See https://web.eecs.umich.edu/~mahlke/courses/583f12/reading/chaitin82.pdf for
       a description of the algorithm we are roughly following. *)
  let k = Set.length all_available_registers in
  let vertex_states = Register.Virtual.Table.create () in
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
             if Hash_set.mem registers_not_to_spill virtual_reg
             then current_max
             else (
               eprint_s
                 [%here]
                 [%lazy_message
                   "Considering spilling"
                     (virtual_reg : Register.Virtual.t)
                     (current_max : (Register.Virtual.t * int) option)];
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
        [%lazy_message "Decided to spill" (reg_to_spill : Register.Virtual.t)];
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
    let coloring = Register.Virtual.Table.create () in
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
        |> Register.Real.Set.of_list
      in
      let color = Set.choose_exn (Set.diff all_available_registers neighbouring_colors) in
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
         let rbp : Register.t Simple_value.t = Register (Real Rbp) in
         reg, Memory.offset rbp I64 ~-(i + 1 + already_spilled_count))
    |> Register.Virtual.Map.of_alist_exn
  in
  (* FIXME: Not handling registers in terminal instrs *)
  List.map basic_blocks ~f:(fun { label; code; terminal } : _ Basic_block.t ->
    let code =
      List.concat_map code ~f:(fun instr ->
        let (spilled_uses, spilled_assignments), instr =
          Instr.Nonterminal.fold_map_args instr ~init:([], []) ~f:(fun acc value ~op ->
            Simple_value.fold_map_registers value ~init:acc ~f:(fun acc reg ->
              match reg with
              | Real _ -> acc, reg
              | Virtual vreg ->
                if Nonempty.mem newly_spilled_registers vreg ~equal:Register.Virtual.equal
                then (
                  let tmp : Register.t = Virtual (create_register ()) in
                  let spilled_uses, spilled_assignments = acc in
                  match op with
                  | Use -> ((vreg, tmp) :: spilled_uses, spilled_assignments), tmp
                  | Assignment -> (spilled_uses, (vreg, tmp) :: spilled_assignments), tmp
                  | Use_and_assignment ->
                    ((vreg, tmp) :: spilled_uses, (vreg, tmp) :: spilled_assignments), tmp)
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
         pushing the return address. But, it gets realigned by pushing the frame pointer.
         So, to ensure 16-byte alignment we need to allocate an even number of words. *)
    let n = if spilled_count mod 2 = 0 then spilled_count else spilled_count + 1 in
    8 * n
  in
  match basic_blocks with
  | [] -> compiler_bug [%message "Empty function"]
  | entry_bb :: all_but_entry ->
    let prologue : Register.Real.t Instr.Nonterminal.t list =
      [ Push (Simple_value (Register Rbp))
      ; Mov { dst = Simple_value (Register Rbp); src = Simple_value (Register Rsp) }
      ]
    in
    let epilogue : Register.Real.t Instr.Nonterminal.t list =
      [ Pop (Simple_value (Register Rbp)) ]
    in
    let prologue, epilogue =
      if stack_space = 0
      then prologue, epilogue
      else
        ( prologue
          @ [ Sub
                { dst = Simple_value (Register Rsp)
                ; src = Simple_value (Constant (Int stack_space))
                }
            ]
        , Add
            { dst = Simple_value (Register Rsp)
            ; src = Simple_value (Constant (Int stack_space))
            }
          :: epilogue )
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
    Call_conv.all_available_registers Umber |> Register.Real.Set.of_list
  in
  let try_find_coloring ~cfg ~basic_blocks ~registers_not_to_spill =
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
    (* TODO: It's a bit weird that we update the interference graph while coalescing,
         but then still need to re-create it from scratch later. This might just be
         expected. It seems to be necessary for correctness, but I forget why. *)
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
        ~registers_not_to_spill
    , basic_blocks )
  in
  let registers_not_to_spill = Register.Virtual.Hash_set.create () in
  let rec loop ~cfg ~basic_blocks ~already_spilled_count =
    let result, basic_blocks =
      try_find_coloring ~cfg ~basic_blocks ~registers_not_to_spill
    in
    match result with
    | Error newly_spilled_registers ->
      eprint_s
        [%here]
        [%lazy_message
          "Coloring failed, spilling registers"
            (newly_spilled_registers : Register.Virtual.t Nonempty.t)];
      let basic_blocks =
        update_code_to_spill_registers
          ~basic_blocks
          ~newly_spilled_registers
          ~already_spilled_count
          ~create_register:(fun () ->
          let tmp = Register.Virtual.Counter.next register_counter in
          Hash_set.add registers_not_to_spill tmp;
          tmp)
      in
      let already_spilled_count =
        already_spilled_count + Nonempty.length newly_spilled_registers
      in
      Nonempty.iter newly_spilled_registers ~f:(Hash_set.add registers_not_to_spill);
      loop ~cfg ~basic_blocks ~already_spilled_count
    | Ok coloring ->
      eprint_s
        [%here]
        [%lazy_message
          "Coloring succeeded" (coloring : Register.Real.t Register.Virtual.Table.t)];
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
