open! Core
open! Import

open struct
  open Asm_program
  module Basic_block = Basic_block
  module Instr = Instr
  module Label_name = Label_name
  module Memory = Memory
  module Value = Value
end

module Block_builder = struct
  type t =
    { code : Register.t Instr.Nonterminal.t Queue.t
    ; terminal : Register.t Instr.Terminal.t Set_once.t
    }
  [@@deriving sexp_of]

  let create () = { code = Queue.create (); terminal = Set_once.create () }
end

module Label_id = Unique_counter.Make ()

type t =
  { fun_name : Label_name.t
  ; arity : int
  ; label_counter : Label_id.Counter.t
  ; register_counter : Register.Virtual.Counter.t
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

let pick_register' t = Register.Virtual.Counter.next t.register_counter

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

let move_to_new_register t src =
  let dst = pick_register t in
  add_code t (Mov { dst; src });
  dst
;;

let move_to_new_register' t src =
  let dst = pick_register' t in
  add_code t (Mov { dst = Simple_value (Register (Virtual dst)); src });
  dst
;;

let create fun_name ~arity =
  let t =
    { fun_name
    ; arity
    ; label_counter = Label_id.Counter.create ()
    ; register_counter = Register.Virtual.Counter.create ()
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
