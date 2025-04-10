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
  module Register = Register
  module Value = Value
  module Instr = Instr
  module Instr_group = Instr_group
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
  let arg_registers t : Register.t Nonempty.t =
    match t with
    | C | Umber -> [ Rdi; Rsi; Rdx; Rcx; R8; R9 ]
  ;;

  let return_value_register : t -> Register.t = function
    | C | Umber -> Rax
  ;;

  let reserved_registers : t -> Register.t list = function
    | C | Umber -> [ (* Stack pointer *) Rsp; (* Frame pointer *) Rbp ]
  ;;
end

module Extern = struct
  type t =
    | C_function of Label_name.t
    | Umber_function
end

module Register_state = struct
  type t =
    | Used
    | Unused
end

module Function_builder : sig
  type t

  val create : Label_name.t -> t
  val add_code : t -> Instr.t -> unit

  (* TODO: Consider if this API makes any sense *)
  val move_values_for_call : t -> call_conv:Call_conv.t -> args:Value.t Nonempty.t -> unit
  val set_register_state : t -> Register.t -> Register_state.t -> unit
  val add_local : t -> Mir_name.t -> Value.t -> unit
  val find_local : t -> Mir_name.t -> Value.t option
  val position_at_label : t -> Label_name.t -> unit
  val move_if_needed : t -> Value.t -> target_reg:Register.t -> unit

  (* TODO: It's not efficient to just randomly pick registers to pick things in. e.g.
     sometimes you might want to return immediately so you'd like the value to be in rax.
     To properly do this I think you need to plan ahead a little? Maybe inject some
     information about the future? *)
  val pick_available_reg_or_stack : t -> Value.t
  val name : t -> Label_name.t
  val instr_groups : t -> Instr_group.t list
end = struct
  (* TODO: Represents a partial codegened function. We need to consider the state of each
     register. *)
  type t =
    { fun_name : Label_name.t
    ; register_states : Register_state.t Register.Table.t
    ; locals : Value.t Mir_name.Table.t
    ; code : Instr.t Queue.t Label_name.Hash_queue.t
    ; mutable current_label : Label_name.t
    }

  let name t = t.fun_name

  let create fun_name =
    let reserved_regisers = Call_conv.reserved_registers Umber in
    let t =
      { fun_name
      ; register_states =
          List.filter_map
            Register.all
            ~f:(fun reg : (Register.t * Register_state.t) option ->
            (* FIXME: callee saved registers must be marked Used *)
            if List.mem reserved_regisers reg ~equal:Register.equal
            then None
            else Some (reg, Unused))
          |> Register.Table.of_alist_exn
      ; locals = Mir_name.Table.create ()
      ; code = Label_name.Hash_queue.create ()
      ; current_label = fun_name
      }
    in
    Hash_queue.enqueue_back_exn t.code fun_name (Queue.create ());
    t
  ;;

  let add_code t instr =
    let instrs =
      match Hash_queue.lookup t.code t.current_label with
      | Some instrs -> instrs
      | None ->
        let instrs = Queue.create () in
        Hash_queue.enqueue_back_exn t.code t.current_label instrs;
        instrs
    in
    Queue.enqueue instrs instr
  ;;

  let set_register_state t reg state = Hashtbl.set t.register_states ~key:reg ~data:state
  let position_at_label t label_name = t.current_label <- label_name

  let add_local t name value =
    (* FIXME: This should update the register states, I think? Maybe that should be
       internal to this module though, so you can't forget to do it.
       
       PROBLEM: We can't notice when registers become unused. Some notion of lifetimes
       would be needed to notice this. Alternatively we can fall back on the stack. *)
    Hashtbl.add_exn t.locals ~key:name ~data:value;
    match value with
    | Register reg -> set_register_state t reg Used
    | Memory _ | Global _ | Constant _ -> ()
  ;;

  let find_local t name = Hashtbl.find t.locals name

  let pick_available_reg_or_stack t : Value.t =
    (* FIXME: Handle stack *)
    (* FIXME: You can't pick just any register I think. We need to reserve some of these
       e.g. stack pointer. *)
    let reg =
      Hashtbl.to_alist t.register_states
      |> List.find_map_exn ~f:(fun (register, state) ->
           match state with
           | Unused -> Some register
           | Used -> None)
    in
    Hashtbl.set t.register_states ~key:reg ~data:Used;
    Register reg
  ;;

  let move_values_for_call t ~call_conv ~(args : Value.t Nonempty.t) =
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
    (* Set all argument registers as used up-front so we don't try to use them as
       temporary registers. *)
    Array.iter arg_registers ~f:(fun reg -> set_register_state t reg Used);
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
      | Memory _ -> failwith "TODO: memory arg location")
  ;;

  let move_if_needed t (value : Value.t) ~target_reg =
    match value with
    | Register reg when Register.equal reg target_reg -> ()
    | _ -> add_code t (Mov { src = value; dst = Register target_reg })
  ;;

  let instr_groups t =
    Hash_queue.to_alist t.code
    |> List.map ~f:(fun (label, instrs) : Instr_group.t ->
         { label; instrs = Queue.to_list instrs })
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
      Function_builder.instr_groups main_function
      @ List.concat_map functions ~f:Function_builder.instr_groups
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

let int_constant_tag tag : Value.t =
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
let lookup_name_for_value t name ~fun_builder : Value.t =
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

let lookup_name_for_fun_call t name ~fun_builder : Value.t * Call_conv.t =
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
let codegen_fun_call t fun_name args ~fun_builder : Value.t =
  let fun_, call_conv = lookup_name_for_fun_call t fun_name ~fun_builder in
  Function_builder.move_values_for_call fun_builder ~call_conv ~args;
  Function_builder.add_code fun_builder (Call fun_);
  let output_register = Call_conv.return_value_register call_conv in
  Function_builder.set_register_state fun_builder output_register Used;
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

let codegen_literal t (literal : Literal.t) : Value.t =
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

(** Insert code such that all the values end up in the same place. *)
let merge_branches
  fun_builder
  (label_a, (value_a : Value.t))
  (label_b, (value_b : Value.t))
  =
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
  | Memory _, _ | Global _, _ | Constant _, _ ->
    raise_s
      [%message "TODO: merge_branches cases" (value_a : Value.t) (value_b : Value.t)]
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
      Function_builder.add_code fun_builder (Jmp body_label)
    in
    Nonempty.iteri conds ~f:(fun i (cond, var_exprs) ->
      if i <> 0 then Function_builder.position_at_label fun_builder (cond_label i);
      codegen_cond t cond ~fun_builder;
      let next_label =
        if i < n_conds then cond_label (i + 1) else if_none_matched_label
      in
      Function_builder.add_code fun_builder (Jnz next_label);
      (* If we didn't jump, the condition succeeded, so set the variables. *)
      assign_vars_and_jump_to_body var_exprs;
      Function_builder.add_code fun_builder (Jmp body_label));
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
         (if_none_matched_label, otherwise_value))

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
    Function_builder.add_code fun_builder (Jz is_block_label);
    (* If it's not a block, we want to "return false" (ZF = 0). This is already the case. *)
    Function_builder.add_code fun_builder (Jmp end_label);
    (* If it's a block, check the tag. *)
    Function_builder.position_at_label fun_builder is_block_label;
    Function_builder.add_code
      fun_builder
      (Cmp (Memory (I16, Value value), Constant (Int (Cnstr_tag.to_int tag))));
    Function_builder.position_at_label fun_builder end_label
  | And (cond1, cond2) ->
    codegen_cond t cond1 ~fun_builder;
    let cond1_result = Function_builder.pick_available_reg_or_stack fun_builder in
    Function_builder.add_code fun_builder (Setz cond1_result);
    codegen_cond t cond2 ~fun_builder;
    let cond2_result = Function_builder.pick_available_reg_or_stack fun_builder in
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
      Function_builder.add_local fun_builder arg_name (Register reg))
  in
  (match result with
   | Same_length | Right_trailing _ -> ()
   | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack");
  let return_value = codegen_expr t body ~fun_builder in
  Function_builder.move_if_needed
    fun_builder
    return_value
    ~target_reg:(Call_conv.return_value_register call_conv);
  Function_builder.add_code fun_builder Ret
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
  Function_builder.add_code t.main_function Ret;
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
  Function_builder.add_code t.main_function Ret;
  compile_to_object_file t ~output_file:entry_file
;;

let%expect_test "hello world" =
  let hello_world : Asm_program.t =
    { globals = [ { name = Label_name.of_string "main"; strength = `Strong } ]
    ; externs = [ Label_name.of_string "puts" ]
    ; text_section =
        [ { label = Label_name.of_string "main"
          ; instrs =
              [ Lea
                  { dst = Register Rdi
                  ; src =
                      Memory (I64, Value (Global (Label_name.of_string "message", Other)))
                  }
              ; Call (Global (Label_name.of_string "puts", Extern_proc))
              ; Ret
              ]
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
