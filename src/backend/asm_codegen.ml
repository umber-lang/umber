open! Core
open! Import

open struct
  open Asm_program
  module Asm_literal = Asm_literal
  module Call_conv = Call_conv
  module Data_decl = Data_decl
  module Global_decl = Global_decl
  module Label_name = Label_name
  module Memory = Memory
  module Simple_value = Simple_value
  module Size = Size
  module Value = Value
end

let mir_name_of_label_name label_name =
  Mir_name.create_exportable_name
    (Value_name.Absolute.of_relative_unchecked
       (Value_name.Relative.of_string (Label_name.to_string label_name)))
;;

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
  (* TODO: Handle using stack for args. *)
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

let load_mem_offset fun_builder (value : Register.t Value.t) size offset : _ Value.t =
  let simple_value =
    match value with
    | Simple_value v -> v
    | Memory mem ->
      Register (Virtual (Function_builder.move_to_new_register' fun_builder (Memory mem)))
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
  load_mem_offset fun_builder value I64 (Block_index.to_int index + 1)
;;

let check_value_is_block fun_builder value =
  (* Check if this value is a pointer to a block. Pointers always have bottom bit 0.
     This is done by checking (in C syntax) `(value & 1) == 0`. *)
  Function_builder.add_code fun_builder (Test (value, Simple_value (Constant (Int 1))));
  `Zero_flag
;;

(** Insert code such that all the values end up in the same place. *)
let merge_branches
  fun_builder
  (label_a, (value_a : _ Value.t))
  (label_b, (value_b : _ Value.t))
  ~merge_label
  =
  let value =
    match value_a, value_b with
    | Simple_value (Register a), Simple_value (Register b) when Register.equal a b ->
      value_a
    | ( Simple_value (Register _)
      , (Simple_value (Register _ | Global _ | Constant _) | Memory _) ) ->
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

let setup_stack_pointers_for_fiber_just_created ~fun_builder =
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
       });
  Function_builder.add_code
    fun_builder
    (Mov
       { dst = Simple_value (Register (Real Rbp))
       ; src = Simple_value (Register (Real Rsp))
       })
;;

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
    setup_stack_pointers_for_fiber_just_created ~fun_builder
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
           Function_builder.move_to_new_register
             fun_builder
             (Simple_value (Register (Real reg))))
    in
    let continuation, arg =
      match args with
      | [ continuation; arg ] -> continuation, arg
      | _ -> compiler_bug [%message "Resumptions should have 2 args"]
    in
    (* TODO: Centralize logic for continuation representation somewhere. *)
    let fiber_to_resume =
      load_block_field fun_builder continuation (Block_index.of_int 1)
    in
    let resume_address =
      load_block_field fun_builder continuation (Block_index.of_int 2)
    in
    let (_ : Register.Virtual.t) =
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
    switch_to_fiber ~fun_builder ~new_fiber:fiber_to_resume ~operation:`Normal_switch;
    Function_builder.add_code
      fun_builder
      (Mov
         { dst = Simple_value (Register (Real (Call_conv.return_value_register Umber)))
         ; src = arg
         });
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
    (* TODO: Need to add function prologues to check for stack overflow. At the moment I
       hacked around this by just making the default stack size massive :) *)
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
    let end_label = Function_builder.create_label fun_builder "handle_effects.end" in
    (* Make is so the value branch can return to the end label. *)
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
           ; src = Simple_value (Constant (Int (Effect_op_id.to_int effect_op)))
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
    (* At this point, since the inner function has returned a regular value, it won't be
       resumed again. Destroy the fiber and switch back to the parent fiber. *)
    let child_fiber =
      Function_builder.move_to_new_register
        fun_builder
        (Simple_value Fiber_layout.current)
    in
    switch_to_fiber
      ~fun_builder
      ~new_fiber:(Memory (Fiber_layout.parent Fiber_layout.current))
      ~operation:`Destroy_child;
    (* Now that we're back on the parent fiber, realign the stack and get ahold of the
       return value. *)
    Function_builder.add_code
      fun_builder
      (Sub
         { dst = Simple_value (Register (Real Rsp))
         ; src = Simple_value (Constant (Int 8))
         });
    let inner_result =
      Function_builder.move_to_new_register
        fun_builder
        (Simple_value (Register (Real (Call_conv.return_value_register Umber))))
    in
    let (_ : Register.Virtual.t) =
      declare_and_call_extern_c_function
        t
        ~fun_builder
        ~fun_name:(Label_name.of_string "umber_fiber_destroy")
        ~args:[ child_fiber ]
    in
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
    (* Undo the stack alignment adjustment from above. *)
    Function_builder.add_code
      fun_builder
      (Add
         { dst = Simple_value (Register (Real Rsp))
         ; src = Simple_value (Constant (Int 8))
         });
    (* Return back up the stack of handler calls. *)
    Function_builder.add_terminal fun_builder Ret;
    (* Generate the effect handlers. *)
    let continuation = Function_builder.pick_register fun_builder in
    Nonempty.iteri effect_handlers ~f:(fun i { effect_op = _; args; resume; handler } ->
      (* TODO: I think the only reason the codegen doesn't reuse registers for values
         before the handler is because all the registesr are clobbered by the call to the
         inner function above. This is very fragile. *)
      Function_builder.position_at_label fun_builder effect_handler_labels.(i);
      (* Now that we are back on the original fiber, realign the stack. *)
      Function_builder.add_code
        fun_builder
        (Sub
           { dst = Simple_value (Register (Real Rsp))
           ; src = Simple_value (Constant (Int 8))
           });
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
      (* Undo the stack alignment adjustment from above. *)
      Function_builder.add_code
        fun_builder
        (Add
           { dst = Simple_value (Register (Real Rsp))
           ; src = Simple_value (Constant (Int 8))
           });
      Function_builder.add_terminal fun_builder Ret);
    (* Finally, return the computed value. *)
    Function_builder.position_at_label fun_builder end_label;
    Function_builder.move_to_new_register
      fun_builder
      (Simple_value (Register (Real (Call_conv.return_value_register Umber))))
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
        ~fun_name:(Label_name.of_string "umber_fiber_find_handler_and_detach")
        ~args:
          [ Simple_value Fiber_layout.current
          ; Simple_value (Constant (Int (Effect_op_id.to_int effect_op)))
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
    (* TODO: Stack arguments won't work with the updated rsp. Maybe reg alloc could
       notice when we set rsp and then adjust later spills to look up the parent stack? *)
    move_values_for_call
      fun_builder
      ~call_conv:Umber
      ~args:(Simple_value (Register (Virtual continuation)) :: Nonempty.to_list args);
    switch_to_fiber ~fun_builder ~new_fiber:fiber_to_switch_to ~operation:`Normal_switch;
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
      load_block_field fun_builder expr_value (Block_index.of_int 0)
    in
    let load_literal ~literal_value =
      load_block_field fun_builder literal_value (Block_index.of_int 0)
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
       between "abstract assembly" and the real assembly, with an explicit conversion
       process that could sort out details like this. *)
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
  (* TODO: Handle using stack for args. *)
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
  setup_stack_pointers_for_fiber_just_created ~fun_builder:t.main_function;
  ignore
    (declare_and_call_extern_c_function
       t
       ~fun_builder:t.main_function
       ~fun_name:(Label_name.of_string "umber_gc_init")
       ~args:[]
      : Register.Virtual.t);
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
  let (_ : Register.Virtual.t) =
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
               mov       rdi, string.210886959
               call      umber_print_endline wrt ..plt
               mov       r9, rax
               mov       qword [HelloWorld.#binding.1], r9
               pop       rbp
               ret

    Std.Prelude.print:
               push      rbp
               mov       rbp, rsp
               mov       rdi, rax
               call      umber_print_endline wrt ..plt
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
               mov       rdi, rsp
               call      umber_fiber_create wrt ..plt
               mov       r14, rax
               mov       r9, qword [r14 + 24]
               lea       rsp, byte [r14 + r9]
               mov       rbp, rsp
               call      umber_gc_init wrt ..plt
               call      umber_main#Std.Prelude wrt ..plt
               call      umber_main#Read wrt ..plt
               mov       rsp, qword [r14]
               mov       rdi, r14
               call      umber_fiber_destroy wrt ..plt
               mov       rax, 0
               pop       rbp
               ret |}]
;;
