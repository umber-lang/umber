open Import
open Names

(** See https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html *)
module Call_conv = struct
  let cc = 0
  let tailcc = 18
end

(** See https://llvm.org/docs/LangRef.html#data-layout *)
let data_layout_string = "i32:64-i64:64-p:64:64-f64:64"

let ignore_value (_ : Llvm.llvalue) = ()

module Value_table : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val add : t -> Mir_name.t -> Llvm.llvalue -> unit
  val find : t -> Mir_name.t -> Llvm.llvalue
end = struct
  type t = Llvm_sexp.llvalue Mir_name.Table.t [@@deriving sexp_of]

  let create () = Mir_name.Table.create ()

  let add t name value =
    match Hashtbl.add t ~key:name ~data:value with
    | `Ok -> ()
    | `Duplicate ->
      compiler_bug
        [%message
          "Tried to add duplicate LLVM value definition" (name : Mir_name.t) (t : t)]
  ;;

  let find t name =
    match Hashtbl.find t name with
    | Some value -> value
    | None ->
      compiler_bug
        [%message "Failed to find LLVM value for name" (name : Mir_name.t) (t : t)]
  ;;
end

type t =
  { context : Llvm.llcontext
  ; module_ : Llvm.llmodule
  ; builder : Llvm.llbuilder
  ; values : Value_table.t
  ; literal_cache : Llvm.llvalue Literal.Table.t
  ; main_function_builder : Llvm.llbuilder
  }

let to_string t = Llvm.string_of_llmodule t.module_
let print t ~to_:file = Llvm.print_module file t.module_
let block_tag_type = Llvm.i16_type
let block_index_type = Llvm.i16_type

let with_type_memo t ~name ~f =
  Option.value_or_thunk (Llvm.type_by_name t.module_ name) ~default:f
;;

let block_header_padding_type = Llvm.i32_type

let block_header_type t =
  let name = "umber_header" in
  with_type_memo t ~name ~f:(fun () ->
    let typ = Llvm.named_struct_type t.context name in
    Llvm.struct_set_body
      typ
      [| block_tag_type t.context
       ; block_index_type t.context
       ; block_header_padding_type t.context
      |]
      false;
    typ)
;;

let block_type ?(len = 0) ?(field_kind = `Word) t =
  let name =
    let prefix = if len = 0 then "umber_block" else "umber_block" ^ Int.to_string len in
    let suffix =
      match field_kind with
      | `Word -> ""
      | `Byte -> "b"
    in
    prefix ^ suffix
  in
  with_type_memo t ~name ~f:(fun () ->
    let block_type = Llvm.named_struct_type t.context name in
    let field_type =
      match field_kind with
      | `Word -> Llvm.i64_type t.context
      | `Byte -> Llvm.i8_type t.context
    in
    Llvm.struct_set_body
      block_type
      [| block_header_type t; Llvm.array_type field_type len |]
      false;
    block_type)
;;

(* TODO: If we use LLVM 15 it just has opaque pointers enabled by default, then we don't
   have to worry about pointer types. See https://llvm.org/docs/OpaquePointers.html. It
   looks like the OCaml bindings only support up to version 13, though. *)
let block_pointer_type = Llvm.pointer_type << block_type

let block_function_type t ~n_args =
  let arg_type = block_pointer_type t in
  Llvm.function_type arg_type (Array.create arg_type ~len:n_args)
;;

let int_constant_tag t tag =
  (* Put the int63 into an int64 and make the bottom bit 1. *)
  let int63_value = Cnstr_tag.to_int tag |> Int64.of_int in
  let int64_value = Int64.shift_left int63_value 1 |> Int64.( + ) Int64.one in
  let is_signed = false in
  Llvm.const_of_int64 (Llvm.i64_type t.context) int64_value is_signed
;;

let codegen_constant_tag t tag =
  Llvm.const_inttoptr (int_constant_tag t tag) (block_pointer_type t)
;;

let int_non_constant_tag t tag =
  Llvm.const_int (Llvm.i16_type t.context) (Cnstr_tag.to_int tag)
;;

let codegen_block_len t len = Llvm.const_int (block_index_type t.context) len

let const_block_header t ~tag ~len =
  Llvm.const_named_struct
    (block_header_type t)
    [| int_non_constant_tag t tag
     ; codegen_block_len t len
     ; Llvm.const_int (block_header_padding_type t.context) 0
    |]
;;

let singleton_array type_ value = Llvm.const_array type_ [| value |]

let constant_block t ~tag ~len ~type_ ~type_name ~name constant_value =
  let block_header = const_block_header t ~tag ~len in
  let value = Llvm.const_named_struct type_ [| block_header; constant_value |] in
  let global_name = [%string "%{type_name}.%{name}"] in
  let global = Llvm.define_global global_name value t.module_ in
  Llvm.set_global_constant true global;
  Llvm.set_linkage Link_once_odr global;
  global
;;

let codegen_literal t literal =
  Hashtbl.find_or_add t.literal_cache literal ~default:(fun () ->
    match literal with
    | Int i ->
      let type_ = Llvm.i64_type t.context in
      let name = Int.to_string i in
      constant_block
        t
        ~tag:Cnstr_tag.int
        ~len:1
        ~type_:(block_type t ~len:1)
        ~type_name:"int"
        ~name
        (singleton_array type_ (Llvm.const_int type_ i))
    | Float x ->
      let type_ = Llvm.double_type t.context in
      let name = Float.to_string x in
      constant_block
        t
        ~tag:Cnstr_tag.float
        ~len:1
        ~type_:(block_type t ~len:1)
        ~type_name:"float"
        ~name
        (singleton_array type_ (Llvm.const_float type_ x))
    | Char c ->
      (* TODO: We could just store Chars as immediate values, they are guaranteed to fit *)
      let type_ = Llvm.i64_type t.context in
      let name = Uchar.to_string c in
      let c = Uchar.to_int c in
      constant_block
        t
        ~tag:Cnstr_tag.char
        ~len:1
        ~type_:(block_type t ~len:1)
        ~type_name:"char"
        ~name
        (singleton_array type_ (Llvm.const_int type_ c))
    | String s ->
      let s = Ustring.to_string s in
      let n_chars = String.length s in
      let name = String.hash s |> Int.to_string in
      let n_words = (n_chars / 8) + 1 in
      let packed_char_array =
        Array.init (n_words * 8) ~f:(fun char_index ->
          let byte =
            if char_index < n_chars
            then Char.to_int s.[char_index]
            else if char_index = (n_words * 8) - 1
            then (* Last byte *) 7 - (n_chars % 8)
            else (* Padding null prefix of last word *) 0
          in
          Llvm.const_int (Llvm.i8_type t.context) byte)
      in
      let value = Llvm.const_array (Llvm.i8_type t.context) packed_char_array in
      constant_block
        t
        ~tag:Cnstr_tag.string
        ~len:n_words
        ~type_:(block_type t ~len:(n_words * 8) ~field_kind:`Byte)
        ~type_name:"string"
        ~name
        value)
;;

let ptr_to_int t value =
  Llvm.build_ptrtoint value (Llvm.i64_type t.context) (Llvm.value_name value) t.builder
;;

let check_value_is_block t value =
  (* Check if this value is a pointer to a block. Pointers always have bottom bit 0.
     This is done by checking (in C syntax) `(value & 1) == 0`. *)
  let i64 = Llvm.i64_type t.context in
  let masked_to_bottom_bit =
    Llvm.build_and (ptr_to_int t value) (Llvm.const_int i64 1) "bottom_bit" t.builder
  in
  Llvm.build_icmp
    Eq
    masked_to_bottom_bit
    (Llvm.const_int i64 0)
    "bottom_bit_set"
    t.builder
;;

let get_block_tag t value =
  (* The tag is at index 0 in the block header. *)
  let gep =
    let typ = Llvm.i32_type t.context in
    Llvm.build_gep
      value
      [| Llvm.const_int typ 0; Llvm.const_int typ 0; Llvm.const_int typ 0 |]
      "tag_gep"
      t.builder
  in
  Llvm.build_load gep "tag" t.builder
;;

let block_indexes_for_gep t ~field_index =
  let typ = Llvm.i32_type t.context in
  (* 0 indexes through the pointer. 1 gets the second element in the umber_block struct
     (the fields array). Then [field_index] gets the nth field of the block. We stick
     with i32 since that type is required for struct indexing. *)
  [| Llvm.const_int typ 0
   ; Llvm.const_int typ 1
   ; Llvm.const_int typ (Mir.Block_index.to_int field_index)
  |]
;;

let build_call ?(name = "fun_call") t fun_ args ~call_conv =
  let call = Llvm.build_call fun_ args name t.builder in
  Llvm.set_tail_call true call;
  Llvm.set_instruction_call_conv call_conv call;
  call
;;

let codegen_umber_apply_fun t ~n_args =
  let apply_fun_name = [%string "umber_apply%{n_args#Int}"] in
  match Llvm.lookup_function apply_fun_name t.module_ with
  | Some fun_ -> fun_
  | None ->
    let original_block = Option.try_with (fun () -> Llvm.insertion_block t.builder) in
    let apply_fun_type = block_function_type t ~n_args:(n_args + 1) in
    let apply_fun_value = Llvm.define_function apply_fun_name apply_fun_type t.module_ in
    (* We need to use the C calling convention so the Rust runtime can call these
       functions. *)
    Llvm.set_function_call_conv Call_conv.cc apply_fun_value;
    Llvm.set_linkage Link_once_odr apply_fun_value;
    let params = Llvm.params apply_fun_value in
    let calling_fun = params.(0) in
    let arg_values = Array.subo params ~pos:1 in
    (* If the pointer is on the heap, do a closure call. Otherwise, do a regular
       function call. *)
    let is_on_heap_fun =
      Llvm.declare_function
        "umber_gc_is_on_heap"
        (Llvm.function_type (Llvm.i1_type t.context) [| block_pointer_type t |])
        t.module_
    in
    let entry_block = Llvm.entry_block apply_fun_value in
    Llvm.position_at_end entry_block t.builder;
    let closure_call_block = Llvm.append_block t.context "closure_call" apply_fun_value in
    let regular_call_block = Llvm.append_block t.context "regular_call" apply_fun_value in
    let phi_block = Llvm.append_block t.context "call_phi" apply_fun_value in
    let is_on_heap_call =
      Llvm.build_call is_on_heap_fun [| calling_fun |] "is_on_heap" t.builder
    in
    ignore_value
      (Llvm.build_cond_br is_on_heap_call closure_call_block regular_call_block t.builder);
    Llvm.position_at_end closure_call_block t.builder;
    let closure_call =
      (* Get first value from env and call it, passing env in as the first argument. *)
      let closure_env = calling_fun in
      let calling_fun =
        let gep =
          Llvm.build_gep
            closure_env
            (block_indexes_for_gep t ~field_index:(Mir.Block_index.of_int 0))
            "closure_gep"
            t.builder
        in
        let load_i64 = Llvm.build_load gep "closure_gep_raw" t.builder in
        Llvm.build_inttoptr
          load_i64
          (Llvm.pointer_type (block_function_type t ~n_args:(n_args + 1)))
          "closure_fun"
          t.builder
      in
      build_call
        t
        calling_fun
        (Array.append [| closure_env |] arg_values)
        ~name:"closure_call"
        ~call_conv:Call_conv.tailcc
    in
    ignore_value (Llvm.build_br phi_block t.builder);
    Llvm.position_at_end regular_call_block t.builder;
    let regular_call =
      let fun_value =
        Llvm.build_bitcast
          calling_fun
          (Llvm.pointer_type (block_function_type t ~n_args))
          "calling_fun"
          t.builder
      in
      build_call t fun_value arg_values ~name:"regular_call" ~call_conv:Call_conv.tailcc
    in
    ignore_value (Llvm.build_br phi_block t.builder);
    Llvm.position_at_end phi_block t.builder;
    let phi =
      Llvm.build_phi
        [ closure_call, closure_call_block; regular_call, regular_call_block ]
        "call_phi"
        t.builder
    in
    ignore_value (Llvm.build_ret phi t.builder);
    Option.iter original_block ~f:(fun original_block ->
      Llvm.position_at_end original_block t.builder);
    apply_fun_value
;;

let rec codegen_expr t expr =
  match (expr : Mir.Expr.t) with
  | Primitive lit ->
    let literal = codegen_literal t lit in
    Llvm.const_bitcast literal (block_pointer_type t)
  | Name name ->
    let value = Value_table.find t.values name in
    (match Llvm.classify_value value with
     | Function -> Llvm.const_bitcast value (block_pointer_type t)
     | GlobalVariable -> Llvm.build_load value (Llvm.value_name value) t.builder
     | _ -> value)
  | Let (name, expr, body) ->
    Value_table.add t.values name (codegen_expr t expr);
    codegen_expr t body
  | Fun_call (fun_name, args) ->
    let args = Array.of_list_map ~f:(codegen_expr t) (Nonempty.to_list args) in
    let build_umber_apply_fun_call fun_value =
      let n_args = Array.length args in
      build_call
        t
        (codegen_umber_apply_fun t ~n_args)
        (Array.append [| fun_value |] args)
        ~call_conv:Call_conv.cc
    in
    let fun_value = Value_table.find t.values fun_name in
    (match Llvm.classify_value fun_value with
     | Function ->
       (* If this is a direct function call to a known function, just call the function. *)
       let call_conv = Llvm.function_call_conv fun_value in
       build_call t fun_value args ~call_conv
     | GlobalVariable ->
       (* If this is a call through a pointer, use [umber_applyN] to do the logic of
         checking if the pointer is to a function or closure. *)
       build_umber_apply_fun_call
         (Llvm.build_load fun_value (Llvm.value_name fun_value) t.builder)
     | _ -> build_umber_apply_fun_call fun_value)
  | Make_block { tag; fields } ->
    (match Nonempty.of_list fields with
     | None -> codegen_constant_tag t tag
     | Some fields ->
       let fields = Nonempty.map fields ~f:(codegen_expr t) in
       box t ~tag ~fields)
  | Get_block_field (field_index, expr) ->
    let gep =
      Llvm.build_gep
        (codegen_expr t expr)
        (block_indexes_for_gep t ~field_index)
        "block_field_gep"
        t.builder
    in
    let load_i64 = Llvm.build_load gep "block_field_raw" t.builder in
    Llvm.build_inttoptr load_i64 (block_pointer_type t) "block_field" t.builder
  | Cond_assign { vars; conds; body; if_none_matched } ->
    let start_block = Llvm.insertion_block t.builder in
    let current_fun = Llvm.block_parent start_block in
    (* Set up a block with phi instructions to receive the variable bindings. *)
    let phi_block = Llvm.append_block t.context "cond_binding_merge" current_fun in
    Llvm.position_at_end phi_block t.builder;
    let phi_values =
      List.map vars ~f:(fun name ->
        let phi_value =
          Llvm.build_empty_phi (block_pointer_type t) "cond_bindings" t.builder
        in
        Value_table.add t.values name phi_value;
        phi_value)
    in
    let body_value = codegen_expr t body in
    let body_block_end = Llvm.insertion_block t.builder in
    let make_binding_block_and_br_to_phi bindings =
      let binding_block = Llvm.append_block t.context "cond_binding" current_fun in
      Llvm.position_at_end binding_block t.builder;
      let binding_values =
        List.map2_exn vars bindings ~f:(fun name expr ->
          let value = codegen_expr t expr in
          Llvm.set_value_name (Mir_name.to_string name) value;
          value)
      in
      ignore_value (Llvm.build_br phi_block t.builder);
      let binding_block_end = Llvm.insertion_block t.builder in
      List.iter2_exn phi_values binding_values ~f:(fun phi_value binding_value ->
        Llvm.add_incoming (binding_value, binding_block_end) phi_value);
      binding_block
    in
    let conds =
      (* Set up condition and binding blocks. Have the bindings go to the phi block. *)
      Nonempty.map conds ~f:(fun (cond, bindings) ->
        let cond_block = Llvm.append_block t.context "cond" current_fun in
        Llvm.position_at_end cond_block t.builder;
        let cond_value = codegen_cond t cond in
        let cond_block_end = Llvm.insertion_block t.builder in
        let binding_block = make_binding_block_and_br_to_phi bindings in
        cond_value, cond_block, cond_block_end, binding_block)
    in
    (* Start at the first cond. *)
    Llvm.position_at_end start_block t.builder;
    ignore_value
      (let _, first_cond_block, _, _ = Nonempty.hd conds in
       Llvm.build_br first_cond_block t.builder);
    (* Have each condition block branch and break to either its binding block or the
       next condition block. The last condition block goes to the `if_none_matched`
       case, which either goes to another arbitrary expression, or runs the body with a
       final set of bindings. *)
    let rec associate_conds : _ Nonempty.t -> _ = function
      | [ (last_cond_value, _, last_cond_block_end, last_binding_block) ] ->
        let if_none_matched_block, final_value, final_block =
          match if_none_matched with
          | Otherwise otherwise_expr ->
            let otherwise_block =
              Llvm.append_block t.context "cond_otherwise" current_fun
            in
            Llvm.position_at_end otherwise_block t.builder;
            let otherwise_value = codegen_expr t otherwise_expr in
            let otherwise_block_end = Llvm.insertion_block t.builder in
            let otherwise_phi_block =
              Llvm.append_block t.context "cond_otherwise_merge" current_fun
            in
            Llvm.position_at_end body_block_end t.builder;
            ignore_value (Llvm.build_br otherwise_phi_block t.builder);
            Llvm.position_at_end otherwise_block_end t.builder;
            ignore_value (Llvm.build_br otherwise_phi_block t.builder);
            Llvm.position_at_end otherwise_phi_block t.builder;
            let otherwise_phi_value =
              Llvm.build_phi
                [ body_value, body_block_end; otherwise_value, otherwise_block_end ]
                "cond_otherwise_merge"
                t.builder
            in
            otherwise_block, otherwise_phi_value, otherwise_phi_block
          | Use_bindings bindings ->
            let actual_last_binding_block = make_binding_block_and_br_to_phi bindings in
            actual_last_binding_block, body_value, body_block_end
        in
        Llvm.position_at_end last_cond_block_end t.builder;
        ignore_value
          (Llvm.build_cond_br
             last_cond_value
             last_binding_block
             if_none_matched_block
             t.builder);
        Llvm.position_at_end final_block t.builder;
        final_value
      | (cond_value, _, current_cond_block_end, binding_block)
        :: ((_, next_cond_block_start, _, _) as next_cond_and_binding)
        :: rest ->
        Llvm.position_at_end current_cond_block_end t.builder;
        ignore_value
          (Llvm.build_cond_br cond_value binding_block next_cond_block_start t.builder);
        associate_conds (next_cond_and_binding :: rest)
    in
    associate_conds conds
  | Handle_effects _ | Perform_effect _ -> failwith "TODO: effects in llvm"

and codegen_cond t cond =
  let make_icmp value value' = Llvm.build_icmp Eq value value' "equals" t.builder in
  match cond with
  | Equals (expr, literal) ->
    let expr_value = codegen_expr t expr in
    let literal_value = codegen_literal t literal in
    let indexes = block_indexes_for_gep t ~field_index:(Mir.Block_index.of_int 0) in
    let load_expr ~expr_value =
      let expr_gep = Llvm.build_gep expr_value indexes "equals_expr_gep" t.builder in
      Llvm.build_load expr_gep "equals_expr" t.builder
    in
    let load_literal ~literal_value =
      let literal_gep = Llvm.const_gep literal_value indexes in
      Llvm.build_load literal_gep "equals_literal" t.builder
    in
    (match literal with
     | Int _ | Char _ -> make_icmp (load_expr ~expr_value) (load_literal ~literal_value)
     | Float _ ->
       Llvm.build_fcmp
         Oeq
         (load_expr ~expr_value)
         (load_literal ~literal_value)
         "equals"
         t.builder
     | String _ -> failwith "TODO: string equality in patterns")
  | Constant_tag_equals (expr, tag) ->
    make_icmp (ptr_to_int t (codegen_expr t expr)) (int_constant_tag t tag)
  | Non_constant_tag_equals (expr, tag) ->
    let value = codegen_expr t expr in
    let is_block = check_value_is_block t value in
    let check_block_tag =
      make_icmp (get_block_tag t value) (int_non_constant_tag t tag)
    in
    Llvm.build_select
      is_block
      check_block_tag
      (Llvm.const_int (Llvm.i1_type t.context) 0)
      "non_constant_tag_equals"
      t.builder
  | And (cond1, cond2) ->
    Llvm.build_and (codegen_cond t cond1) (codegen_cond t cond2) "cond_and" t.builder

and box t ~tag ~fields =
  let block_field_num = Nonempty.length fields in
  let heap_pointer =
    let alloc_fun =
      Llvm.declare_function
        "umber_gc_alloc"
        (Llvm.function_type
           (Llvm.pointer_type (Llvm.i64_type t.context))
           [| Llvm.i64_type t.context |])
        t.module_
    in
    Llvm.build_call
      alloc_fun
      [| Llvm.const_int (Llvm.i64_type t.context) (8 * (block_field_num + 1)) |]
      "box"
      t.builder
  in
  let heap_pointer_16 =
    Llvm.build_bitcast
      heap_pointer
      (Llvm.pointer_type (Llvm.i16_type t.context))
      "box"
      t.builder
  in
  ignore_value (Llvm.build_store (int_non_constant_tag t tag) heap_pointer_16 t.builder);
  ignore_value
    (let heap_pointer =
       Llvm.build_gep
         heap_pointer_16
         [| Llvm.const_int (Llvm.i32_type t.context) 1 |]
         "box"
         t.builder
     in
     Llvm.build_store (codegen_block_len t block_field_num) heap_pointer t.builder);
  let heap_pointer =
    Llvm.build_bitcast
      heap_pointer
      (Llvm.pointer_type (block_pointer_type t))
      "box"
      t.builder
  in
  Nonempty.iteri fields ~f:(fun i field_value ->
    ignore_value
      (let heap_pointer =
         Llvm.build_gep
           heap_pointer
           [| Llvm.const_int (Llvm.i32_type t.context) (i + 1) |]
           "box"
           t.builder
       in
       Llvm.build_store field_value heap_pointer t.builder));
  Llvm.build_bitcast heap_pointer (block_pointer_type t) "box" t.builder
;;

let preprocess_stmt t stmt =
  let process_decl ~name ~arity ~extern_name =
    let name_str = Mir_name.to_string name in
    let value =
      if arity = 0
      then Llvm.declare_global (block_pointer_type t) name_str t.module_
      else (
        let fun_type = block_function_type t ~n_args:arity in
        let fun_ =
          match extern_name with
          | None ->
            (* This is an Umber function, just declare it. *)
            Llvm.declare_function name_str fun_type t.module_
          | Some extern_name ->
            (* This is an external C function. We need to wrap it with an Umber function
               to maintain the invariant that all function values must have the tailcc
               calling convention.  *)
            let fun_ = Llvm.define_function name_str fun_type t.module_ in
            Llvm.position_at_end (Llvm.entry_block fun_) t.builder;
            let external_fun =
              Llvm.declare_function
                (Extern_name.to_ustring extern_name |> Ustring.to_string)
                fun_type
                t.module_
            in
            let call =
              build_call t external_fun (Llvm.params fun_) ~call_conv:Llvm.CallConv.c
            in
            ignore_value (Llvm.build_ret call t.builder);
            fun_
        in
        Llvm.set_function_call_conv Call_conv.tailcc fun_;
        fun_)
    in
    Value_table.add t.values name value
  in
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, _) ->
    let global_value =
      Llvm.define_global
        (Mir_name.to_string name)
        (Llvm.const_null (block_pointer_type t))
        t.module_
    in
    Value_table.add t.values name global_value
  | Fun_def { fun_name; args; body = _ } ->
    let fun_type = block_function_type t ~n_args:(Nonempty.length args) in
    let fun_ = Llvm.define_function (Mir_name.to_string fun_name) fun_type t.module_ in
    Llvm.set_function_call_conv Call_conv.tailcc fun_;
    Value_table.add t.values fun_name fun_
  | Fun_decl { name; arity } -> process_decl ~name ~arity ~extern_name:None
  | Extern_decl { name; extern_name; arity } ->
    process_decl ~name ~arity ~extern_name:(Some extern_name)
;;

let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    let global_value = Value_table.find t.values name in
    Llvm.position_at_end (Llvm.insertion_block t.main_function_builder) t.builder;
    let expr_value = codegen_expr t expr in
    Llvm.position_at_end (Llvm.insertion_block t.builder) t.main_function_builder;
    if Llvm.is_constant expr_value
    then (
      Llvm.set_global_constant true global_value;
      Llvm.set_initializer expr_value global_value)
    else ignore_value (Llvm.build_store expr_value global_value t.main_function_builder)
  | Fun_def { fun_name; args; body } ->
    let fun_ = Value_table.find t.values fun_name in
    let fun_params = Llvm.params fun_ in
    Nonempty.iteri args ~f:(fun i arg_name ->
      let arg_value = fun_params.(i) in
      Llvm.set_value_name (Mir_name.to_string arg_name) arg_value;
      Value_table.add t.values arg_name arg_value);
    let entry_block = Llvm.entry_block fun_ in
    Llvm.position_at_end entry_block t.builder;
    let return_value = codegen_expr t body in
    ignore_value (Llvm.build_ret return_value t.builder);
    Llvm_analysis.assert_valid_function fun_
  | Fun_decl _ | Extern_decl _ -> (* Already handled in the preprocessing step *) ()
;;

let main_function_name ~(module_path : Module_path.Absolute.t) =
  [%string "umber_main:%{module_path#Module_path}"]
;;

let main_function_type context = Llvm.function_type (Llvm.i32_type context) [||]

let build_main_ret context builder =
  ignore_value (Llvm.build_ret (Llvm.const_int (Llvm.i32_type context) 0) builder)
;;

let create ~module_path ~source_filename =
  let context = Llvm.create_context () in
  let module_ = Llvm.create_module context source_filename in
  Llvm.set_data_layout data_layout_string module_;
  let main_function_builder = Llvm.builder context in
  let main_function =
    Llvm.define_function
      (main_function_name ~module_path)
      (main_function_type context)
      module_
  in
  Llvm.position_at_end (Llvm.entry_block main_function) main_function_builder;
  { context
  ; builder = Llvm.builder context
  ; module_
  ; values = Value_table.create ()
  ; literal_cache = Literal.Table.create ()
  ; main_function_builder
  }
;;

let codegen_runtime_required_functions t =
  (* Ensure that functions the runtime needs are codegened. See closure.rs in the runtime. *)
  ignore_value (codegen_umber_apply_fun t ~n_args:2)
;;

let of_mir_exn ~module_path ~source_filename mir =
  let t = create ~module_path ~source_filename in
  List.iter mir ~f:(preprocess_stmt t);
  List.iter mir ~f:(codegen_stmt t);
  build_main_ret t.context t.main_function_builder;
  codegen_runtime_required_functions t;
  match Llvm_analysis.verify_module t.module_ with
  | None -> t
  | Some error ->
    compiler_bug
      [%message
        "Llvm_analysis found invalid module" (error : string) ~module_:(to_string t)]
;;

let of_mir ~module_path ~source_filename mir =
  Compilation_error.try_with
    ~filename:source_filename
    Codegen_error
    ~msg:[%message "LLVM codegen failed"]
    (fun () -> of_mir_exn ~module_path ~source_filename mir)
;;

let compile_to_object_and_dispose_internal module_ context ~output_file =
  Llvm_helpers.compile_module_to_object module_ ~output_file;
  Llvm.dispose_module module_;
  Llvm.dispose_context context
;;

let compile_to_object_and_dispose t ~output_file =
  compile_to_object_and_dispose_internal t.module_ t.context ~output_file
;;

let compile_entry_module ~module_paths ~entry_file =
  let context = Llvm.create_context () in
  let module_ = Llvm.create_module context entry_file in
  let builder = Llvm.builder context in
  let fun_type = main_function_type context in
  let main_fun = Llvm.define_function "main" fun_type module_ in
  Llvm.position_at_end (Llvm.entry_block main_fun) builder;
  let gc_init_fun =
    Llvm.declare_function
      "umber_gc_init"
      (Llvm.function_type (Llvm.void_type context) [||])
      module_
  in
  ignore_value (Llvm.build_call gc_init_fun [||] "" builder);
  List.iter module_paths ~f:(fun module_path ->
    let fun_ = Llvm.declare_function (main_function_name ~module_path) fun_type module_ in
    ignore_value (Llvm.build_call fun_ [||] "" builder));
  build_main_ret context builder;
  compile_to_object_and_dispose_internal module_ context ~output_file:entry_file
;;
