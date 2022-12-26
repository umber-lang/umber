open Import
open Names

let tailcc = 18

let data_layout_string =
  (* See https://llvm.org/docs/LangRef.html#data-layout *)
  "i32:64-i64:64-p:64:64-f64:64"
;;

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

module Tag = struct
  (* let no_scan = 0x8000 *)
  let int = Mir.Cnstr_tag.of_int 0x8001
  let char = Mir.Cnstr_tag.of_int 0x8002
  let float = Mir.Cnstr_tag.of_int 0x8003
  let string = Mir.Cnstr_tag.of_int 0x8004
end

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

let block_fields_type context ~len = Llvm.array_type (Llvm.i64_type context) len

let block_type ?(len = 0) t =
  let name = if len = 0 then "umber_block" else "umber_block" ^ Int.to_string len in
  with_type_memo t ~name ~f:(fun () ->
    let block_type = Llvm.named_struct_type t.context name in
    Llvm.struct_set_body
      block_type
      [| block_header_type t; block_fields_type t.context ~len |]
      false;
    block_type)
;;

(* TODO: If we use LLVM 15 it just has opaque pointers enabled by default, then we don't
   have to worry about pointer types. See https://llvm.org/docs/OpaquePointers.html. It
   looks like the OCaml bindings only support up to version 13, though. *)
let block_pointer_type = Llvm.pointer_type << block_type

let int_constant_tag t tag =
  (* Put the int63 into an int64 and make the bottom bit 1. *)
  let int63_value = Mir.Cnstr_tag.to_int tag |> Int64.of_int in
  let int64_value = Int64.shift_left int63_value 1 |> Int64.( + ) Int64.one in
  let is_signed = false in
  Llvm.const_of_int64 (Llvm.i64_type t.context) int64_value is_signed
;;

let codegen_constant_tag t tag =
  Llvm.const_inttoptr (int_constant_tag t tag) (block_pointer_type t)
;;

let int_non_constant_tag t tag =
  Llvm.const_int (Llvm.i16_type t.context) (Mir.Cnstr_tag.to_int tag)
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

let constant_block t ~tag ~len ~type_ ~name constant_value =
  let block_header = const_block_header t ~tag ~len in
  let value =
    Llvm.const_named_struct (block_type t ~len) [| block_header; constant_value |]
  in
  let global_name = [%string "%{type_}.%{name}"] in
  let global = Llvm.define_global global_name value t.module_ in
  Llvm.set_global_constant true global;
  Llvm.const_bitcast global (block_pointer_type t)
;;

let codegen_literal t literal =
  Hashtbl.find_or_add t.literal_cache literal ~default:(fun () ->
    match literal with
    | Int i ->
      let type_ = Llvm.i64_type t.context in
      let name = Int.to_string i in
      constant_block t ~tag:Tag.int ~len:1 ~type_:"int" ~name (Llvm.const_int type_ i)
    | Float x ->
      let type_ = Llvm.double_type t.context in
      let name = Float.to_string x in
      constant_block
        t
        ~tag:Tag.float
        ~len:1
        ~type_:"float"
        ~name
        (Llvm.const_float type_ x)
    | Char c ->
      let type_ = Llvm.i64_type t.context in
      let name = Uchar.to_string c in
      let c = Uchar.to_int c in
      constant_block t ~tag:Tag.char ~len:1 ~type_:"char" ~name (Llvm.const_int type_ c)
    | String s ->
      let s = Ustring.to_string s in
      let n_chars = String.length s in
      let name = String.hash s |> Int.to_string in
      let len = (n_chars / 8) + 1 in
      let packed_char_array =
        Array.init len ~f:(fun i ->
          let j = i * 8 in
          let get_char offset =
            let c =
              if j + offset < n_chars
              then Int64.of_int (Char.to_int s.[j + offset])
              else if offset = 7
              then (* Last byte *) Int64.of_int (7 - (n_chars % 8))
              else (* Padding null prefix of last word *) 0L
            in
            Int64.( lsl ) c ((7 - offset) * 8)
          in
          let int64_value = List.reduce_exn (List.init 8 ~f:get_char) ~f:Int64.( + ) in
          let is_signed = false in
          Llvm.const_of_int64 (Llvm.i64_type t.context) int64_value is_signed)
      in
      let value = Llvm.const_array (Llvm.i64_type t.context) packed_char_array in
      constant_block t ~tag:Tag.string ~len ~type_:"string" ~name value)
;;

let get_block_tag t value =
  Llvm.build_gep value [| Llvm.const_int (block_index_type t.context) 0 |] "tag" t.builder
;;

let ptr_to_int t value =
  Llvm.build_ptrtoint value (Llvm.i64_type t.context) (Llvm.value_name value) t.builder
;;

let rec codegen_expr t expr =
  match (expr : Mir.Expr.t) with
  | Primitive lit -> codegen_literal t lit
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
    let fun_ =
      let fun_value = Value_table.find t.values fun_name in
      match Llvm.classify_value fun_value with
      | Function -> fun_value
      | _ ->
        let arg_type = block_pointer_type t in
        let fun_pointer_type =
          Llvm.pointer_type
            (Llvm.function_type
               arg_type
               (Array.create arg_type ~len:(Nonempty.length args)))
        in
        let fun_name = Mir_name.to_string fun_name in
        Llvm.build_bitcast fun_value fun_pointer_type fun_name t.builder
    in
    let args = Array.of_list_map ~f:(codegen_expr t) (Nonempty.to_list args) in
    let call = Llvm.build_call fun_ args "fun_call" t.builder in
    Llvm.set_tail_call true call;
    call
  | Make_block { tag; fields } ->
    (match Nonempty.of_list fields with
     | None -> codegen_constant_tag t tag
     | Some fields ->
       let fields = Nonempty.map fields ~f:(codegen_expr t) in
       box t ~tag ~fields)
  | Get_block_field (i, expr) ->
    Llvm.build_gep
      (codegen_expr t expr)
      [| Llvm.const_int (Llvm.i16_type t.context) (Mir.Block_index.to_int i + 1) |]
      "block_field"
      t.builder
  | Cond_assign { vars; conds; body; if_none_matched } ->
    (* Problem: How do we assign multiple values conditionally? Phi nodes only
         accept one value. We can't use multiple phi blocks because you lose the
         predecessor information after the first one.
         Some possible approaches:
         1. Duplicate the conditions and re-check them to do each phi. This obviously
            sucks.
         2. Put the variables in a vector or array in the phi. Unclear what LLVM will do
            with this (stack allocation?) and whether that's better or worse than other
            options.
         3. Use `select` instead of `phi`. This means we always do all of the GEPs, etc.
            for each match arm. Maybe this is fine since they will always be simple/not
            side-effecting, so it might not cause (much) duplicated work.
            
        For now I've gone with (2). *)
    let start_block = Llvm.insertion_block t.builder in
    let current_fun = Llvm.block_parent start_block in
    let num_vars = List.length vars in
    (* Set up a phi block to receive the variable bindings as a vector. *)
    let phi_block = Llvm.append_block t.context "cond_binding_merge" current_fun in
    Llvm.position_at_end phi_block t.builder;
    let phi_value =
      if num_vars = 0
      then None
      else (
        let phi_value =
          Llvm.build_empty_phi
            (Llvm.vector_type (block_pointer_type t) num_vars)
            "cond_bindings"
            t.builder
        in
        (* Extract all the variables out of the vector in the phi. *)
        List.iteri vars ~f:(fun i var ->
          ignore_value
            (Llvm.build_extractelement
               phi_value
               (Llvm.const_int (Llvm.i64_type t.context) i)
               (Mir_name.to_string var)
               t.builder));
        Some phi_value)
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
      Option.iter phi_value ~f:(fun phi_value ->
        let binding_vector = Llvm.const_vector (List.to_array binding_values) in
        Llvm.add_incoming (binding_vector, binding_block_end) phi_value);
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
         next condition block. The last condition block goes to the [if_none_matched]
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

and codegen_cond t cond =
  let make_icmp value value' = Llvm.build_icmp Eq value value' "equals" t.builder in
  match cond with
  | Equals (expr, literal) ->
    let expr_value = codegen_expr t expr in
    let literal_value = codegen_literal t literal in
    let indexes = [| Llvm.const_int (Llvm.i64_type t.context) 1 |] in
    let build_gep value = Llvm.build_gep value indexes "equals_expr" t.builder in
    let const_gep value = Llvm.const_gep value indexes in
    (match literal with
     | Int _ | Char _ -> make_icmp (build_gep expr_value) (const_gep literal_value)
     | Float _ ->
       Llvm.build_fcmp
         Oeq
         (build_gep expr_value)
         (const_gep literal_value)
         "equals"
         t.builder
     | String _ -> failwith "TODO: string equality in patterns")
  | Constant_tag_equals (expr, tag) ->
    make_icmp (ptr_to_int t (codegen_expr t expr)) (int_constant_tag t tag)
  | Non_constant_tag_equals (expr, tag) ->
    make_icmp (get_block_tag t (codegen_expr t expr)) (int_non_constant_tag t tag)
  | And _ -> failwith "TODO: And conditions"

and box ?(tag = Mir.Cnstr_tag.default) t ~fields =
  (* TODO: Use GC instead of leaking memory. For now, let's just try to plug in a
       conservative GC e.g. Boehm. *)
  let block_field_num = Nonempty.length fields in
  let heap_pointer =
    Llvm.build_array_malloc
      (Llvm.i64_type t.context)
      (Llvm.const_int (Llvm.i32_type t.context) (block_field_num + 1))
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
         [| Llvm.const_int (Llvm.i64_type t.context) 1 |]
         "box"
         t.builder
     in
     Llvm.build_store (codegen_block_len t block_field_num) heap_pointer t.builder);
  Nonempty.iteri fields ~f:(fun i field_value ->
    ignore_value
      (let heap_pointer =
         Llvm.build_bitcast
           heap_pointer
           (Llvm.pointer_type (block_pointer_type t))
           "box"
           t.builder
       in
       let heap_pointer =
         Llvm.build_gep
           heap_pointer
           [| Llvm.const_int (Llvm.i64_type t.context) (i + 1) |]
           "box"
           t.builder
       in
       Llvm.build_store field_value heap_pointer t.builder));
  Llvm.build_bitcast heap_pointer (block_pointer_type t) "box" t.builder
;;

let preprocess_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, _) ->
    let global_value =
      Llvm.define_global
        (Mir_name.to_string name)
        (Llvm.const_null (block_pointer_type t))
        t.module_
    in
    Value_table.add t.values name global_value
  | Fun_def { fun_name; args; closed_over = _; body = _ } ->
    let type_ = block_pointer_type t in
    let fun_type =
      Llvm.function_type type_ (Array.create type_ ~len:(Nonempty.length args))
    in
    let fun_ = Llvm.define_function (Mir_name.to_string fun_name) fun_type t.module_ in
    Llvm.set_function_call_conv tailcc fun_;
    Value_table.add t.values fun_name fun_
  | Extern_decl { name; arity } ->
    let type_ = block_pointer_type t in
    let name_str = Mir_name.to_string name in
    let value =
      if arity = 0
      then Llvm.declare_global type_ name_str t.module_
      else
        Llvm.declare_function
          name_str
          (Llvm.function_type type_ (Array.create type_ ~len:arity))
          t.module_
    in
    Value_table.add t.values name value
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
  | Fun_def { fun_name; closed_over; args; body } ->
    if not (Set.is_empty closed_over)
    then raise_s [%message "TODO: closures" (closed_over : Mir_name.Set.t)];
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
  | Extern_decl _ -> (* Already handled in the preprocessing step *) ()
;;

let main_function_name ~source_filename = "main" ^ source_filename
let main_function_type context = Llvm.function_type (Llvm.i32_type context) [||]

let build_main_ret context builder =
  ignore_value (Llvm.build_ret (Llvm.const_int (Llvm.i32_type context) 0) builder)
;;

let create ~source_filename =
  let context = Llvm.create_context () in
  let module_ = Llvm.create_module context source_filename in
  Llvm.set_data_layout data_layout_string module_;
  let main_function_builder = Llvm.builder context in
  let main_function =
    Llvm.define_function
      (main_function_name ~source_filename)
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

let of_mir_exn ~source_filename mir =
  let t = create ~source_filename in
  List.iter mir ~f:(preprocess_stmt t);
  List.iter mir ~f:(codegen_stmt t);
  build_main_ret t.context t.main_function_builder;
  match Llvm_analysis.verify_module t.module_ with
  | None -> t
  | Some error ->
    compiler_bug
      [%message
        "Llvm_analysis found invalid module" (error : string) ~module_:(to_string t)]
;;

let of_mir ~source_filename mir =
  Compilation_error.try_with
    ~filename:source_filename
    Codegen_error
    ~msg:[%message "LLVM codegen failed"]
    (fun () -> of_mir_exn ~source_filename mir)
;;

let compile_to_object_and_dispose_internal module_ context ~output_file =
  Llvm_helpers.compile_module_to_object module_ ~output_file;
  Llvm.dispose_module module_;
  Llvm.dispose_context context
;;

let compile_to_object_and_dispose t ~output_file =
  compile_to_object_and_dispose_internal t.module_ t.context ~output_file
;;

let compile_entry_module ~source_filenames ~entry_file =
  let context = Llvm.create_context () in
  let module_ = Llvm.create_module context entry_file in
  let builder = Llvm.builder context in
  let fun_type = main_function_type context in
  let main_fun = Llvm.define_function "main" fun_type module_ in
  Llvm.position_at_end (Llvm.entry_block main_fun) builder;
  List.iter source_filenames ~f:(fun source_filename ->
    let fun_ =
      Llvm.declare_function (main_function_name ~source_filename) fun_type module_
    in
    ignore_value (Llvm.build_call fun_ [||] "" builder));
  build_main_ret context builder;
  compile_to_object_and_dispose_internal module_ context ~output_file:entry_file
;;
