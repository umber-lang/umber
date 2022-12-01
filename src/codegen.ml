open Import
open Names

let ignore_value (_ : Llvm.llvalue) = ()

let data_layout_string =
  (* See https://llvm.org/docs/LangRef.html#data-layout *)
  "i32:64-i64:64-p:64:64-f64:64"
;;

module Value_table : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val parse : Llvm.llcontext -> string -> t
  val add : t -> Unique_name.t -> Llvm.llvalue -> unit
  val find : t -> kind:[ `Function | `Unknown ] -> Unique_name.t -> Llvm.llvalue
end = struct
  type t =
    { local : Llvm_sexp.llvalue Unique_name.Table.t
    ; existing : Llvm_sexp.llmodule option
    }
  [@@deriving sexp_of]

  let create () = { local = Unique_name.Table.create (); existing = None }

  let parse context ll_string =
    { local = Unique_name.Table.create ()
    ; existing =
        Some (Llvm_irreader.parse_ir context (Llvm.MemoryBuffer.of_string ll_string))
    }
  ;;

  let add ({ local; existing = _ } as t) name value =
    match Hashtbl.add local ~key:name ~data:value with
    | `Ok -> ()
    | `Duplicate ->
      compiler_bug
        [%message
          "Tried to add duplicate LLVM value definition" (name : Unique_name.t) (t : t)]
  ;;

  let find ({ local; existing } as t) ~(kind : [ `Function | `Unknown ]) name =
    match Hashtbl.find local name with
    | Some value -> value
    | None ->
      let result =
        let name_str = Unique_name.to_string name in
        let%bind.Option existing = existing in
        match Llvm.lookup_function name_str existing, kind with
        | (Some _ as value), _ ->
          (* FIXME: Do we need to emit a declaration when we hit this case or the below one? *)
          value
        | None, `Function -> None
        | None, _ -> Llvm.lookup_global name_str existing
      in
      Option.value_or_thunk result ~default:(fun () ->
        compiler_bug
          [%message "Failed to find LLVM value for name" (name : Unique_name.t) (t : t)])
  ;;
end

type t =
  { context : Llvm.llcontext
  ; builder : Llvm.llbuilder
  ; module_ : Llvm.llmodule
  ; values : Value_table.t
  ; literal_cache : Llvm.llvalue Literal.Table.t
  }

module Tag = struct
  (* let no_scan = 0x8000 *)
  let int = Mir.Cnstr.Tag.of_int 0x8001
  let char = Mir.Cnstr.Tag.of_int 0x8002
  let float = Mir.Cnstr.Tag.of_int 0x8003
  let string = Mir.Cnstr.Tag.of_int 0x8004
end

let block_tag_type = Llvm.i16_type
let block_index_type = Llvm.i16_type

let with_type_memo t ~name ~f =
  Option.value_or_thunk (Llvm.type_by_name t.module_ name) ~default:f
;;

let block_header_type t =
  let name = "umber_header" in
  with_type_memo t ~name ~f:(fun () ->
    let typ = Llvm.named_struct_type t.context name in
    Llvm.struct_set_body
      typ
      [| block_tag_type t.context (* tag *)
       ; block_index_type t.context (* length *)
       ; Llvm.i32_type t.context (* padding *)
      |]
      false;
    typ)
;;

let block_type t =
  let name = "umber_block" in
  with_type_memo t ~name ~f:(fun () ->
    let block_type = Llvm.named_struct_type t.context name in
    Llvm.struct_set_body
      block_type
      [| block_header_type t; Llvm.array_type (Llvm.i64_type t.context) 0 |]
      false;
    block_type)
;;

(* TODO: If we use LLVM 15 it just has opaque pointers enabled by default, then we don't
   have to worry about pointer types. See https://llvm.org/docs/OpaquePointers.html. It
   looks like the OCaml bindings only support up to version 13, though. *)
let block_pointer_type = Llvm.pointer_type << block_type

let codegen_constant_tag t tag =
  (* Put the int63 into an int64 and make the bottom bit 1. *)
  let int63_value = Mir.Cnstr.Tag.to_int tag |> Int64.of_int in
  let int64_value = Int64.shift_left int63_value 1 |> Int64.( + ) Int64.one in
  (* FIXME: What is this bool? *)
  Llvm.const_of_int64 (Llvm.i64_type t.context) int64_value true
;;

let codegen_non_constant_tag t tag =
  Llvm.const_int (Llvm.i16_type t.context) (Mir.Cnstr.Tag.to_int tag)
;;

let codegen_block_len t len = Llvm.const_int (block_index_type t.context) len

let const_block_header t ~tag ~len =
  Llvm.const_named_struct
    (block_header_type t)
    [| codegen_non_constant_tag t tag; codegen_block_len t len |]
;;

let constant_block t ~tag ~len ~name ~str constant_value =
  let block_header = const_block_header t ~tag ~len in
  let value = Llvm.const_named_struct (block_type t) [| block_header; constant_value |] in
  let global_name = [%string "%{name}.%{str}"] in
  let global = Llvm.define_global global_name value t.module_ in
  Llvm.set_global_constant true global;
  global
;;

let codegen_literal t literal =
  Hashtbl.find_or_add t.literal_cache literal ~default:(fun () ->
    match literal with
    | Int i ->
      let type_ = Llvm.i64_type t.context in
      let str = Int.to_string i in
      constant_block t ~tag:Tag.int ~len:1 ~name:"int" ~str (Llvm.const_int type_ i)
    | Float x ->
      let type_ = Llvm.double_type t.context in
      let str = Float.to_string x in
      constant_block t ~tag:Tag.float ~len:1 ~name:"float" ~str (Llvm.const_float type_ x)
    | Char c ->
      let str = Uchar.to_string c in
      let c = Uchar.to_int c in
      let type_ = Llvm.i32_type t.context in
      constant_block t ~tag:Tag.char ~len:1 ~name:"char" ~str (Llvm.const_int type_ c)
    | String s ->
      (* FIXME: Strings will need handling of the final word, similar to how OCaml does it *)
      let s = Ustring.to_string s in
      let len = String.length s in
      let str = [%string "%{String.prefix s 10}.%{String.hash s#Int}"] in
      constant_block
        t
        ~tag:Tag.string
        ~len
        ~name:"string"
        ~str
        (Llvm.const_string t.context s))
;;

let get_block_tag t value =
  Llvm.build_gep value [| Llvm.const_int (block_index_type t.context) 0 |] "tag" t.builder
;;

let find_value t ~kind name =
  (* TODO: It would be nice to have something less hacky/special-cased for intrinsics,
     and just deal with them like any other module. *)
  let intrinsic_value =
    match Unique_name.base_name name |> Ustring.to_string |> String.lsplit2 ~on:'%' with
    | Some (_, "false") -> Some (Mir.Cnstr.Tag.of_int 0)
    | Some (_, "true") -> Some (Mir.Cnstr.Tag.of_int 1)
    | None | Some _ -> None
  in
  match intrinsic_value with
  | Some tag -> codegen_constant_tag t tag
  | None -> Value_table.find t.values ~kind name
;;

let ptr_to_int t value =
  Llvm.build_ptrtoint value (Llvm.i64_type t.context) (Llvm.value_name value) t.builder
;;

let codegen_expr t expr =
  let rec codegen_expr t expr =
    match (expr : Mir.Expr.t) with
    | Primitive lit -> codegen_literal t lit
    | Name name -> find_value t ~kind:`Unknown name
    | Let (name, expr, body) ->
      (* FIXME: I think mir needs to make it clear whether at least a fun_def is recursive,
       and maybe allow recursive lets too. (?) *)
      Value_table.add t.values name (codegen_expr t expr);
      codegen_expr t body
    | Fun_call (fun_name, args) ->
      let fun_ =
        let fun_value = find_value t ~kind:`Function fun_name in
        match Llvm.classify_value fun_value with
        | Function -> fun_value
        | Argument ->
          let arg_type = block_pointer_type t in
          let fun_pointer_type =
            Llvm.pointer_type
              (Llvm.function_type
                 arg_type
                 (Array.create arg_type ~len:(Nonempty.length args)))
          in
          let fun_name = Unique_name.to_string fun_name in
          Llvm.build_bitcast fun_value fun_pointer_type fun_name t.builder
        | value_kind ->
          compiler_bug
            [%message
              "Unexpected LLVM value kind for function"
                (fun_name : Unique_name.t)
                (value_kind : Llvm_sexp.Value_kind.t)]
      in
      let args = Array.of_list_map ~f:(codegen_expr t) (Nonempty.to_list args) in
      Llvm.build_call fun_ args "fun_call" t.builder
    | Make_block { tag; fields } ->
      if List.is_empty fields
      then codegen_constant_tag t tag
      else (
        let fields = List.map fields ~f:(codegen_expr t) in
        box t ~tag ~fields)
    | Get_block_field (i, expr) ->
      Llvm.const_gep
        (codegen_expr t expr)
        [| Llvm.const_int (Llvm.i16_type t.context) (Mir.Block_index.to_int i + 1) |]
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
      let phi_block = Llvm.append_block t.context "cond_merge" current_fun in
      Llvm.position_at_end phi_block t.builder;
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
             (Unique_name.to_string var)
             t.builder));
      let body_value = codegen_expr t body in
      let make_binding_block_and_br_to_phi bindings =
        let binding_block = Llvm.append_block t.context "cond_binding" current_fun in
        Llvm.position_at_end binding_block t.builder;
        let binding_values =
          List.map2_exn vars bindings ~f:(fun name expr ->
            let value = codegen_expr t expr in
            Llvm.set_value_name (Unique_name.to_string name) value;
            value)
        in
        ignore_value (Llvm.build_br phi_block t.builder);
        let binding_block_end = Llvm.insertion_block t.builder in
        let binding_vector = Llvm.const_vector (List.to_array binding_values) in
        Llvm.add_incoming (binding_vector, binding_block_end) phi_value;
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
      (* Have each condition block branch and break to either its binding block or the
         next condition block. The last condition block goes to the [if_none_matched]
         case, which either goes to another arbitrary expression, or runs the body with a
         final set of bindings. *)
      let rec associate_conds = function
        | [] -> ()
        | [ (last_cond_value, _, last_cond_block_end, last_binding_block) ] ->
          let if_none_matched_block =
            match if_none_matched with
            | Otherwise otherwise_expr ->
              let otherwise_block =
                Llvm.append_block t.context "cond_otherwise" current_fun
              in
              Llvm.position_at_end otherwise_block t.builder;
              ignore_value (codegen_expr t otherwise_expr);
              otherwise_block
            | Use_bindings bindings -> make_binding_block_and_br_to_phi bindings
          in
          Llvm.position_at_end last_cond_block_end t.builder;
          ignore_value
            (Llvm.build_cond_br
               last_cond_value
               last_binding_block
               if_none_matched_block
               t.builder)
        | (cond_value, _, current_cond_block_end, binding_block)
          :: (_, next_cond_block_start, _, _)
          :: _ as rest ->
          Llvm.position_at_end current_cond_block_end t.builder;
          ignore_value
            (Llvm.build_cond_br cond_value binding_block next_cond_block_start t.builder);
          associate_conds rest
      in
      associate_conds (Nonempty.to_list conds);
      body_value
  and codegen_cond t cond =
    let make_equals value value' =
      (* FIXME: This isn't going to work through pointers, yeah? Need to dereference/gep *)
      Llvm.build_icmp Eq (ptr_to_int t value) (ptr_to_int t value') "equals" t.builder
    in
    match cond with
    | Equals (expr, lit) ->
      (* FIXME: need to handle strings, chars, ints, floats, separately *)
      make_equals (codegen_expr t expr) (codegen_literal t lit)
    | Constant_tag_equals (expr, tag) ->
      make_equals (codegen_expr t expr) (codegen_constant_tag t tag)
    | Non_constant_tag_equals (expr, tag) ->
      make_equals (get_block_tag t (codegen_expr t expr)) (codegen_non_constant_tag t tag)
    | And _ -> failwith "TODO: And conditions"
  and box ?(tag = Mir.Cnstr.Tag.default) t ~fields =
    (* TODO: Use GC instead of leaking memory. For now, let's just try to plug in a
       conservative GC e.g. Boehm. *)
    (* FIXME: Abstract over allocating a struct and filling in a bunch of fields.
       We should also probably just generate one function that does this so the generated
       IR isn't completely unreadable. *)
    let block_field_num = List.length fields in
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
    ignore_value
      (Llvm.build_store (codegen_non_constant_tag t tag) heap_pointer_16 t.builder);
    ignore_value
      (let heap_pointer =
         Llvm.build_gep
           heap_pointer_16
           [| Llvm.const_int (Llvm.i64_type t.context) 1 |]
           "box"
           t.builder
       in
       Llvm.build_store (codegen_block_len t block_field_num) heap_pointer t.builder);
    List.iteri fields ~f:(fun i field_value ->
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
  in
  codegen_expr t expr
;;

let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    let value =
      Llvm.define_global (Unique_name.to_string name) (codegen_expr t expr) t.module_
    in
    Llvm.set_global_constant true value;
    Value_table.add t.values name value;
    value
  | Fun_def { fun_name; closed_over; args; body } ->
    if not (Set.is_empty closed_over)
    then raise_s [%message "TODO: closures" (closed_over : Unique_name.Set.t)];
    let type_ = block_pointer_type t in
    let fun_type =
      Llvm.function_type type_ (Array.create type_ ~len:(Nonempty.length args))
    in
    let fun_ = Llvm.define_function (Unique_name.to_string fun_name) fun_type t.module_ in
    let fun_params = Llvm.params fun_ in
    Nonempty.iteri args ~f:(fun i arg_name ->
      let arg_value = fun_params.(i) in
      Llvm.set_value_name (Unique_name.to_string arg_name) arg_value;
      Value_table.add t.values arg_name arg_value);
    let entry_block = Llvm.entry_block fun_ in
    Llvm.position_at_end entry_block t.builder;
    let return_value = codegen_expr t body in
    ignore_value (Llvm.build_ret return_value t.builder);
    (* TODO: see if we can use [verify_function] instead, so we don't abort on failure. *)
    (* FIXME: re-enable *)
    (* Llvm_analysis.assert_valid_function fun_; *)
    Value_table.add t.values fun_name fun_;
    fun_
;;

(* FIXME: remove *)
(* TODO: Do this as less of a hack. We should be able to treat the prelude basically like
   importing any other file. *)
(* NOTE: This function only works for the subset of the prelude we need. *)
(* let llvm_type_of_type_scheme t scheme =
  match (scheme : Type.Scheme.t) with
  | Type_app _ | Var _ | Tuple _ -> block_pointer_type t
  | Function (arg_types, _) ->
    let type_ = block_pointer_type t in
    Llvm.function_type type_ (Array.create type_ ~len:(Nonempty.length arg_types))
  | Partial_function _ -> .
;;

let add_prelude t =
  Name_bindings.fold_local_names
    (force Name_bindings.std_prelude)
    ~init:t
    ~f:(fun t name name_entry ->
    let name = Value_name.Qualified.to_ustring name in
    let extern_name =
      match Name_bindings.Name_entry.extern_name name_entry with
      | Some extern_name -> Extern_name.to_ustring extern_name
      | None -> name
    in
    let name = Unique_name.create name in
    match Name_bindings.Name_entry.scheme name_entry with
    | None ->
      compiler_bug
        [%message
          "Missing type scheme on Prelude name entry"
            (name_entry : Name_bindings.Name_entry.t)]
    | Some scheme ->
      let is_function =
        match scheme with
        | Function _ -> true
        | _ -> false
      in
      let type_ = llvm_type_of_type_scheme t scheme in
      let value =
        if is_function
        then Llvm.declare_function (Ustring.to_string extern_name) type_ t.module_
        else (
          let global =
            Llvm.declare_global type_ (Ustring.to_string extern_name) t.module_
          in
          Llvm.set_global_constant true global;
          global)
      in
      Value_table.add t.values name value;
      t)
;; *)

let create ~context ~source_filename ~values =
  let module_ =
    (* TODO: need to manually free modules and possibly other things: see e.g.
       `dispose_module` *)
    Llvm.create_module context source_filename
  in
  Llvm.set_data_layout data_layout_string module_;
  { context
  ; builder = Llvm.builder context
  ; module_
  ; values
  ; literal_cache = Literal.Table.create ()
  }
;;

let to_string t = Llvm.string_of_llmodule t.module_
let print t ~to_:file = Llvm.print_module file t.module_

let of_mir_exn ~context ~source_filename ~values mir =
  let t = create ~context ~source_filename ~values in
  List.iter mir ~f:(ignore_value << codegen_stmt t);
  match Llvm_analysis.verify_module t.module_ with
  | None -> t
  | Some error ->
    (* FIXME: remove writing to files *)
    Out_channel.write_all "/tmp/llvm_error.txt" ~data:error;
    Out_channel.write_all "/tmp/llvm_module.llvm" ~data:(to_string t);
    compiler_bug [%message "Llvm_analysis found invalid module" (error : string)]
;;

let of_mir ~context ~source_filename ~values mir =
  Compilation_error.try_with
    ~filename:source_filename
    Codegen_error
    ~msg:[%message "LLVM codegen failed"]
    (fun () -> of_mir_exn ~context ~source_filename ~values mir)
;;
