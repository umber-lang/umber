open! Core
open! Import

module Name_kind = struct
  type t =
    | Local
    | External of { arity : int }
    | Effect_op of Effect_op_id.t
    | Bool_intrinsic of { tag : Cnstr_tag.t }
  [@@deriving sexp_of]
end

type t =
  { expr_local_names : Mir_name.t Value_name.Absolute.Map.t
  ; toplevel_names : (Mir_name.t * Name_kind.t) Value_name.Absolute.Table.t
  ; module_path : Module_path.Absolute.t
  ; name_bindings : (Name_bindings.t[@sexp.opaque])
  ; name_table : (Mir_name.Name_table.t[@sexp.opaque])
  ; find_override :
      (Value_name.Absolute.t -> Mir_name.t -> Mir_name.t option[@sexp.opaque])
  }
[@@deriving sexp_of]

let with_module t module_name ~f =
  let name_bindings = Name_bindings.into_module t.name_bindings module_name ~place:`Def in
  let t, x = f { t with name_bindings } in
  { t with name_bindings = Name_bindings.into_parent name_bindings }, x
;;

let with_path_into_defs t path ~f =
  let name_bindings, result =
    Name_bindings.with_path_into_defs t.name_bindings path ~f:(fun name_bindings ->
      let t, result = f { t with name_bindings } in
      t.name_bindings, result)
  in
  { t with name_bindings }, result
;;

let current_path t = Name_bindings.current_path t.name_bindings
let copy_name t name = Mir_name.copy_name t.name_table name

let lookup_toplevel_name t ((path, _) as name) =
  let entry = Name_bindings.find_absolute_entry t.name_bindings name in
  let extern_name = Name_bindings.Name_entry.extern_name entry in
  let fallback_to_external () : Name_kind.t =
    let scheme =
      match Name_bindings.Name_entry.type_ entry with
      | Scheme scheme -> scheme
      | Type _ ->
        compiler_bug
          [%message
            "Didn't find type scheme for external name entry"
              (name : Value_name.Absolute.t)
              (entry : Name_bindings.Name_entry.t)]
    in
    External { arity = Type_utils.arity_of_type ~names:t.name_bindings (fst scheme) }
  in
  let name_kind : Name_kind.t =
    match extern_name with
    | None ->
      (* TODO: The thing we actually care about is "is this defined in the same file?"
           If we allow submodules to be defined in different files, this won't work.
           Also, being defined in the same file, and being `extern`, seem orthogonal. *)
      if Module_path.is_prefix path ~prefix:t.module_path
      then (
        match Name_bindings.Name_entry.type_source entry with
        | Effect_operation -> Effect_op (Effect_op_id.create ~effect_operation_name:name)
        | _ -> Local)
      else fallback_to_external ()
    | Some extern_name ->
      (* TODO: I don't think there's any good reason we need to special-case Bool in MIR
           It needs some special-case handling when type-checking `if` expressions, but
           other than that it's just a normal variant type. *)
      (match Extern_name.to_ustring extern_name |> Ustring.to_string with
       | "%false" -> Bool_intrinsic { tag = Cnstr_tag.of_int 0 }
       | "%true" -> Bool_intrinsic { tag = Cnstr_tag.of_int 1 }
       | _ -> fallback_to_external ())
  in
  let mir_name = Mir_name.create_exportable_name name in
  mir_name, name_kind
;;

let peek_toplevel_name_internal t name =
  try
    Some
      (Hashtbl.find_or_add t.toplevel_names name ~default:(fun () ->
         lookup_toplevel_name t name))
  with
  | Compilation_error.Compilation_error { kind = Name_error; _ } -> None
;;

let peek_value_name_internal t name : (Mir_name.t * Name_kind.t) option =
  match Map.find t.expr_local_names name with
  | Some mir_name -> Some (mir_name, Local)
  | None -> peek_toplevel_name_internal t name
;;

let add_value_name t name =
  let path = Name_bindings.current_path t.name_bindings in
  let name = path, name in
  let mir_name = Mir_name.create_value_name t.name_table name in
  ( { t with expr_local_names = Map.set t.expr_local_names ~key:name ~data:mir_name }
  , mir_name )
;;

let find_value_name t name : Mir_name.t * Name_kind.t =
  match peek_value_name_internal t name with
  | Some ((name', _) as entry) ->
    (match t.find_override name name' with
     | Some name_override -> name_override, Local
     | None -> entry)
  | None ->
    compiler_bug
      [%message "Name missing from context" (name : Value_name.Absolute.t) (t : t)]
;;

let peek_value_name t name =
  peek_value_name_internal t name
  |> Option.map ~f:(fun (mir_name, (_ : Name_kind.t)) ->
       t.find_override name mir_name |> Option.value ~default:mir_name)
;;

let find_value_name_assert_internal t name ~name_kind_matches ~expected =
  let name, name_kind =
    find_value_name t (Name_bindings.current_path t.name_bindings, name)
  in
  if not (name_kind_matches name_kind)
  then
    compiler_bug
      [%message
        "Unexpected name kind value"
          (name : Mir_name.t)
          (name_kind : Name_kind.t)
          (expected : string)];
  name
;;

let find_value_name_assert_local =
  find_value_name_assert_internal ~expected:"local" ~name_kind_matches:(function
    | Local | Bool_intrinsic _ | Effect_op _ -> true
    | External _ -> false)
;;

let find_value_name_assert_external =
  find_value_name_assert_internal ~expected:"external" ~name_kind_matches:(function
    | External _ | Bool_intrinsic _ -> true
    | Local | Effect_op _ -> false)
;;

let with_find_override t ~f =
  { t with
    find_override =
      (fun value_name mir_name ->
        let mir_name =
          t.find_override value_name mir_name |> Option.value ~default:mir_name
        in
        f value_name mir_name)
  }
;;

let create ~names:name_bindings ~name_table =
  { expr_local_names = Value_name.Absolute.Map.empty
  ; toplevel_names = Value_name.Absolute.Table.create ()
  ; module_path = Name_bindings.current_path name_bindings
  ; name_bindings
  ; name_table
  ; find_override = (fun _ _ -> None)
  }
;;

let cnstr_info_lookup_failed type_ =
  compiler_bug
    [%message
      "Constructor info lookup failed" (type_ : Module_path.absolute Type_scheme.type_)]
;;

let rec find_cnstr_info_internal t (type_ : Module_path.absolute Type_scheme.type_) =
  match type_ with
  | Type_app (type_name, _args) ->
    let decl =
      snd
        (Name_bindings.find_absolute_type_decl ~defs_only:true t.name_bindings type_name)
    in
    find_cnstr_info_from_decl t decl ~follow_aliases:true
  | Tuple args -> Some (Cnstr_info.of_tuple (List.length args))
  | Function _ | Var _ -> cnstr_info_lookup_failed type_
  | Union _ | Intersection _ ->
    failwith "TODO: handle cnstr info lookup for union and intersection types"

and find_cnstr_info_from_decl t decl ~follow_aliases =
  match (decl : _ Type_decl.decl) with
  | Alias alias -> if follow_aliases then find_cnstr_info_internal t alias else None
  | Variants variants ->
    Some
      (Cnstr_info.of_variants
         (List.map variants ~f:(fun (cnstr_name, args) -> cnstr_name, List.length args)))
  | Abstract | Record _ -> None
;;

let find_cnstr_info t type_ =
  Option.value_or_thunk (find_cnstr_info_internal t type_) ~default:(fun () ->
    compiler_bug
      [%message
        "Constructor info lookup failed" (type_ : Module_path.absolute Type_scheme.type_)])
;;
