open Import
open Names

module Name_entry = struct
  module Type_source = struct
    type t =
      | Placeholder
      | Val_declared
      | Let_inferred
    [@@deriving equal, sexp]
  end

  module Type_or_scheme = struct
    type t =
      | Type of Type.t
      | Scheme of Type.Scheme.t
    [@@deriving sexp]
  end

  type t =
    { typ : Type_or_scheme.t
    ; type_source : Type_source.t
         [@default Type_source.Val_declared] [@sexp_drop_default.equal]
    ; fixity : Fixity.t option [@sexp.option]
    ; extern_name : Extern_name.t option [@sexp.option]
    }
  [@@deriving sexp]

  let typ entry =
    match entry.typ with
    | Type typ -> typ
    | Scheme scheme -> Type.Scheme.instantiate ~map_name:Fn.id scheme
  ;;

  let let_inferred ?fixity ?extern_name typ =
    { type_source = Let_inferred; typ = Type typ; fixity; extern_name }
  ;;

  let val_declared ?fixity ?extern_name typ =
    { type_source = Val_declared; typ = Scheme typ; fixity; extern_name }
  ;;

  let placeholder typ =
    { type_source = Placeholder; typ = Type typ; fixity = None; extern_name = None }
  ;;
end

module Or_imported = struct
  type ('entry, 'name) t =
    | Local of 'entry
    | Imported of (Module_path.t * 'name)
  [@@deriving sexp, variants]
end

type t = Module_path.t * bindings

and bindings =
  { names : (Name_entry.t, Value_name.t) Or_imported.t Value_name.Map.t
  ; types : (Type.Decl.t, Type_name.t) Or_imported.t option Type_name.Map.t
  ; modules : bindings Module_name.Map.t
  }
[@@deriving sexp]

exception Name_error of Ustring.t [@@deriving sexp]

let name_error_msg str ustr =
  let str = str ^ ": " in
  raise (Name_error Ustring.(of_string_exn str ^ ustr))
;;

let name_error_path path = raise (Name_error (Module_path.to_ustring path))

let or_name_clash msg ustr = function
  | `Ok value -> value
  | `Duplicate -> name_error_msg msg ustr
;;

let empty_bindings =
  { names = Value_name.Map.empty
  ; types = Type_name.Map.empty
  ; modules = Module_name.Map.empty
  }
;;

let empty = [], empty_bindings

let add_to_types ?(err_msg = "Type name clash") types name decl =
  Map.update types name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error_msg err_msg (Type_name.to_ustring name))
;;

let update_at_path path bindings ~f =
  let rec loop bindings ~f path =
    match path with
    | [] -> f bindings
    | module_name :: rest ->
      { bindings with
        modules =
          Map.update bindings.modules module_name ~f:(function
            | Some bindings -> loop bindings ~f rest
            | None -> name_error_path path)
      }
  in
  path, loop bindings ~f path
;;

let update_current (path, bindings) ~f = update_at_path path bindings ~f

let into_module t module_name =
  update_current t ~f:(fun bindings ->
    { bindings with
      modules =
        Map.update bindings.modules module_name ~f:(Option.value ~default:empty_bindings)
    })
  |> Tuple2.map_fst ~f:(fun path -> path @ [ module_name ])
;;

let into_parent (path, t) = List.drop_last path |> Option.value ~default:[], t
let with_submodule t module_name ~f = fst t, snd (f (into_module t module_name))

let with_submodule' ((current_path, _) as t) module_name ~f =
  let (_, bindings), x = f (into_module t module_name) in
  (current_path, bindings), x
;;

let with_path (current_path, bindings) path ~f =
  let (_, bindings), x = f (path, bindings) in
  (current_path, bindings), x
;;

let core =
  ( []
  , { empty_bindings with
      types =
        List.fold
          ~init:empty_bindings.types
          ~f:(fun types (name, decl) -> Map.set types ~key:name ~data:(Some (Local decl)))
          Core.
            [ Bool.name, Bool.decl
            ; Int.name, Int.decl
            ; Float.name, Float.decl
            ; Char.name, Char.decl
            ; String.name, String.decl
            ]
    ; names =
        List.fold Core.Bool.cnstrs ~init:empty_bindings.names ~f:(fun names cnstr ->
          Map.set
            names
            ~key:(Value_name.of_cnstr_name cnstr)
            ~data:(Local (Name_entry.val_declared (Type.Concrete.cast Core.Bool.typ))))
    } )
;;

let merge_no_shadow t1 t2 =
  let err to_ustring ~key:name = name_error_msg "Name clash" (to_ustring name) in
  { names = Map.merge_skewed t1.names t2.names ~combine:(err Value_name.to_ustring)
  ; types = Map.merge_skewed t1.types t2.types ~combine:(err Type_name.to_ustring)
  ; modules = Map.merge_skewed t1.modules t2.modules ~combine:(err Module_name.to_ustring)
  }
;;

let rec current_bindings (current_path, bindings) =
  let open Option.Let_syntax in
  match current_path with
  | [] -> Some bindings
  | module_name :: path ->
    let%bind child_bindings = Map.find bindings.modules module_name in
    current_bindings (path, child_bindings)
;;

let current_bindings_exn t =
  option_or_default (current_bindings t) ~f:(fun () -> name_error_path (fst t))
;;

let rec find ((current_path, _) as t) ((path, name) as input) ~f ~to_ustring =
  (* Try looking at the current scope, then travel up to parent scopes to find a matching name *)
  let open Option.Let_syntax in
  let bindings_at_current = current_bindings_exn t in
  match path with
  | first_module :: path_rest ->
    (match Map.find bindings_at_current.modules first_module with
    | Some bindings ->
      option_or_default
        (let%bind bindings = current_bindings (path_rest, bindings) in
         f (current_path @ path) bindings name)
        ~f:(fun () -> raise (Name_error (to_ustring input)))
    | None -> check_parent t input ~f ~to_ustring)
  | [] ->
    option_or_default (f current_path bindings_at_current name) ~f:(fun () ->
      check_parent t input ~f ~to_ustring)

and check_parent (current_path, bindings) input ~f ~to_ustring =
  (* Recursively check the parent *)
  match List.drop_last current_path with
  | Some parent_path -> find (parent_path, bindings) input ~f ~to_ustring
  | None -> raise (Name_error (to_ustring input))
;;

let find_module t path =
  find
    t
    (path, ())
    ~f:(fun _ bindings () -> Some bindings)
    ~to_ustring:(fun (path, ()) -> Module_path.to_ustring path)
;;

let rec find_entry t =
  let open Option.Let_syntax in
  find t ~to_ustring:Value_name.Qualified.to_ustring ~f:(fun _ bindings name ->
    Map.find bindings.names name >>| resolve_name_or_import t)

and resolve_name_or_import t = function
  | Or_imported.Local entry -> entry
  | Imported path_name -> find_entry t path_name
;;

let find_type t name = find_entry t name |> Name_entry.typ
let find_cnstr_type t = Value_name.Qualified.of_cnstr_name >> find_type t
let find_fixity t name = Option.value (find_entry t name).fixity ~default:Fixity.default

let find_type_decl t name =
  let open Option.Let_syntax in
  find t name ~to_ustring:Type_name.Qualified.to_ustring ~f:(fun path bindings name ->
    match Map.find bindings.types name with
    | Some decl -> Some (path, decl)
    | None ->
      let module_name = Type_name.to_ustring name |> Module_name.of_ustring_unchecked in
      let%bind bindings = Map.find bindings.modules module_name in
      let%map decl = Map.find bindings.types name in
      path, decl)
;;

let absolutify_path t path =
  find
    t
    (path, ())
    ~f:(fun path _ () -> Some path)
    ~to_ustring:(fun (path, ()) -> Module_path.to_ustring path)
;;

let absolutify_type_name t ((_, name) as path) = fst (find_type_decl t path), name

let absolutify_value_name =
  find
    ~f:(fun path bindings name ->
      Option.some_if (Map.mem bindings.names name) (path, name))
    ~to_ustring:Value_name.Qualified.to_ustring
;;

(* TODO: how do I fill in foreign modules?
   For now, just assume a toplevel module already exists and copy (?) it into scope
   Later we can implement looking up new modules from the file system, installed packages, etc. *)
let import t module_name =
  let module_bindings = find_module t [ module_name ] in
  update_current t ~f:(fun bindings ->
    { bindings with
      modules =
        Map.add bindings.modules ~key:module_name ~data:module_bindings
        |> or_name_clash "Import of duplicate module" (Module_name.to_ustring module_name)
    })
;;

let filter bindings ~f =
  { names = Map.filter_keys bindings.names ~f:(f << Value_name.unidentify)
  ; types = Map.filter_keys bindings.types ~f:(f << Type_name.unidentify)
  ; modules = Map.filter_keys bindings.modules ~f:(f << Module_name.unidentify)
  }
;;

let import_filtered t path ~f =
  let filtered_bindings = filter ~f (find_module t path) in
  let rec map_to_imports path bindings =
    { names =
        Map.mapi bindings.names ~f:(fun ~key:name ~data:_ ->
          Or_imported.Imported (absolutify_path t path, name))
    ; types =
        Map.mapi bindings.types ~f:(fun ~key:name ~data:_ ->
          Some (Or_imported.Imported (absolutify_path t path, name)))
    ; modules =
        Map.mapi bindings.modules ~f:(fun ~key:module_name ~data:bindings ->
          map_to_imports (path @ [ module_name ]) bindings)
    }
  in
  update_current t ~f:(fun bindings ->
    merge_no_shadow bindings (map_to_imports path filtered_bindings))
;;

let import_all = import_filtered ~f:(fun _ -> true)

let import_with t path = function
  | [] -> import_all t path
  | imports -> import_filtered t path ~f:(List.mem imports ~equal:Unidentified_name.equal)
;;

let import_without t path hiding =
  import_filtered t path ~f:(not << List.mem hiding ~equal:Unidentified_name.equal)
;;

let map_type_expr_names type_expr ~f =
  Type.Expr.map type_expr ~f:(function
    | Type_app (name, args) -> `Defer (Type.Expr.Type_app (f name, args))
    | typ -> `Defer typ)
;;

let absolutify_type_expr t =
  map_type_expr_names ~f:(fun name -> absolutify_type_name t name)
;;

let std_prelude =
  lazy
    (let t = into_parent (t_of_sexp (Sexp.of_string Prelude.names_sexp)) in
     import_all t Core.prelude_module_path)
;;

let without_std =
  Tuple2.map_snd ~f:(fun bindings ->
    { bindings with modules = Map.remove bindings.modules Core.std_module_name })
;;

let add_val ?extern_name t name fixity (trait_bounds, type_expr) ~unify =
  update_current t ~f:(fun bindings ->
    if not (List.is_empty trait_bounds) then failwith "TODO: trait bounds in val";
    let scheme = absolutify_type_expr t type_expr in
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None | Some (Local { type_source = Val_declared | Let_inferred; _ }) ->
            compiler_bug [%message "Missing placeholder name entry" (name : Value_name.t)]
          | Some (Local ({ type_source = Placeholder; _ } as existing_entry)) ->
            unify (Type.Scheme.instantiate scheme) (Name_entry.typ existing_entry);
            Local { type_source = Val_declared; typ = Scheme scheme; fixity; extern_name }
          | Some (Imported imported_name) ->
            (* TODO: consider allowing this use case
             e.g. importing from another module, and then giving that import a new,
             compatible type declaration *)
            name_error_msg
              "Duplicate val for imported item"
              Ustring.(
                Value_name.to_ustring name
                ^ of_string_exn " vs "
                ^ Value_name.Qualified.to_ustring imported_name))
    })
;;

let absolutify_type_decl t = Type.Decl.map_exprs ~f:(absolutify_type_expr t)

let add_type_decl ((current_path, _) as t) type_name decl =
  update_current t ~f:(fun bindings ->
    if not (Type.Decl.no_free_params decl)
    then raise (Name_error (Ustring.of_string_exn "Free params in type decl"));
    let decl = absolutify_type_decl t decl in
    { bindings with
      types =
        add_to_types
          bindings.types
          type_name
          (Some (Local decl))
          ~err_msg:"Duplicate type declarations"
    ; names =
        (match decl with
        | params, Variants cnstrs ->
          (* Add constructors as functions to the namespace *)
          let result_type =
            Type.Expr.Type_app
              ((current_path, type_name), List.map params ~f:Type.Expr.var)
          in
          List.fold cnstrs ~init:bindings.names ~f:(fun names (cnstr_name, args) ->
            let entry =
              Name_entry.val_declared
                (List.fold_right args ~init:result_type ~f:Type.Expr.function_)
            in
            Map.add names ~key:(Value_name.of_cnstr_name cnstr_name) ~data:(Local entry)
            |> or_name_clash
                 "Variant constructor name clashes with another value"
                 (Cnstr_name.to_ustring cnstr_name))
        | _ -> bindings.names)
    })
;;

let set_scheme t name scheme =
  update_current t ~f:(fun bindings ->
    let entry =
      { Name_entry.type_source = Let_inferred
      ; typ = Scheme scheme
      ; fixity = None
      ; extern_name = None
      }
    in
    { bindings with names = Map.set bindings.names ~key:name ~data:(Local entry) })
;;

let add_fresh_var t name =
  let typ = Type.fresh_var () in
  ( update_current t ~f:(fun bindings ->
      { bindings with
        names =
          Map.add bindings.names ~key:name ~data:(Local (Name_entry.placeholder typ))
          |> or_name_clash "Duplicate name" (Value_name.to_ustring name)
      })
  , typ )
;;

let add_type_placeholder t type_name =
  update_current t ~f:(fun bindings ->
    { bindings with
      types = add_to_types bindings.types type_name None ~err_msg:"Duplicate type name"
    })
;;

let merge_names t new_names ~combine =
  let new_names = Map.map new_names ~f:Or_imported.local in
  update_current t ~f:(fun bindings ->
    { bindings with
      names =
        Map.merge_skewed bindings.names new_names ~combine:(fun ~key entry1 entry2 ->
          let entry1, entry2 =
            resolve_name_or_import t entry1, resolve_name_or_import t entry2
          in
          Local (combine key entry1 entry2))
    })
;;

let find_type_decl =
  let rec loop t name =
    match snd (find_type_decl t name) with
    | Some (Local decl) -> decl
    | Some (Imported path_name) -> loop t path_name
    | None -> compiler_bug [%message "Placeholder decl not replaced"]
  in
  loop
;;

let current_path = fst
