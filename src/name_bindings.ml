open Import
open Names

module Name_entry = struct
  module Type_source = struct
    type t =
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
  [@@deriving fields, sexp]

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
end

(* TODO: probably just make 'path the variable so we don't have to put unit for module paths *)
module Or_imported = struct
  type ('entry, 'name) t =
    | Local of 'entry
    | Imported of (Module_path.t * 'name)
  [@@deriving sexp, variants]
end

module Bindings_path = struct
  module T = struct
    type t = (Module_name.t * [ `Sig | `Def ]) list [@@deriving sexp]

    let to_string =
      let rec loop buf = function
        | [] -> Buffer.contents buf
        | (module_name, place) :: rest ->
          if Buffer.length buf > 0 then Buffer.add_char buf '.';
          Ustring.add_to_buffer buf (Module_name.to_ustring module_name);
          (match place with
          | `Sig -> Buffer.add_string buf "(s)"
          | `Def -> Buffer.add_string buf "(d)");
          loop buf rest
      in
      fun t ->
        let buf = Buffer.create (List.length t * 5) in
        loop buf t
    ;;

    let of_string =
      let open Option.Let_syntax in
      let rec lex_nonempty acc lexbuf =
        let%bind module_name =
          Lex_helpers.lex_upper_name lexbuf >>| Module_name.of_ustring_unchecked
        in
        let%bind place = Lex_helpers.lex_place lexbuf in
        let acc = (module_name, place) :: acc in
        match%sedlex lexbuf with
        | '.' -> lex_nonempty acc lexbuf
        | eof -> Some acc
        | _ -> None
      in
      function
      | "" -> []
      | str ->
        (match lex_nonempty [] (Sedlexing.Utf8.from_string str) with
        | Some path -> List.rev path
        | None -> failwith "Bindings_path.of_string: parse failed")
    ;;
  end

  include T
  include Sexpable.Of_stringable (T)

  let to_module_path = List.map ~f:fst
end

type t =
  { current_path : Bindings_path.t
  ; toplevel : defs
  }

and sigs = Nothing.t bindings

and defs = sigs bindings

(* TODO: may want to add a separate field for imports *)
and 'a bindings =
  { names : (Name_entry.t, Value_name.t) Or_imported.t Value_name.Map.t
  ; types : (Type.Decl.t, Type_name.t) Or_imported.t option Type_name.Map.t
  ; modules : ('a option * 'a bindings, unit) Or_imported.t Module_name.Map.t
  }
[@@deriving sexp]

module Sigs_or_defs = struct
  type t =
    | Sigs of sigs
    | Defs of defs
  [@@deriving sexp, variants]
end

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

let or_name_error_path x path = option_or_default x ~f:(fun () -> name_error_path path)

let empty_bindings =
  { names = Value_name.Map.empty
  ; types = Type_name.Map.empty
  ; modules = Module_name.Map.empty
  }
;;

let empty = { current_path = []; toplevel = empty_bindings }

(* TODO: I feel like we shouldn't have to do these conversions *)
(*let defs_of_sigs (sigs : sigs) : defs =
  { sigs with
    modules =
      Map.map sigs.modules ~f:(fun (None, sub_sigs) -> Some sub_sigs, empty_bindings)
  }
;;

let rec sigs_of_defs (defs : defs) : sigs =
  { defs with
    modules =
      Map.map defs.modules ~f:(fun (sigs, defs) ->
        None, option_or_default sigs ~f:(fun () -> sigs_of_defs defs))
  }
;;*)

let without_std t =
  { t with
    toplevel =
      { t.toplevel with modules = Map.remove t.toplevel.modules Core.std_module_name }
  }
;;

type f_bindings = { f : 'a. 'a bindings -> 'a bindings }

let update_current t ~f =
  let updating_import_err t (imported_module, ()) =
    compiler_bug
      [%message "Updating imported module" (imported_module : Module_path.t) (t : t)]
  in
  let rec loop_sigs t (sigs : sigs) path ~f =
    (* print_s [%message "loop_sigs" (path : Bindings_path.t) (sigs : sigs)]; *)
    match path with
    | [] -> f.f sigs
    | (_, `Def) :: _ -> compiler_bug [%message "`Def inside sig path" (t : t)]
    | (module_name, `Sig) :: rest ->
      { sigs with
        modules =
          Map.update sigs.modules module_name ~f:(function
            | Some (Local (None, sigs)) -> Local (None, loop_sigs t sigs rest ~f)
            | Some (Imported imported_module) -> updating_import_err t imported_module
            | None -> name_error_path (Bindings_path.to_module_path t.current_path)
            | Some (Local (Some _, _)) -> .)
      }
  in
  let rec loop_defs t defs path ~f =
    (* print_s [%message "loop_defs" (path : Bindings_path.t) (defs : defs)]; *)
    match path with
    | [] -> f.f defs
    | (module_name, place) :: rest ->
      { defs with
        modules =
          Map.update defs.modules module_name ~f:(function
            | Some (Local (sigs, defs)) ->
              (match place with
              | `Sig ->
                let sigs = Option.value sigs ~default:empty_bindings in
                Local (Some (loop_sigs t sigs rest ~f), defs)
              | `Def -> Local (sigs, loop_defs t defs rest ~f))
            | Some (Imported imported_module) -> updating_import_err t imported_module
            | None -> name_error_path (Bindings_path.to_module_path t.current_path))
      }
  in
  { t with toplevel = loop_defs t t.toplevel t.current_path ~f }
;;

let into_module t ~place module_name =
  let f bindings =
    { bindings with
      modules =
        Map.update
          bindings.modules
          module_name
          ~f:(Option.value ~default:(Or_imported.Local (None, empty_bindings)))
    }
  in
  let t = update_current t ~f:{ f } in
  { t with current_path = t.current_path @ [ module_name, place ] }
;;

let into_parent t =
  { t with current_path = List.drop_last t.current_path |> Option.value ~default:[] }
;;

let with_submodule t ~place module_name ~f =
  { (f (into_module t ~place module_name)) with current_path = t.current_path }
;;

let with_submodule' t ~place module_name ~f =
  let t', x = f (into_module ~place t module_name) in
  { t' with current_path = t.current_path }, x
;;

let core =
  { current_path = []
  ; toplevel =
      { empty_bindings with
        types =
          List.fold
            ~init:empty_bindings.types
            ~f:(fun types (name, decl) ->
              Map.set types ~key:name ~data:(Some (Local decl)))
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
      }
  }
;;

(* TODO: remove if unused *)
(*let merge_shadow t1 t2 =
  let shadow ~key:_ _ x = x in
  { names = Map.merge_skewed t1.names t2.names ~combine:shadow
  ; types = Map.merge_skewed t1.types t2.types ~combine:shadow
  ; modules = Map.merge_skewed t1.modules t2.modules ~combine:shadow
  }
;;*)

let merge_no_shadow t1 t2 =
  let err to_ustring ~key:name = name_error_msg "Name clash" (to_ustring name) in
  { names = Map.merge_skewed t1.names t2.names ~combine:(err Value_name.to_ustring)
  ; types = Map.merge_skewed t1.types t2.types ~combine:(err Type_name.to_ustring)
  ; modules = Map.merge_skewed t1.modules t2.modules ~combine:(err Module_name.to_ustring)
  }
;;

let rec resolve_path =
  let open Option.Let_syntax in
  let rec loop_sigs t acc_path path sigs =
    match path with
    | [] -> Some (Sigs_or_defs.Sigs sigs, List.rev acc_path)
    | module_name :: rest ->
      (match%bind Map.find sigs.modules module_name with
      | Local (None, sigs) -> loop_sigs t ((module_name, `Sig) :: acc_path) rest sigs
      | Local (Some _, _) -> .
      | Imported (path, ()) -> resolve_path t path)
  and loop_defs t acc_path current_path path defs =
    match path with
    | [] -> Some (Sigs_or_defs.Defs defs, List.rev acc_path)
    | module_name :: rest ->
      (match%bind Map.find defs.modules module_name with
      | Local (sigs, defs) ->
        let current_path, go_into =
          match current_path with
          | Some [] | None -> None, `Sig
          | Some ((module_name', place) :: rest') ->
            if Module_name.(module_name = module_name')
            then Some rest', place
            else None, `Sig
        in
        (match go_into, sigs with
        | `Sig, Some sigs -> loop_sigs t ((module_name, `Sig) :: acc_path) rest sigs
        | `Sig, None | `Def, _ ->
          loop_defs t ((module_name, `Def) :: acc_path) current_path rest defs)
      | Imported (path, ()) -> resolve_path t path)
  in
  fun t path -> loop_defs t [] (Some t.current_path) path t.toplevel
;;

let resolve_path_exn t path = or_name_error_path (resolve_path t path) path

let with_path t path ~f =
  let current_path = snd (resolve_path_exn t path) in
  let t', x = f { t with current_path } in
  { t' with current_path = t.current_path }, x
;;

let find =
  let rec loop ?at_path t ((path, name) as input) ~f ~to_ustring =
    (* Try looking at the current scope, then travel up to parent scopes to find a matching name *)
    let at_path =
      Option.value at_path ~default:(Bindings_path.to_module_path t.current_path)
    in
    let bindings_at_current = fst (resolve_path_exn t at_path) in
    (* let input' = to_ustring input in
  if Ustring.(input' = of_string_exn "Range.in")
  then
    print_s
      [%message
        "find"
          (to_ustring input : Ustring.t)
          (at_path : Module_path.t)
          (bindings_at_current : sigs_defs)]; *)
    match List.hd path with
    | Some first_module ->
      let full_path = at_path @ path in
      let f bindings =
        if Map.mem bindings.modules first_module
        then (
          let bindings = fst (or_name_error_path (resolve_path t full_path) at_path) in
          option_or_default (f full_path name bindings) ~f:(fun () ->
            raise (Name_error (to_ustring input))))
        else check_parent t at_path input ~f ~to_ustring
      in
      (match bindings_at_current with
      | Sigs sigs -> f sigs
      | Defs defs -> f defs)
    | None ->
      option_or_default (f at_path name bindings_at_current) ~f:(fun () ->
        check_parent t at_path input ~f ~to_ustring)
  and check_parent t current_path input ~f ~to_ustring =
    (* Recursively check the parent *)
    match List.drop_last current_path with
    | Some parent_path -> loop t ~at_path:parent_path input ~f ~to_ustring
    | None -> raise (Name_error (to_ustring input))
  in
  loop
;;

let rec find_entry t name =
  let entry =
    let open Option.Let_syntax in
    find t name ~to_ustring:Value_name.Qualified.to_ustring ~f:(fun _ name bindings ->
      let f bindings = Map.find bindings.names name >>| resolve_name_or_import t in
      match bindings with
      | Sigs sigs -> f sigs
      | Defs defs -> f defs)
  in
  (* print_s
    [%message
      "Found entry!"
        (name : Value_name.Qualified.t)
        (entry : Name_entry.t)
        (without_std t : t)]; *)
  entry

and resolve_name_or_import t = function
  | Or_imported.Local entry -> entry
  | Imported path_name -> find_entry t path_name
;;

let find_type t name = find_entry t name |> Name_entry.typ
let find_cnstr_type t = Value_name.Qualified.of_cnstr_name >> find_type t
let find_fixity t name = Option.value (find_entry t name).fixity ~default:Fixity.default

let find_type_decl' ?at_path t name =
  (* print_s
    [%message
      "find_type_decl'"
        (at_path : Module_path.t option)
        (name : Type_name.Qualified.t)
        (t : t)]; *)
  let open Option.Let_syntax in
  find
    ?at_path
    t
    name
    ~to_ustring:Type_name.Qualified.to_ustring
    ~f:(fun path name bindings ->
    (* print_s
      [%message
        "find_type_decl'.f"
          (path : Module_path.t)
          (name : Type_name.t)
          (bindings : Sigs_or_defs.t)]; *)
    let f bindings ~check_submodule =
      match Map.find bindings.types name with
      (* TODO: should we resolve the import here? *)
      | Some decl -> Some (path, decl)
      | None ->
        (* Allow type names like [List.List] to be found as just [List] *)
        let module_name = Type_name.to_ustring name |> Module_name.of_ustring_unchecked in
        Map.find bindings.modules module_name >>= check_submodule
    in
    match bindings with
    | Sigs sigs ->
      f sigs ~check_submodule:(function
        | Local (None, sigs) ->
          let%bind decl = Map.find sigs.types name in
          Some (path, decl)
        | Local (Some _, _) -> .
        | Imported (import_path, ()) -> Some (path, Some (Imported (import_path, name))))
    | Defs defs ->
      f defs ~check_submodule:(function
        | Local (None, defs) ->
          let%bind decl = Map.find defs.types name in
          Some (path, decl)
        | Local (Some sigs, _defs) ->
          let%bind decl = Map.find sigs.types name in
          Some (path, decl)
        | Imported (import_path, ()) -> Some (path, Some (Imported (import_path, name)))))
;;

let absolutify_path t path =
  find
    t
    (path, ())
    ~f:(fun path () _ -> Some path)
    ~to_ustring:(fun (path, ()) -> Module_path.to_ustring path)
;;

let absolutify_type_name t ((_, name) as path) = fst (find_type_decl' t path), name

let absolutify_value_name t =
  find
    t
    ~f:(fun path name bindings ->
      let f bindings = Option.some_if (Map.mem bindings.names name) (path, name) in
      match bindings with
      | Sigs sigs -> f sigs
      | Defs defs -> f defs)
    ~to_ustring:Value_name.Qualified.to_ustring
;;

(* TODO: how do I fill in foreign modules?
   For now, just assume a toplevel module already exists and copy (?) it into scope
   Later we can implement looking up new modules from the file system, installed packages, etc. 
   Should be able to work out all dependency information fairly easily by enforcing that
   everything is imported, including toplevel modules *)
(* NOTE: Imports at toplevel defs affect both sigs and defs, but in submodules,
   they affect defs only. This behavior is super weird, tbh.
   TODO: try to make this less confusing
   Also, maybe the order of imports should matter - could just gather them as we go? *)
let import _t _module_name =
  (*let module_bindings = find_module t [ module_name ] in
  update_current t ~f:(fun bindings ->
    { bindings with
      modules =
        Map.add bindings.modules ~key:module_name ~data:module_bindings
        |> or_name_clash "Import of duplicate module" (Module_name.to_ustring module_name)
    })*)
  failwith "TODO: module imports (properly)"
;;

(* TODO: test this, it's almost certainly wrong somehow *)
let import_filtered t path ~f =
  let path = absolutify_path t path in
  let map_to_imports_filtered path bindings ~f =
    { names =
        Map.filter_mapi bindings.names ~f:(fun ~key:name ~data:_ ->
          Option.some_if
            (f (Value_name.unidentify name))
            (Or_imported.Imported (path, name)))
    ; types =
        Map.filter_mapi bindings.types ~f:(fun ~key:type_name ~data:_ ->
          Option.some_if
            (f (Type_name.unidentify type_name))
            (Some (Or_imported.Imported (path, type_name))))
    ; modules =
        Map.filter_mapi bindings.modules ~f:(fun ~key:module_name ~data:_ ->
          Option.some_if
            (f (Module_name.unidentify module_name))
            (Or_imported.Imported (path @ [ module_name ], ())))
    }
  in
  let bindings_to_import =
    match fst (resolve_path_exn t path) with
    | Sigs sigs -> map_to_imports_filtered path ~f sigs
    | Defs defs -> map_to_imports_filtered path ~f defs
  in
  let f bindings = merge_no_shadow bindings bindings_to_import in
  update_current t ~f:{ f }
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

let add_val ?extern_name t name fixity (trait_bounds, type_expr) ~unify =
  let f bindings =
    if not (List.is_empty trait_bounds) then failwith "TODO: trait bounds in val";
    let scheme = absolutify_type_expr t type_expr in
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None ->
            compiler_bug [%message "Missing placeholder name entry" (name : Value_name.t)]
          | Some (Local existing_entry) ->
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
    }
  in
  update_current t ~f:{ f }
;;

let absolutify_type_decl t = Type.Decl.map_exprs ~f:(absolutify_type_expr t)

let add_to_types ?(err_msg = "Type name clash") types name decl =
  Map.update types name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error_msg err_msg (Type_name.to_ustring name))
;;

let add_type_decl ({ current_path; _ } as t) type_name decl =
  let f bindings =
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
            let path = Bindings_path.to_module_path current_path in
            Type.Expr.Type_app ((path, type_name), List.map params ~f:Type.Expr.var)
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
    }
  in
  update_current t ~f:{ f }
;;

let set_scheme t name scheme =
  let f bindings =
    let entry =
      { Name_entry.type_source = Let_inferred
      ; typ = Scheme scheme
      ; fixity = None
      ; extern_name = None
      }
    in
    { bindings with names = Map.set bindings.names ~key:name ~data:(Local entry) }
  in
  update_current t ~f:{ f }
;;

let add_name_placeholder t name =
  let f bindings =
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None -> Local (Name_entry.val_declared (Var Type_param_name.default))
          | Some (Local { Name_entry.type_source = Let_inferred; _ } as entry) -> entry
          | _ -> name_error_msg "Duplicate name" (Value_name.to_ustring name))
    }
  in
  update_current t ~f:{ f }
;;

let add_type_placeholder t type_name =
  let f bindings =
    { bindings with
      types = add_to_types bindings.types type_name None ~err_msg:"Duplicate type name"
    }
  in
  update_current t ~f:{ f }
;;

let merge_names t new_names ~combine =
  let new_names = Map.map new_names ~f:Or_imported.local in
  let f bindings =
    { bindings with
      names =
        Map.merge_skewed bindings.names new_names ~combine:(fun ~key entry1 entry2 ->
          let entry1, entry2 =
            resolve_name_or_import t entry1, resolve_name_or_import t entry2
          in
          Local (combine key entry1 entry2))
    }
  in
  update_current t ~f:{ f }
;;

let rec find_type_decl ?at_path t type_name =
  resolve_decl_or_import ?at_path t (snd (find_type_decl' ?at_path t type_name))

and resolve_decl_or_import ?at_path t = function
  | Some (Or_imported.Local decl) -> Some decl
  | Some (Imported path_name) ->
    (* TODO: pretty sure this imprt path should be resolved at the place it's written,
       not the current path - this goes for all imports, unless we absolutify their paths *)
    find_type_decl ?at_path t path_name
  | None -> None
;;

let find_type_decl ?at_path t type_name =
  option_or_default (find_type_decl ?at_path t type_name) ~f:(fun () ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (type_name : Type_name.Qualified.t)
          (without_std t : t)])
;;

let find_absolute_type_decl = find_type_decl ~at_path:[]
let find_type_decl = find_type_decl ?at_path:None
let current_path t = Bindings_path.to_module_path t.current_path
