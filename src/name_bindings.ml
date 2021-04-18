open Import
open Names

(* FIXME: (!) Remove all traces of code trying to do sig lookups inside defs when lookups
   in the def fail - we're just adding a new compiler pass for that.
   I think keeping lookups to the def when there is no sig is ok, as we don't have
   information to propogate e.g. val statements to sigs before type inference is done *)

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

type t =
  { current_path : Module_path.t
  ; sigs : sigs option
  ; defs : defs
  }

and sigs = Nothing.t bindings

and defs = sigs bindings

(* TODO: modules should probably be imported too, not just copied (I think)
   For now, since modules can't be consumed as values in any way, it should be ok
   (avoiding copying is good for performance/avoiding potential issues with phys_equal) *)
and 'a bindings =
  { names : (Name_entry.t, Value_name.t) Or_imported.t Value_name.Map.t
  ; types : (Type.Decl.t, Type_name.t) Or_imported.t option Type_name.Map.t
  ; modules : ('a option * 'a bindings) Module_name.Map.t
  }
[@@deriving sexp]

type sigs_defs =
  | Sigs of sigs
  | Sigs_and_defs of sigs option * defs
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

let or_name_error_path x path = option_or_default x ~f:(fun () -> name_error_path path)

let empty_bindings =
  { names = Value_name.Map.empty
  ; types = Type_name.Map.empty
  ; modules = Module_name.Map.empty
  }
;;

let empty = { current_path = []; sigs = None; defs = empty_bindings }

let defs_of_sigs (sigs : sigs) : defs =
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
;;

let without_std t =
  { t with
    sigs =
      Option.map t.sigs ~f:(fun sigs ->
        { sigs with modules = Map.remove sigs.modules Core.std_module_name })
  ; defs = { t.defs with modules = Map.remove t.defs.modules Core.std_module_name }
  }
;;

let add_to_types ?(err_msg = "Type name clash") types name decl =
  Map.update types name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error_msg err_msg (Type_name.to_ustring name))
;;

(* TODO: decide what to do with create_empty - should it just always be in effect? *)
let update_current_sigs ?(create_empty = true) t ~f =
  let rec loop sigs path ~f =
    match path with
    | [] -> f sigs
    | module_name :: rest ->
      (* FIXME: if there's a name error, try looking in the def *)
      { sigs with
        modules =
          Map.update sigs.modules module_name ~f:(function
            | Some (other, sigs) -> other, loop sigs rest ~f
            | None ->
              if create_empty
              then None, loop empty_bindings rest ~f
              else name_error_path path)
      }
  in
  (* TOOD: is it ok to always create empty sigs here? *)
  { t with
    sigs = Some (loop (Option.value t.sigs ~default:empty_bindings) t.current_path ~f)
  }
;;

let update_current_defs t ~f =
  let rec loop defs path ~f =
    match path with
    | [] -> f defs
    | module_name :: rest ->
      { defs with
        modules =
          Map.update defs.modules module_name ~f:(function
            | Some (sigs, defs) -> sigs, loop defs rest ~f
            | None -> name_error_path path)
      }
  in
  { t with defs = loop t.defs t.current_path ~f }
;;

let into_module t ~place module_name =
  let f bindings =
    { bindings with
      modules =
        Map.update
          bindings.modules
          module_name
          ~f:(Option.value ~default:(None, empty_bindings))
    }
  in
  let t =
    match place with
    | `Sig -> update_current_sigs t ~f ~create_empty:true
    | `Def -> update_current_defs t ~f
  in
  { t with current_path = t.current_path @ [ module_name ] }
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

let with_path t path ~f =
  let t', x = f { t with current_path = path } in
  { t' with current_path = t.current_path }, x
;;

let core =
  { current_path = []
  ; sigs =
      Some
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
  ; defs = empty_bindings
  }
;;

let merge_shadow t1 t2 =
  let shadow ~key:_ _ x = x in
  { names = Map.merge_skewed t1.names t2.names ~combine:shadow
  ; types = Map.merge_skewed t1.types t2.types ~combine:shadow
  ; modules = Map.merge_skewed t1.modules t2.modules ~combine:shadow
  }
;;

let merge_no_shadow t1 t2 =
  let err to_ustring ~key:name = name_error_msg "Name clash" (to_ustring name) in
  { names = Map.merge_skewed t1.names t2.names ~combine:(err Value_name.to_ustring)
  ; types = Map.merge_skewed t1.types t2.types ~combine:(err Type_name.to_ustring)
  ; modules = Map.merge_skewed t1.modules t2.modules ~combine:(err Module_name.to_ustring)
  }
;;

let bindings_at_path =
  let open Option.Let_syntax in
  let rec loop current_path path sigs defs =
    match path with
    | [] ->
      let following_current = Option.is_some current_path in
      (match following_current, sigs, defs with
      | true, _, Some defs -> Some (Sigs_and_defs (sigs, defs))
      | false, None, Some defs -> Some (Sigs_and_defs (None, defs))
      | _, Some sigs, _ -> Some (Sigs sigs)
      | _, None, None -> None)
    | module_name :: rest ->
      let current_path, following_current =
        match current_path with
        | Some [] | None -> None, false
        | Some (module_name' :: rest') ->
          if Module_name.equal module_name module_name'
          then Some rest', true
          else None, false
      in
      if following_current
      then (
        match pursue_defs current_path defs module_name rest with
        | Some _ as sigs_and_defs -> sigs_and_defs
        | None ->
          let%bind sigs = sigs in
          let%bind _, sigs = Map.find sigs.modules module_name in
          loop current_path rest (Some sigs) None)
      else (
        (* FIXME: PROBLEM: during type-checking of our own module, the sig is not visible externally
        We should be allowed to see defs from our current module, whether or not that is the target 
        You should also be allowed to follow defs all the way into where you currently are 
        ^ you can follow defs if the target path is a child of your path (or equal)*)
        match sigs with
        | Some sigs ->
          let%bind _, sigs = Map.find sigs.modules module_name in
          loop current_path rest (Some sigs) None
        | None -> pursue_defs current_path defs module_name rest)
  and pursue_defs current_path defs module_name rest =
    let%bind defs = defs in
    let%bind sigs, defs = Map.find defs.modules module_name in
    loop current_path rest sigs (Some defs)
  in
  fun t path -> loop (Some t.current_path) path t.sigs (Some t.defs)
;;

(* FIXME: this is looking stuff up in sigs when it probably shouldn't be
   - we have a compiler pass to handle this now *)
let check_sigs_and_defs { current_path; _ } path (sigs, defs) ~f_sigs ~f_defs =
  let open Option.Let_syntax in
  if Module_path.equal current_path path || Option.is_none sigs
  then (
    match f_defs defs with
    | Some _ as result -> result
    | None -> sigs >>= f_sigs)
  else sigs >>= f_sigs
;;

let check_sigs_defs t path bindings ~f_sigs ~f_defs =
  match bindings with
  | Sigs sigs -> f_sigs sigs
  | Sigs_and_defs (sigs, defs) -> check_sigs_and_defs t path (sigs, defs) ~f_sigs ~f_defs
;;

let check_sigs_defs' { current_path; _ } path bindings ~f_sigs ~f_defs =
  match bindings with
  | Sigs sigs -> f_sigs sigs
  | Sigs_and_defs (sigs, defs) ->
    if Module_path.equal current_path path || Option.is_none sigs
    then f_defs defs
    else f_sigs (Option.value sigs ~default:empty_bindings)
;;

let rec find ?at_path t ((path, name) as input) ~f ~to_ustring =
  (* Try looking at the current scope, then travel up to parent scopes to find a matching name *)
  let open Option.Let_syntax in
  let at_path = option_or_default at_path ~f:(fun () -> t.current_path) in
  let bindings_at_current = or_name_error_path (bindings_at_path t at_path) at_path in
  (* print_s
    [%message
      "find"
        (to_ustring input : Ustring.t)
        (at_path : Module_path.t)
        (bindings_at_current : sigs_defs)]; *)
  match path with
  | first_module :: _ ->
    let full_path = at_path @ path in
    let f_helper bindings =
      (* FIXME: PROBLEM: this module lookup is failing when bindings_at_current fails to
         contain the module we need, since it lacks external sig info

         ^ By external sig, we mean a sig which is a child of a parent sig e.g.
         module A :
           module B :
             type alias B = Int
         
         I want to be able to elide stuff from defs if its just type info - should be able
         to look at the sigs, but may have to look up to parents (e.g. the sig of B inside
         the def of A is empty, we need the sig of B inside the sig of A)
         
         IDEA: what if instead of passing up direct maps/bindings, we wrapped it in a
         cleverer type which could do lookups on your behalf, taking into account 
         external sigs when doing lookups/iterating over to do merges (for imports) *)
      if Map.mem bindings.modules first_module
      then (
        (* print_s [%message "Found match!" (first_module : Module_name.t)]; *)
        (* FIXME: needs to take into account sigs from parent modules when searching
           IDEA: could try just merging all the parent sigs at once? *)
        (* Lookup and merge relevant sigs from each parent module *)
        (* TODO: doing this on every lookup seems pretty bad 
           Also, bindings_at_path looks up from the module root from scratch each time,
           which doesn't seem ideal *)
        let local_sigs, local_defs =
          match bindings_at_path t full_path with
          | Some (Sigs sigs) -> sigs, None
          | Some (Sigs_and_defs (sigs, defs)) ->
            option_or_default sigs ~f:(fun () -> sigs_of_defs defs), Some defs
          | None -> empty_bindings, None
        in
        let rec merge_parent_sigs sigs full_path my_path =
          (* FIXME: should be looking for a module via the path I've cut off -
             my own module, not the entire parent *)
          match list_split_last full_path with
          | Some (full_path, last_module) ->
            let my_path = last_module :: my_path in
            let sigs' =
              let extract_sigs = function
                | Sigs sigs | Sigs_and_defs (Some sigs, _) -> sigs
                | Sigs_and_defs (None, defs) -> sigs_of_defs defs
              in
              let%bind parent_sigs = bindings_at_path t full_path >>| extract_sigs in
              let%map my_sigs_in_parent =
                bindings_at_path
                  { current_path = []; sigs = Some parent_sigs; defs = empty_bindings }
                  my_path
                >>| extract_sigs
              in
              merge_shadow my_sigs_in_parent sigs
            in
            merge_parent_sigs (Option.value sigs' ~default:sigs) full_path my_path
          | None -> sigs
        in
        let sigs = merge_parent_sigs local_sigs full_path [] in
        let sigs_defs =
          match local_defs with
          | Some defs -> Sigs_and_defs (Some sigs, defs)
          | None -> Sigs sigs
        in
        option_or_default (f full_path name sigs_defs) ~f:(fun () ->
          raise (Name_error (to_ustring input))))
      else check_parent t at_path input ~f ~to_ustring
    in
    check_sigs_defs' t full_path ~f_sigs:f_helper ~f_defs:f_helper bindings_at_current
  | [] ->
    option_or_default (f at_path name bindings_at_current) ~f:(fun () ->
      check_parent t at_path input ~f ~to_ustring)

and check_parent t current_path input ~f ~to_ustring =
  (* Recursively check the parent *)
  match List.drop_last current_path with
  | Some parent_path -> find t ~at_path:parent_path input ~f ~to_ustring
  | None -> raise (Name_error (to_ustring input))
;;

let find_module t path =
  (* print_s [%message "find_module" (path : Module_path.t) (t : t)]; *)
  find
    t
    (path, ())
    ~f:(fun _ () -> Option.some)
    ~to_ustring:(fun (path, ()) -> Module_path.to_ustring path)
;;

let rec find_entry t =
  let open Option.Let_syntax in
  find t ~to_ustring:Value_name.Qualified.to_ustring ~f:(fun path name ->
    let f bindings = Map.find bindings.names name >>| resolve_name_or_import t in
    check_sigs_defs t path ~f_sigs:f ~f_defs:f)

and resolve_name_or_import t = function
  | Or_imported.Local entry -> entry
  | Imported path_name -> find_entry t path_name
;;

let find_type t name = find_entry t name |> Name_entry.typ
let find_cnstr_type t = Value_name.Qualified.of_cnstr_name >> find_type t
let find_fixity t name = Option.value (find_entry t name).fixity ~default:Fixity.default

let find_type_decl' ?at_path t name =
  let open Option.Let_syntax in
  find
    ?at_path
    t
    name
    ~to_ustring:Type_name.Qualified.to_ustring
    ~f:(fun path name sigs_defs ->
    (* print_s
      [%message
        "find_type_decl'"
          (path : Module_path.t)
          (name : Type_name.t)
          (sigs_defs : sigs_defs)]; *)
    let f bindings ~try_again =
      match Map.find bindings.types name with
      | Some decl -> Some (path, decl)
      | None ->
        let module_name = Type_name.to_ustring name |> Module_name.of_ustring_unchecked in
        let%bind bindings = Map.find bindings.modules module_name in
        let%map decl = try_again bindings in
        path, decl
    in
    check_sigs_defs
      t
      path
      sigs_defs
      ~f_sigs:(f ~try_again:(fun (_, sigs) -> Map.find sigs.types name))
      ~f_defs:
        (f ~try_again:(fun (sigs, defs) ->
           let f bindings = Map.find bindings.types name in
           check_sigs_and_defs t path ~f_sigs:f ~f_defs:f (sigs, defs))))
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
    ~f:(fun path name ->
      let f bindings = Option.some_if (Map.mem bindings.names name) (path, name) in
      check_sigs_defs t path ~f_sigs:f ~f_defs:f)
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
let import _t ~place:_ _module_name =
  (*let module_bindings = find_module t [ module_name ] in
  update_current t ~f:(fun bindings ->
    { bindings with
      modules =
        Map.add bindings.modules ~key:module_name ~data:module_bindings
        |> or_name_clash "Import of duplicate module" (Module_name.to_ustring module_name)
    })*)
  failwith "TODO: module imports (properly)"
;;

let filter bindings ~f =
  { names = Map.filter_keys bindings.names ~f:(f << Value_name.unidentify)
  ; types = Map.filter_keys bindings.types ~f:(f << Type_name.unidentify)
  ; modules = Map.filter_keys bindings.modules ~f:(f << Module_name.unidentify)
  }
;;

(* FIXME: import modules directly, don't copy the contents *)
let import_filtered t ~place path ~f =
  let map_to_imports_names_types path bindings =
    { bindings with
      names =
        Map.mapi bindings.names ~f:(fun ~key:name ~data:_ ->
          Or_imported.Imported (absolutify_path t path, name))
    ; types =
        Map.mapi bindings.types ~f:(fun ~key:name ~data:_ ->
          Some (Or_imported.Imported (absolutify_path t path, name)))
    }
  in
  let rec map_to_imports_sigs path (sigs : sigs) : sigs =
    { (map_to_imports_names_types path sigs) with
      modules =
        Map.mapi sigs.modules ~f:(fun ~key:module_name ~data:(None, bindings) ->
          None, map_to_imports_sigs (path @ [ module_name ]) bindings)
    }
  in
  let rec map_to_imports_defs path (defs : defs) =
    { (map_to_imports_names_types path defs) with
      modules =
        Map.mapi defs.modules ~f:(fun ~key:module_name ~data:(sigs, defs) ->
          let new_path = path @ [ module_name ] in
          ( Option.map sigs ~f:(map_to_imports_sigs new_path)
          , map_to_imports_defs new_path defs ))
    }
  in
  let f_sigs sigs = map_to_imports_sigs path (filter ~f sigs) in
  let f_defs defs = map_to_imports_defs path (filter ~f defs) in
  let merge sigs_defs ~f_sigs ~f_defs bindings =
    merge_no_shadow bindings (check_sigs_defs' t path sigs_defs ~f_sigs ~f_defs)
  in
  (* FIXME: just finding one set of sigs/defs is not good enough: what about the others? *)
  let sigs_defs = find_module t path in
  match place with
  | `Sig ->
    update_current_sigs t ~f:(merge ~f_sigs ~f_defs:(sigs_of_defs << f_defs) sigs_defs)
  | `Def ->
    update_current_defs t ~f:(merge ~f_sigs:(defs_of_sigs << f_sigs) ~f_defs sigs_defs)
;;

let import_all = import_filtered ~f:(fun _ -> true)

let import_with t ~place path = function
  | [] -> import_all t ~place path
  | imports ->
    import_filtered t ~place path ~f:(List.mem imports ~equal:Unidentified_name.equal)
;;

let import_without t ~place path hiding =
  import_filtered t ~place path ~f:(not << List.mem hiding ~equal:Unidentified_name.equal)
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
     import_all t Core.prelude_module_path ~place:`Sig)
;;

let add_val ?extern_name t ~place name fixity (trait_bounds, type_expr) ~unify =
  let f bindings =
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
    }
  in
  match place with
  | `Sig -> update_current_sigs t ~f
  | `Def -> update_current_defs t ~f
;;

let absolutify_type_decl t = Type.Decl.map_exprs ~f:(absolutify_type_expr t)

let add_type_decl ({ current_path; _ } as t) ~place type_name decl =
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
    }
  in
  match place with
  | `Sig -> update_current_sigs t ~f
  | `Def -> update_current_defs t ~f
;;

let set_scheme t ~place name scheme =
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
  match place with
  | `Sig -> update_current_sigs t ~f
  | `Def -> update_current_defs t ~f
;;

let add_fresh_var t ~place name =
  (* TODO: remove *)
  (* print_s [%message "add_fresh_var" (place : [< `Def | `Sig ]) (name : Value_name.t)]; *)
  let typ = Type.fresh_var () in
  let f bindings =
    { bindings with
      names =
        Map.add bindings.names ~key:name ~data:(Local (Name_entry.placeholder typ))
        |> or_name_clash "Duplicate name" (Value_name.to_ustring name)
    }
  in
  match place with
  | `Sig -> update_current_sigs t ~f, typ
  | `Def -> update_current_defs t ~f, typ
;;

let add_type_placeholder t ~place type_name =
  let f bindings =
    { bindings with
      types = add_to_types bindings.types type_name None ~err_msg:"Duplicate type name"
    }
  in
  match place with
  | `Sig -> update_current_sigs t ~f
  | `Def -> update_current_defs t ~f
;;

let merge_names t new_names ~combine =
  let new_names = Map.map new_names ~f:Or_imported.local in
  update_current_defs t ~f:(fun bindings ->
    { bindings with
      names =
        Map.merge_skewed bindings.names new_names ~combine:(fun ~key entry1 entry2 ->
          let entry1, entry2 =
            resolve_name_or_import t entry1, resolve_name_or_import t entry2
          in
          Local (combine key entry1 entry2))
    })
;;

let rec find_type_decl ?at_path t type_name =
  resolve_decl_or_import ?at_path t (snd (find_type_decl' ?at_path t type_name))

and resolve_decl_or_import ?at_path t = function
  | Some (Or_imported.Local decl) -> Some decl
  | Some (Imported path_name) -> find_type_decl ?at_path t path_name
  | None -> None
;;

let find_type_decl ?at_path t type_name =
  option_or_default (find_type_decl ?at_path t type_name) ~f:(fun () ->
    compiler_bug [%message "Placeholder decl not replaced"])
;;

let find_absolute_type_decl = find_type_decl ~at_path:[]
let find_type_decl = find_type_decl ?at_path:None
let current_path t = t.current_path
