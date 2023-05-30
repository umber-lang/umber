open Import
open Names

module Name_entry = struct
  module Type_source = struct
    module T = struct
      type t =
        | Placeholder
        | Let_inferred
        | Val_declared
        | Extern_declared
      [@@deriving compare, enumerate, equal, sexp, variants]
    end

    include T
    include Comparable.Make (T)

    let%test "priority order" =
      List.equal
        equal
        (List.sort ~compare all)
        [ Placeholder; Let_inferred; Val_declared; Extern_declared ]
    ;;
  end

  module Type_or_scheme = struct
    type t =
      | Type of Type.t
      | Scheme of Module_path.absolute Type.Scheme.t
    [@@deriving equal, sexp]
  end

  module Id = Unique_id.Int ()

  (* TODO: Consider having this type be responsible for assigning/tracking unique names,
     rather than doing it in the MIR. *)
  type t =
    { ids : Id.Set.t
         [@default Id.Set.singleton (Id.create ())] [@sexp_drop_default fun _ _ -> true]
    ; typ : Type_or_scheme.t
    ; type_source : Type_source.t [@default Val_declared] [@sexp_drop_default.equal]
    ; fixity : Fixity.t option [@sexp.option]
    ; extern_name : Extern_name.t option [@sexp.option]
    }
  [@@deriving equal, fields, sexp]

  let identical entry entry' = not (Set.are_disjoint entry.ids entry'.ids)

  let typ entry =
    match entry.typ with
    | Type typ -> typ
    | Scheme scheme -> Type.Scheme.instantiate scheme
  ;;

  let scheme entry =
    match entry.typ with
    | Scheme scheme -> Some scheme
    | Type _ -> None
  ;;

  let val_declared ?fixity ?extern_name typ =
    { ids = Id.Set.singleton (Id.create ())
    ; type_source = Val_declared
    ; typ = Scheme typ
    ; fixity
    ; extern_name
    }
  ;;

  (* TODO: Probably stop exposing let_inferred, just use types inside pattern names, and
     don't merge names entries, etc. *)
  let let_inferred ?fixity ?extern_name typ =
    { ids = Id.Set.singleton (Id.create ())
    ; type_source = Let_inferred
    ; typ = Type typ
    ; fixity
    ; extern_name
    }
  ;;

  let placeholder () =
    { ids = Id.Set.singleton (Id.create ())
    ; type_source = Placeholder
    ; typ = Scheme (Var Type.Param.dummy)
    ; fixity = None
    ; extern_name = None
    }
  ;;

  let merge entry entry' =
    let preferred, typ, other =
      match
        Ordering.of_int (Type_source.compare entry.type_source entry'.type_source)
      with
      | Greater -> entry, entry.typ, entry'
      | Less -> entry', entry'.typ, entry
      | Equal ->
        let typ =
          match entry.typ, entry'.typ with
          | Type _, Scheme _ | Scheme _, Scheme _ | Type _, Type _ -> entry'.typ
          | Scheme _, Type _ -> entry.typ
        in
        entry', typ, entry
    in
    let pick getter = Option.first_some (getter preferred) (getter other) in
    { ids = Set.union entry.ids entry'.ids
    ; typ
    ; type_source = preferred.type_source
    ; fixity = pick fixity
    ; extern_name = pick extern_name
    }
  ;;
end

module Type_entry = struct
  module Id = Unique_id.Int ()

  type t =
    { id : Id.t
    ; decl : Module_path.absolute Type.Decl.t
    }
  [@@deriving fields]

  let sexp_of_t { id = _; decl } = [%sexp (decl : Module_path.absolute Type.Decl.t)]

  let t_of_sexp sexp =
    { id = Id.create (); decl = [%of_sexp: Module_path.absolute Type.Decl.t] sexp }
  ;;

  let identical t t' = Id.equal t.id t'.id
  let create decl = { id = Id.create (); decl }
end

(* TODO: probably just make 'path the variable so we don't have to put unit for module paths *)
module Or_imported = struct
  type ('entry, 'path) t =
    | Local of 'entry
    | Imported of 'path
  [@@deriving equal, sexp, variants]
end

module Path = struct
  module T = struct
    type t = (Module_name.t * [ `Sig | `Def ]) list
    [@@deriving equal, compare, hash, sexp]

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
          Result.ok (Lex_helpers.lex_upper_name lexbuf)
          >>| Module_name.of_ustring_unchecked
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
         | None -> failwith "Name_bindings.Path.of_string: parse failed")
    ;;
  end

  include T
  include Sexpable.Of_stringable (T)
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_module_path t = List.map t ~f:fst |> Module_path.Relative.of_module_names
end

type t =
  { current_path : Path.t
  ; toplevel : defs
  }

and sigs = Nothing.t bindings
and defs = sigs bindings

and 'a bindings =
  { names : (Name_entry.t, Value_name.Absolute.t) Or_imported.t Value_name.Map.t
  ; types : (Type_entry.t, Type_name.Absolute.t) Or_imported.t option Type_name.Map.t
  ; modules :
      ('a option * 'a bindings, Module_path.Absolute.t) Or_imported.t Module_name.Map.t
  }
[@@deriving sexp]

type sigs_or_defs =
  | Sigs of sigs
  | Defs of defs
[@@deriving sexp_of]

let name_error ~msg ustr =
  Compilation_error.raise Name_error ~msg:[%message msg ~_:(ustr : Ustring.t)]
;;

let name_error_path path =
  name_error ~msg:"Couldn't find path" (Module_path.to_ustring path)
;;

let or_name_error_path x path =
  Option.value_or_thunk x ~default:(fun () -> name_error_path path)
;;

let empty_bindings =
  { names = Value_name.Map.empty
  ; types = Type_name.Map.empty
  ; modules = Module_name.Map.empty
  }
;;

let empty = { current_path = []; toplevel = empty_bindings }

(* We maintain the invariant that the current path of [t] is an absolute path from the
   toplevel down. *)
let current_path t =
  t.current_path |> Path.to_module_path |> Module_path.Absolute.of_relative_unchecked
;;

let without_std t =
  { t with
    toplevel =
      { t.toplevel with
        modules = Map.remove t.toplevel.modules Intrinsics.std_module_name
      }
  }
;;

type f_bindings = { f : 'a. 'a bindings -> 'a bindings }

let update_current t ~f =
  let updating_import_err imported_module =
    name_error ~msg:"Name clashes with import" (Module_path.to_ustring imported_module)
  in
  let rec loop_sigs t (sigs : sigs) path ~f =
    match path with
    | [] -> f.f sigs
    | (_, `Def) :: _ -> compiler_bug [%message "`Def inside sig path" (t : t)]
    | (module_name, `Sig) :: rest ->
      { sigs with
        modules =
          Map.update sigs.modules module_name ~f:(function
            | Some (Local (None, sigs)) -> Local (None, loop_sigs t sigs rest ~f)
            | Some (Imported imported_module) -> updating_import_err imported_module
            | None ->
              name_error_path
                (Path.to_module_path t.current_path :> Module_path.Relative.t)
            | Some (Local (Some _, _)) -> .)
      }
  in
  let rec loop_defs t defs path ~f =
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
            | Some (Imported imported_module) -> updating_import_err imported_module
            | None ->
              name_error_path
                (Path.to_module_path t.current_path :> Module_path.Relative.t))
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
            Intrinsics.all
            ~init:empty_bindings.types
            ~f:(fun types (module Intrinsic) ->
            Map.set
              types
              ~key:Intrinsic.name
              ~data:(Some (Local (Type_entry.create Intrinsic.decl))))
      ; names =
          List.fold
            Intrinsics.Bool.cnstrs
            ~init:empty_bindings.names
            ~f:(fun names (cnstr_name, extern_name) ->
            Map.set
              names
              ~key:(Value_name.of_cnstr_name cnstr_name)
              ~data:
                (Local
                   (Name_entry.val_declared
                      ~extern_name
                      (Type.Concrete.cast Intrinsics.Bool.typ))))
      }
  }
;;

let merge_no_shadow t1 t2 =
  let err to_ustring sexp_of_entry ~key:name entry1 entry2 =
    (* TODO: Attach span information to name/type/module entries so we can get good type
       errors out of them. I guess we can just chuck everything inside a Node.t? *)
    Compilation_error.raise
      Name_error
      ~msg:
        [%message
          "Name clash"
            ~name:(to_ustring name : Ustring.t)
            (entry1 : entry)
            (entry2 : entry)]
  in
  { names =
      Map.merge_skewed
        t1.names
        t2.names
        ~combine:
          (err
             Value_name.to_ustring
             [%sexp_of: (Name_entry.t, Value_name.Absolute.t) Or_imported.t])
  ; types =
      Map.merge_skewed
        t1.types
        t2.types
        ~combine:
          (err
             Type_name.to_ustring
             [%sexp_of: (Type_entry.t, Type_name.Absolute.t) Or_imported.t option])
  ; modules =
      Map.merge_skewed
        t1.modules
        t2.modules
        ~combine:
          (err
             Module_name.to_ustring
             [%sexp_of: (_, Module_path.Absolute.t) Or_imported.t])
  }
;;

let resolve_absolute_path =
  let open Option.Let_syntax in
  let rec loop_sigs t path sigs =
    match path with
    | [] -> Some (Sigs sigs)
    | module_name :: rest ->
      (match%bind Map.find sigs.modules module_name with
       | Local (None, sigs) -> loop_sigs t rest sigs
       | Local (Some _, _) -> .
       | Imported path -> resolve_absolute_path t path ~defs_only:false)
  and loop_defs t current_path path defs =
    match path with
    | [] -> Some (Defs defs)
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
          | `Sig, Some sigs -> loop_sigs t rest sigs
          | `Sig, None | `Def, _ -> loop_defs t current_path rest defs)
       | Imported path -> resolve_absolute_path t path ~defs_only:false)
  and loop_defs_only t path defs =
    match path with
    | [] -> Some (Defs defs)
    | module_name :: rest ->
      (match%bind Map.find defs.modules module_name with
       | Local (_, defs) -> loop_defs_only t rest defs
       | Imported path -> resolve_absolute_path t path ~defs_only:true)
  and resolve_absolute_path t (path : Module_path.Absolute.t) ~defs_only =
    if defs_only
    then loop_defs_only t (path :> Module_name.t list) t.toplevel
    else loop_defs t (Some t.current_path) (path :> Module_name.t list) t.toplevel
  in
  resolve_absolute_path
;;

let resolve_absolute_path_exn t path ~defs_only =
  or_name_error_path (resolve_absolute_path t path ~defs_only) path
;;

let with_path_into_defs t (path : Module_path.Absolute.t) ~f =
  let t', x =
    f
      { t with
        current_path =
          List.map (path :> Module_name.t list) ~f:(fun module_name -> module_name, `Def)
      }
  in
  { t' with current_path = t.current_path }, x
;;

(* TODO: Consider having `Name_bindings.t` contain a list (stack) of parent bindings up to
   the toplevel. This would let us just walk up those bindings instead of doing a
   complicated loop that has to keep walking from the top of tree downwards. *)
let find =
  let rec loop t ((path, name) as input) ~at_path ~defs_only ~f ~to_ustring =
    (* Try looking at the current scope, then travel up to parent scopes to find a
       matching name. *)
    let bindings_at_current = resolve_absolute_path_exn ~defs_only t at_path in
    match List.hd (path : Module_path.Relative.t :> Module_name.t list) with
    | Some first_module ->
      let full_path = Module_path.append' at_path path in
      let f bindings =
        if Map.mem bindings.modules first_module
        then (
          let bindings = resolve_absolute_path_exn ~defs_only t full_path in
          option_or_default (f full_path name bindings) ~f:(fun () ->
            name_error ~msg:"Couldn't find name" (to_ustring input)))
        else check_parent t ~at_path input ~defs_only ~f ~to_ustring
      in
      (match bindings_at_current with
       | Sigs sigs -> f sigs
       | Defs defs -> f defs)
    | None ->
      option_or_default (f at_path name bindings_at_current) ~f:(fun () ->
        check_parent t ~at_path input ~defs_only ~f ~to_ustring)
  and check_parent t ~at_path:current_path input ~defs_only ~f ~to_ustring =
    (* Recursively check the parent. *)
    match Module_path.drop_last current_path with
    | Some parent_path -> loop t input ~at_path:parent_path ~defs_only ~f ~to_ustring
    | None -> name_error ~msg:"Couldn't find name" (to_ustring input)
  in
  fun ?(defs_only = false) t input ~f ~to_ustring ->
    loop t input ~at_path:(current_path t) ~defs_only ~f ~to_ustring
;;

let find_absolute ?(defs_only = false) t ((path, name) as input) ~f ~to_ustring =
  match f name (resolve_absolute_path_exn t path ~defs_only) with
  | Some result -> result
  | None -> name_error ~msg:"Couldn't find name" (to_ustring input)
;;

let find_entry_with_path, find_absolute_entry_with_path =
  let open Option.Let_syntax in
  let rec f t current_path name bindings =
    let f bindings =
      Map.find bindings.names name
      >>| resolve_name_or_import_with_path t (current_path, name)
    in
    match bindings with
    | Sigs sigs -> f sigs
    | Defs defs -> f defs
  and find_entry_with_path t name =
    find t name ~to_ustring:Value_name.Relative.to_ustring ~f:(f t)
  and find_absolute_entry_with_path t (path, name) =
    find_absolute t (path, name) ~to_ustring:Value_name.Absolute.to_ustring ~f:(f t path)
  and resolve_name_or_import_with_path t name = function
    | Local entry -> name, entry
    | Imported path_name -> find_absolute_entry_with_path t path_name
  in
  find_entry_with_path, find_absolute_entry_with_path
;;

let rec find_entry t name = snd (find_entry_with_path t name)
and find_absolute_entry t name = snd (find_absolute_entry_with_path t name)

and resolve_name_or_import t = function
  | Or_imported.Local entry -> entry
  | Imported path_name -> find_absolute_entry t path_name
;;

let find_type t name = find_entry t name |> Name_entry.typ
let find_cnstr_type t = Value_name.Qualified.of_cnstr_name >> find_type t
let find_fixity t name = Option.value (find_entry t name).fixity ~default:Fixity.default

let find_type_entry_with_path, find_absolute_type_entry_with_path =
  let open Option.Let_syntax in
  let f path name bindings =
    let f bindings ~check_submodule =
      match Map.find bindings.types name with
      | Some decl -> Some (path, decl)
      | None ->
        (* Allow type names like `List.List` to be found as just `List` *)
        let module_name = Type_name.to_ustring name |> Module_name.of_ustring_unchecked in
        Map.find bindings.modules module_name >>= check_submodule
    in
    match bindings with
    | Sigs sigs ->
      f sigs ~check_submodule:(function
        | Local (None, sigs) ->
          let%map decl = Map.find sigs.types name in
          path, decl
        | Local (Some _, _) -> .
        | Imported import_path -> Some (path, Some (Imported (import_path, name))))
    | Defs defs ->
      f defs ~check_submodule:(function
        | Local (None, defs) ->
          let%map decl = Map.find defs.types name in
          path, decl
        | Local (Some sigs, _defs) ->
          let%map decl = Map.find sigs.types name in
          path, decl
        | Imported import_path -> Some (path, Some (Imported (import_path, name))))
  in
  let find_type_entry_with_path ?defs_only t name =
    find t name ~to_ustring:Type_name.Relative.to_ustring ?defs_only ~f
  in
  let find_absolute_type_entry_with_path ?defs_only t (path, name) =
    find_absolute
      t
      (path, name)
      ~to_ustring:Type_name.Absolute.to_ustring
      ?defs_only
      ~f:(f path)
  in
  find_type_entry_with_path, find_absolute_type_entry_with_path
;;

(* FIXME: Ideally we should have consistent behavior between all the absolutify functions,
   which should include following imports all the way to a local name. I don't think that
   is currently the case.
   
   I'm not sure if we want to follow all imports all the way, necessarily. We just need an
   absolute path, so this is basically aesthetics. I think following imports 1 step is
   probably good, since otherwise basically every path is from the current module, and
   that way the imports don't get too far away. *)
let absolutify_path t (path : Module_path.Relative.t) =
  match Module_path.split_last path with
  | None -> current_path t
  | Some (path, name) ->
    find
      t
      (path, name)
      ~f:(fun path module_name sigs_or_defs ->
        let check_bindings bindings =
          if Map.mem bindings.modules module_name
          then Some (Module_path.append path [ module_name ])
          else None
        in
        match sigs_or_defs with
        | Sigs sigs -> check_bindings sigs
        | Defs defs -> check_bindings defs)
      ~to_ustring:(fun (path, name) ->
        Module_path.to_ustring (Module_path.append path [ name ]))
;;

let absolutify_type_name t ((_, name) as path) =
  fst (find_type_entry_with_path t path), name
;;

let absolutify_value_name t name = fst (find_entry_with_path t name)

let bindings_are_empty { names; types; modules } =
  Map.is_empty names && Map.is_empty types && Map.is_empty modules
;;

let import =
  let sigs_or_defs_into_module sigs_or_defs module_name ~path_so_far =
    let get_bindings modules =
      match Map.find modules module_name with
      | Some (Or_imported.Local local) -> local
      | Some (Imported import_path) ->
        name_error
          (Module_path.to_ustring (Module_path.append path_so_far [ module_name ]))
          ~msg:
            [%string
              "Can't import an item via another import. Try importing from \
               %{import_path#Module_path} directly."]
      | None ->
        name_error
          (Module_path.to_ustring (Module_path.append path_so_far [ module_name ]))
          ~msg:"Couldn't find import path"
    in
    match sigs_or_defs with
    | Sigs sigs ->
      let None, sigs = get_bindings sigs.modules in
      Sigs sigs
    | Defs defs ->
      let sigs, defs = get_bindings defs.modules in
      (match sigs with
       | Some sigs -> Sigs sigs
       | None -> Defs defs)
  in
  let import_name acc sigs_or_defs path name ~as_:as_name =
    let name = Unidentified_name.to_ustring name in
    let as_name = Unidentified_name.to_ustring as_name in
    let import_from_bindings bindings =
      let find_singleton_map map name_module of_ustring import =
        if Map.mem map (of_ustring name)
        then Map.singleton name_module (of_ustring as_name) (import (of_ustring name))
        else Map.empty name_module
      in
      let bindings_to_import =
        { names =
            find_singleton_map
              bindings.names
              (module Value_name)
              Value_name.of_ustring_unchecked
              (fun name -> Or_imported.Imported (path, name))
        ; types =
            find_singleton_map
              bindings.types
              (module Type_name)
              Type_name.of_ustring_unchecked
              (fun name -> Some (Or_imported.Imported (path, name)))
        ; modules =
            find_singleton_map
              bindings.modules
              (module Module_name)
              Module_name.of_ustring_unchecked
              (fun name -> Or_imported.Imported (Module_path.append path [ name ]))
        }
      in
      if bindings_are_empty bindings_to_import
      then
        name_error
          (Ustring.concat
             [ Module_path.to_ustring path; Ustring.of_string_exn "."; name ])
          ~msg:"Import not found";
      merge_no_shadow acc bindings_to_import
    in
    match sigs_or_defs with
    | Sigs sigs -> import_from_bindings sigs
    | Defs defs -> import_from_bindings defs
  in
  let import_all acc sigs_or_defs path =
    let import_from_bindings bindings =
      let bindings_to_import =
        { names =
            Map.mapi bindings.names ~f:(fun ~key:name ~data:_ ->
              Or_imported.Imported (path, name))
        ; types =
            Map.mapi bindings.types ~f:(fun ~key:name ~data:_ ->
              Some (Or_imported.Imported (path, name)))
        ; modules =
            Map.mapi bindings.modules ~f:(fun ~key:name ~data:_ ->
              Or_imported.Imported (Module_path.append path [ name ]))
        }
      in
      (* FIXME: We can't just import everything and then un-import it. We'll still get
         issues about importing things we shouldn't. *)
      merge_no_shadow acc bindings_to_import
    in
    match sigs_or_defs with
    | Sigs sigs -> import_from_bindings sigs
    | Defs defs -> import_from_bindings defs
  in
  (* FIXME: Ok, how about this? We come up with all the bindings we need, then import them
     all in one go?*)
  let exclude_imported_name acc path_so_far name =
    let name = Unidentified_name.to_ustring name in
    let value_name = Value_name.of_ustring_unchecked name in
    let type_name = Type_name.of_ustring_unchecked name in
    let module_name = Module_name.of_ustring_unchecked name in
    if not
         (Map.mem acc.names value_name
         || Map.mem acc.types type_name
         || Map.mem acc.modules module_name)
    then name_error_path (Module_path.append path_so_far [ module_name ]);
    { names = Map.remove acc.names value_name
    ; types = Map.remove acc.types type_name
    ; modules = Map.remove acc.modules module_name
    }
  in
  let rec loop acc import_bindings (paths : Module.Import.Paths.t) ~path_so_far =
    match paths with
    | Module (module_name, paths') ->
      (* We need to sort the paths to ensure that we always add names before
         excluding/removing them. This works because `Module.Import.Paths.compare` puts
         `Name_excluded` last in sorted order. *)
      let paths' = Nonempty.sort paths' ~compare:Module.Import.Paths.compare in
      Nonempty.fold paths' ~init:acc ~f:(fun acc paths' ->
        loop
          acc
          (sigs_or_defs_into_module import_bindings module_name ~path_so_far)
          paths'
          ~path_so_far:(Module_path.append path_so_far [ module_name ]))
    | Name name -> import_name acc import_bindings path_so_far name ~as_:name
    | Name_as (name, as_) -> import_name acc import_bindings path_so_far name ~as_
    | Name_excluded name -> exclude_imported_name acc path_so_far name
    | All -> import_all acc import_bindings path_so_far
  in
  fun t ({ kind; paths } : Module.Import.t) ->
    (match paths with
     | Module _ | Name _ | Name_as _ | Name_excluded _ -> ()
     | All ->
       Compilation_error.raise
         Name_error
         ~msg:[%message "Universal (underscore) import without a module path"]);
    let src_path =
      match kind with
      | Absolute -> Module_path.Absolute.empty
      | Relative { nth_parent } -> Module_path.drop_last_n_exn (current_path t) nth_parent
    in
    let import_bindings = resolve_absolute_path_exn t src_path ~defs_only:false in
    let bindings_to_import =
      loop empty_bindings import_bindings paths ~path_so_far:src_path
    in
    let f bindings = merge_no_shadow bindings bindings_to_import in
    update_current t ~f:{ f }
;;

let import_all_absolute t (path : Module_path.Absolute.t) =
  match Nonempty.of_list (path :> Module_name.t list) with
  | None -> compiler_bug [%message "import_all on empty path"]
  | Some path ->
    let paths =
      Nonempty.fold_right path ~init:Module.Import.Paths.All ~f:(fun module_name paths ->
        Module (module_name, [ paths ]))
    in
    import t { kind = Absolute; paths }
;;

let import_all t path = import_all_absolute t (absolutify_path t path)

let absolutify_type_expr t type_ =
  Type.Expr.map type_ ~var:Fn.id ~pf:Fn.id ~name:(fun name -> absolutify_type_name t name)
;;

let prelude = lazy (into_parent (t_of_sexp Umber_std.Prelude.names))

let add_val_or_extern
  ?extern_name
  t
  name
  fixity
  (trait_bounds, scheme)
  ~unify
  ~type_source
  =
  let f bindings =
    if not (List.is_empty trait_bounds) then failwith "TODO: trait bounds in val";
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None ->
            compiler_bug [%message "Missing placeholder name entry" (name : Value_name.t)]
          | Some (Local existing_entry) ->
            unify (Type.Scheme.instantiate scheme) (Name_entry.typ existing_entry);
            Local
              (Name_entry.merge
                 existing_entry
                 { ids = Name_entry.Id.Set.singleton (Name_entry.Id.create ())
                 ; type_source
                 ; typ = Scheme scheme
                 ; fixity
                 ; extern_name
                 })
          | Some (Imported imported_name) ->
            (* TODO: consider allowing this use case
               e.g. importing from another module, and then giving that import a new,
               compatible type declaration *)
            name_error
              ~msg:"Duplicate val for imported item"
              Ustring.(
                Value_name.to_ustring name
                ^ of_string_exn " vs "
                ^ Value_name.Absolute.to_ustring imported_name))
    }
  in
  update_current t ~f:{ f }
;;

let add_val = add_val_or_extern ?extern_name:None ~type_source:Val_declared

let add_extern t name fixity typ extern_name ~unify =
  add_val_or_extern t name fixity typ ~extern_name ~unify ~type_source:Extern_declared
;;

let absolutify_type_decl t = Type.Decl.map_exprs ~f:(absolutify_type_expr t)

let add_to_types ?(err_msg = "Type name clash") types name decl =
  Map.update types name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error ~msg:err_msg (Type_name.to_ustring name))
;;

let add_type_decl t type_name decl =
  let f bindings =
    if not (Type.Decl.no_free_params decl)
    then
      Compilation_error.raise
        Type_error
        ~msg:
          [%message
            "Free parameters in type declaration"
              (decl : Module_path.absolute Type.Decl.t)];
    { bindings with
      types =
        add_to_types
          bindings.types
          type_name
          (Some (Local (Type_entry.create decl)))
          ~err_msg:"Duplicate type declarations"
    ; names =
        (match decl with
         | params, Variants cnstrs ->
           (* Add constructors as functions to the namespace *)
           let result_type : _ Type.Scheme.t =
             let path = current_path t in
             let params = List.map params ~f:Type.Expr.var in
             Type_app ((path, type_name), params)
           in
           List.fold cnstrs ~init:bindings.names ~f:(fun names (cnstr_name, args) ->
             let entry =
               Name_entry.val_declared
                 (match Nonempty.of_list args with
                  | Some args -> Function (args, result_type)
                  | None -> result_type)
             in
             Map.set names ~key:(Value_name.of_cnstr_name cnstr_name) ~data:(Local entry))
         | _ -> bindings.names)
    }
  in
  update_current t ~f:{ f }
;;

let set_inferred_scheme t name scheme =
  let f bindings =
    let inferred_entry : Name_entry.t =
      { ids = Name_entry.Id.Set.singleton (Name_entry.Id.create ())
      ; type_source = Let_inferred
      ; typ = Scheme scheme
      ; fixity = None
      ; extern_name = None
      }
    in
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None -> Local inferred_entry
          | Some (Local existing_entry) ->
            Local (Name_entry.merge existing_entry inferred_entry)
          | Some (Imported _) ->
            (* TODO: Think about the exact semantics of this. I think we want to disallow
               shadowing/name clashes between imported and local names, but I'm not sure
               if here is the best place to do it. *)
            name_error
              ~msg:"Name clash between imported and local binding"
              (Value_name.to_ustring name))
    }
  in
  update_current t ~f:{ f }
;;

let add_name_placeholder_internal names name =
  Map.update names name ~f:(function
    | None -> Local (Name_entry.placeholder ())
    | Some (Or_imported.Local { Name_entry.type_source = Let_inferred; _ } as entry) ->
      entry
    | _ -> name_error ~msg:"Duplicate name" (Value_name.to_ustring name))
;;

let add_name_placeholder t name =
  let f bindings =
    { bindings with names = add_name_placeholder_internal bindings.names name }
  in
  update_current t ~f:{ f }
;;

let add_type_decl_placeholder t type_name (decl : _ Type.Decl.t) =
  let f bindings =
    { bindings with
      types = add_to_types bindings.types type_name None ~err_msg:"Duplicate type name"
    ; names =
        (match decl with
         | _, Variants cnstrs ->
           (* Add placeholders for variant constructor names. *)
           List.fold cnstrs ~init:bindings.names ~f:(fun names (cnstr_name, _) ->
             add_name_placeholder_internal names (Value_name.of_cnstr_name cnstr_name))
         | _ -> bindings.names)
    }
  in
  update_current t ~f:{ f }
;;

let fold_local_names t ~init ~f =
  let fold_local path bindings init =
    Map.fold bindings.names ~init ~f:(fun ~key:name ~data acc ->
      match data with
      | Local entry -> f acc (path, name) entry
      | Imported _ -> acc)
  in
  let rec fold_defs t path (defs : defs) ~init ~f =
    Map.fold
      defs.modules
      ~init:(fold_local path defs init)
      ~f:(fun ~key:module_name ~data acc ->
      (* We can ignore sigs here because defs should have all the names *)
      match data with
      | Local (_, defs) ->
        fold_defs t (Module_path.append path [ module_name ]) defs ~init:acc ~f
      | Imported _ -> acc)
  in
  fold_defs t Module_path.Absolute.empty t.toplevel ~init ~f
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

let rec find_type_entry ?defs_only t type_name =
  resolve_type_or_import
    ?defs_only
    t
    (snd (find_type_entry_with_path ?defs_only t type_name))

and find_absolute_type_entry ?defs_only t type_name =
  resolve_type_or_import
    ?defs_only
    t
    (snd (find_absolute_type_entry_with_path ?defs_only t type_name))

and resolve_type_or_import ?defs_only t = function
  | Some (Or_imported.Local decl) -> Some decl
  | Some (Imported path_name) -> find_absolute_type_entry ?defs_only t path_name
  | None -> None
;;

let find_type_entry ?(defs_only = false) t type_name =
  option_or_default (find_type_entry ~defs_only t type_name) ~f:(fun () ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (type_name : Type_name.Relative.t)
          (without_std t : t)])
;;

let find_absolute_type_entry ?(defs_only = false) t type_name =
  option_or_default (find_absolute_type_entry ~defs_only t type_name) ~f:(fun () ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (type_name : Type_name.Absolute.t)
          (without_std t : t)])
;;

let find_type_decl ?defs_only t type_name = (find_type_entry ?defs_only t type_name).decl

let find_absolute_type_decl ?defs_only t type_name =
  (find_absolute_type_entry ?defs_only t type_name).decl
;;

let resolve_type_or_import t decl_or_import =
  option_or_default (resolve_type_or_import t decl_or_import) ~f:(fun () ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (decl_or_import : (Type_entry.t, Type_name.Absolute.t) Or_imported.t option)
          (without_std t : t)])
;;

let find_sigs_and_defs t path module_name =
  let rec loop t path module_name =
    find
      t
      (path, module_name)
      ~f:
        (fun absolute_path module_name -> function
          | Sigs _ ->
            compiler_bug
              [%message
                "Name_bindings.find_sigs_and_defs found only sigs"
                  ~relative_path:(path : Module_path.Relative.t)
                  (module_name : Module_name.t)
                  (absolute_path : Module_path.Absolute.t)
                  (t : t)]
          | Defs bindings ->
            (match%bind.Option Map.find bindings.modules module_name with
             | Imported path ->
               let%bind.Option path, module_name =
                 List.split_last (path :> Module_name.t list)
               in
               let path = Module_path.Relative.of_module_names path in
               Some (loop t path module_name)
             | Local sigs_and_defs -> Some sigs_and_defs))
      ~to_ustring:(fun (path, module_name) ->
        Module_path.to_ustring (Module_path.append path [ module_name ]))
  in
  let sigs, defs = loop t path module_name in
  Option.map sigs ~f:(fun sigs -> Sigs sigs), Defs defs
;;

module Sigs_or_defs = struct
  type name_bindings = t
  type t = sigs_or_defs

  let names = function
    | Sigs { names; _ } | Defs { names; _ } -> names
  ;;

  let value_names = Map.key_set << names

  let types = function
    | Sigs { types; _ } | Defs { types; _ } -> types
  ;;

  let type_names = Map.key_set << types

  let module_names = function
    | Sigs sigs -> Map.key_set sigs.modules
    | Defs defs -> Map.key_set defs.modules
  ;;

  let make_find ~into_bindings ~resolve t bindings name =
    option_or_default
      (Map.find (into_bindings bindings) name)
      ~f:(fun () ->
        compiler_bug [%message "Sigs_or_defs.find failed" (bindings : sigs_or_defs)])
    |> resolve t
  ;;

  let find_entry = make_find ~into_bindings:names ~resolve:resolve_name_or_import

  let find_type_decl names t type_name =
    (make_find names t type_name ~into_bindings:types ~resolve:resolve_type_or_import)
      .decl
  ;;

  let find_module t bindings module_name =
    let open Option.Let_syntax in
    match bindings with
    | Sigs bindings ->
      (match%bind Map.find bindings.modules module_name with
       | Imported path -> resolve_absolute_path t path ~defs_only:false
       | Local (None, sigs) -> Some (Sigs sigs)
       | Local (Some _, _) -> .)
    | Defs bindings ->
      (match%bind Map.find bindings.modules module_name with
       | Imported path -> resolve_absolute_path t path ~defs_only:false
       | Local (Some sigs, _) -> Some (Sigs sigs)
       | Local (None, defs) -> Some (Defs defs))
  ;;
end
