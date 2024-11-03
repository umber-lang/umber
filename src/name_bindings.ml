open Import
open Names

module Name_entry = struct
  module Type_source = struct
    module T = struct
      type t =
        | Placeholder
        | Let_inferred
        | Val_declared
        | Val_and_let
        | Extern_declared
        | Effect_operation
      [@@deriving compare, enumerate, equal, sexp, variants]
    end

    include T
    include Comparable.Make (T)

    let%test "priority order" =
      List.equal
        equal
        (List.sort ~compare all)
        [ Placeholder
        ; Let_inferred
        ; Val_declared
        ; Val_and_let
        ; Extern_declared
        ; Effect_operation
        ]
    ;;
  end

  module Type_or_scheme = struct
    type t =
      | Type of Internal_type.t
      | Scheme of Module_path.absolute Type_scheme.t
    [@@deriving equal, sexp]
  end

  module Id = Unique_id.Int ()

  type t =
    { ids : Id.Set.t
         [@default Id.Set.singleton (Id.create ())] [@sexp_drop_default fun _ _ -> true]
    ; type_ : Type_or_scheme.t
    ; type_source : Type_source.t [@default Val_declared] [@sexp_drop_default.equal]
    ; fixity : Fixity.t option [@sexp.option]
    ; extern_name : Extern_name.t option [@sexp.option]
    }
  [@@deriving equal, fields, sexp]

  let identical entry entry' = not (Set.are_disjoint entry.ids entry'.ids)

  let val_declared ?fixity ?extern_name type_ =
    { ids = Id.Set.singleton (Id.create ())
    ; type_source = Val_declared
    ; type_ = Scheme type_
    ; fixity
    ; extern_name
    }
  ;;

  let create ?fixity ~type_source type_ =
    { ids = Id.Set.singleton (Id.create ())
    ; type_source
    ; type_ = Type type_
    ; fixity
    ; extern_name = None
    }
  ;;

  let placeholder () = create ~type_source:Placeholder (Internal_type.fresh_var ())

  let merge entry entry' =
    let preferred, type_, other =
      match
        Ordering.of_int (Type_source.compare entry.type_source entry'.type_source)
      with
      | Greater -> entry, entry.type_, entry'
      | Less -> entry', entry'.type_, entry
      | Equal ->
        let typ =
          match entry.type_, entry'.type_ with
          | Type _, Scheme _ | Scheme _, Scheme _ | Type _, Type _ -> entry'.type_
          | Scheme _, Type _ -> entry.type_
        in
        entry', typ, entry
    in
    let type_source : Type_source.t =
      match preferred.type_source, other.type_source with
      | Val_declared, Let_inferred | Let_inferred, Val_declared -> Val_and_let
      | _ -> preferred.type_source
    in
    let pick getter = Option.first_some (getter preferred) (getter other) in
    { ids = Set.union entry.ids entry'.ids
    ; type_
    ; type_source
    ; fixity = pick fixity
    ; extern_name = pick extern_name
    }
  ;;
end

(* TODO: Represent placeholder entries in `Type_entry.t` rather than as an extra `option`
   in `bindings`. This will be easier to understand and more consistent with [Name_entry]. *)
module Make_entry_with_id (T : Sexpable.S) () = struct
  module Id = Unique_id.Int ()

  type t =
    { id : Id.t
    ; decl : T.t
    }
  [@@deriving fields]

  let sexp_of_t { id = _; decl } = [%sexp (decl : T.t)]
  let t_of_sexp sexp = { id = Id.create (); decl = [%of_sexp: T.t] sexp }
  let identical t t' = Id.equal t.id t'.id
  let create decl = { id = Id.create (); decl }
end

module Type_entry =
  Make_entry_with_id
    (struct
      type t = Module_path.absolute Type_decl.t [@@deriving sexp]
    end)
    ()

module Effect_entry =
  Make_entry_with_id
    (struct
      type t = Module_path.absolute Effect.t [@@deriving sexp]
    end)
    ()

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
  ; effects :
      (Effect_entry.t, Effect_name.Absolute.t) Or_imported.t option Effect_name.Map.t
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
  ; effects = Effect_name.Map.empty
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
type 'b get_bindings = { get : 'a. 'a bindings -> 'b }

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
              ~data:(Local (Name_entry.val_declared ~extern_name Intrinsics.Bool.typ)))
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
          (err Value_name.to_ustring [%sexp_of: (_, Value_name.Absolute.t) Or_imported.t])
  ; types =
      Map.merge_skewed
        t1.types
        t2.types
        ~combine:
          (err
             Type_name.to_ustring
             [%sexp_of: (Type_entry.t, Type_name.Absolute.t) Or_imported.t option])
  ; effects =
      Map.merge_skewed
        t1.effects
        t2.effects
        ~combine:
          (err
             Effect_name.to_ustring
             [%sexp_of: (Effect_entry.t, Effect_name.Absolute.t) Or_imported.t option])
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
  let rec loop_sigs t ~path_to_return ~path_to_follow sigs =
    match path_to_follow with
    | [] -> Some (path_to_return, Sigs sigs)
    | module_name :: rest ->
      (match%bind Map.find sigs.modules module_name with
       | Local (None, sigs) -> loop_sigs t ~path_to_return ~path_to_follow:rest sigs
       | Local (Some _, _) -> .
       | Imported path -> resolve_absolute_path t path ~defs_only:false)
  and loop_defs t ~path_to_return ~current_path ~path_to_follow defs =
    match path_to_follow with
    | [] -> Some (path_to_return, Defs defs)
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
          | `Sig, Some sigs -> loop_sigs t ~path_to_return ~path_to_follow:rest sigs
          | `Sig, None | `Def, _ ->
            loop_defs t ~path_to_return ~current_path ~path_to_follow:rest defs)
       | Imported path -> resolve_absolute_path t path ~defs_only:false)
  and loop_defs_only t ~path_to_return ~path_to_follow defs =
    match path_to_follow with
    | [] -> Some (path_to_return, Defs defs)
    | module_name :: rest ->
      (match%bind Map.find defs.modules module_name with
       | Local (_, defs) -> loop_defs_only t ~path_to_return ~path_to_follow:rest defs
       | Imported path -> resolve_absolute_path t path ~defs_only:true)
  and resolve_absolute_path t (path : Module_path.Absolute.t) ~defs_only =
    if defs_only
    then
      loop_defs_only
        t
        ~path_to_return:path
        ~path_to_follow:(path :> Module_name.t list)
        t.toplevel
    else
      loop_defs
        t
        ~path_to_return:path
        ~current_path:(Some t.current_path)
        ~path_to_follow:(path :> Module_name.t list)
        t.toplevel
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
    let (_ : Module_path.Absolute.t), bindings_at_current =
      resolve_absolute_path_exn ~defs_only t at_path
    in
    match List.hd (path : Module_path.Relative.t :> Module_name.t list) with
    | Some first_module ->
      let full_path = Module_path.append' at_path path in
      let f bindings =
        if Map.mem bindings.modules first_module
        then (
          let full_path, bindings = resolve_absolute_path_exn ~defs_only t full_path in
          Option.value_or_thunk (f full_path name bindings) ~default:(fun () ->
            name_error ~msg:"Couldn't find name" (to_ustring input)))
        else check_parent t ~at_path input ~defs_only ~f ~to_ustring
      in
      (match bindings_at_current with
       | Sigs sigs -> f sigs
       | Defs defs -> f defs)
    | None ->
      Option.value_or_thunk (f at_path name bindings_at_current) ~default:(fun () ->
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
  match f name (snd (resolve_absolute_path_exn t path ~defs_only)) with
  | Some result -> result
  | None -> name_error ~msg:"Couldn't find name" (to_ustring input)
;;

let find_entry_with_path, find_absolute_entry_with_path =
  let open Option.Let_syntax in
  let rec f t path name bindings =
    let f bindings =
      Map.find bindings.names name >>| resolve_name_or_import_with_path t (path, name)
    in
    match bindings with
    | Sigs sigs -> f sigs
    | Defs defs -> f defs
  and find_entry_with_path t name =
    find t name ~to_ustring:Value_name.Relative.to_ustring ~f:(f t)
  and find_absolute_entry_with_path t (path, name) =
    find_absolute t (path, name) ~to_ustring:Value_name.Absolute.to_ustring ~f:(f t path)
  and resolve_name_or_import_with_path t path (entry : _ Or_imported.t) =
    match entry with
    | Local entry -> path, entry
    | Imported path_name -> find_absolute_entry_with_path t path_name
  in
  find_entry_with_path, find_absolute_entry_with_path
;;

let find_entry t name = snd (find_entry_with_path t name)
let find_absolute_entry t name = snd (find_absolute_entry_with_path t name)

let resolve_name_or_import t (entry : _ Or_imported.t) =
  match entry with
  | Local entry -> entry
  | Imported path -> find_absolute_entry t path
;;

let find_cnstr_type t cnstr_name =
  Value_name.Qualified.of_cnstr_name cnstr_name |> find_entry t |> Name_entry.type_
;;

let find_fixity t name = Option.value (find_entry t name).fixity ~default:Fixity.default

let ( (find_type_entry_with_path, find_absolute_type_entry_with_path)
    , (find_effect_entry_with_path, find_absolute_effect_entry) )
  =
  let make (type name) ~field ~name:(module Name : Name_qualified with type t = name) =
    let rec f t ~defs_only path name bindings =
      let f bindings ~check_submodule =
        match Map.find (field.get bindings) name with
        | Some entry ->
          Some (resolve_type_or_import_with_path t (path, name) entry ~defs_only)
        | None ->
          (* Allow type names like `List.List` to be found as just `List` *)
          let module_name = Name.to_ustring name |> Module_name.of_ustring_unchecked in
          let%bind.Option bindings = Map.find bindings.modules module_name in
          check_submodule bindings (Module_path.append path [ module_name ])
      in
      let find_type bindings path name =
        let%map.Option entry = Map.find (field.get bindings) name in
        resolve_type_or_import_with_path t (path, name) entry ~defs_only
      in
      let find_in_imported_submodule ~import_path =
        let bindings = snd (resolve_absolute_path_exn t import_path ~defs_only) in
        let name =
          (* Use the name of the imported module as the type name to look up. *)
          match Module_path.last import_path with
          | Some module_name ->
            Module_name.to_ustring module_name |> Name.of_ustring_unchecked
          | None ->
            compiler_bug
              [%message "Empty import path" (import_path : Module_path.Absolute.t)]
        in
        match bindings with
        | Sigs sigs -> find_type sigs import_path name
        | Defs defs -> find_type defs import_path name
      in
      match bindings with
      | Sigs sigs ->
        f sigs ~check_submodule:(fun bindings path ->
          match bindings with
          | Local (None, sigs) -> find_type sigs path name
          | Local (Some _, _) -> .
          | Imported import_path -> find_in_imported_submodule ~import_path)
      | Defs defs ->
        f defs ~check_submodule:(fun bindings path ->
          match bindings with
          | Local (None, defs) -> find_type defs path name
          | Local (Some sigs, defs) ->
            if defs_only then find_type defs path name else find_type sigs path name
          | Imported import_path -> find_in_imported_submodule ~import_path)
    and find_type_entry_with_path ?(defs_only = false) t name =
      find t name ~to_ustring:Name.Relative.to_ustring ~defs_only ~f:(f t ~defs_only)
    and find_absolute_type_entry_with_path ?(defs_only = false) t (path, name) =
      find_absolute
        t
        (path, name)
        ~defs_only
        ~to_ustring:Name.Absolute.to_ustring
        ~f:(f t ~defs_only path)
    and resolve_type_or_import_with_path
      ?defs_only
      t
      path
      (entry : _ Or_imported.t option)
      =
      match entry with
      | Some (Local decl) -> path, Some decl
      | Some (Imported path_name) ->
        find_absolute_type_entry_with_path ?defs_only t path_name
      | None -> path, None
    in
    find_type_entry_with_path, find_absolute_type_entry_with_path
  in
  ( make ~field:{ get = (fun bindings -> bindings.types) } ~name:(module Type_name)
  , make ~field:{ get = (fun bindings -> bindings.effects) } ~name:(module Effect_name) )
;;

let absolutify_path t (path : Module_path.Relative.t) =
  match Module_path.split_last path with
  | None -> current_path t
  | Some (path, name) ->
    find
      t
      (path, name)
      ~f:(fun path module_name sigs_or_defs ->
        let check_bindings bindings =
          match%map.Option Map.find bindings.modules module_name with
          | Local _ -> Module_path.append path [ module_name ]
          | Imported path -> path
        in
        match sigs_or_defs with
        | Sigs sigs -> check_bindings sigs
        | Defs defs -> check_bindings defs)
      ~to_ustring:(fun (path, name) ->
        Module_path.to_ustring (Module_path.append path [ name ]))
;;

let absolutify_value_name t path = fst (find_entry_with_path t path)
let absolutify_type_name t path = fst (find_type_entry_with_path t path)
let absolutify_effect_name t path = fst (find_effect_entry_with_path t path)

let bindings_are_empty { names; types; effects; modules } =
  Map.is_empty names && Map.is_empty types && Map.is_empty effects && Map.is_empty modules
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
  let import_not_found path name =
    name_error
      (Ustring.concat [ Module_path.to_ustring path; Ustring.of_string_exn "."; name ])
      ~msg:"Import not found"
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
        ; effects =
            find_singleton_map
              bindings.effects
              (module Effect_name)
              Effect_name.of_ustring_unchecked
              (fun name -> Some (Or_imported.Imported (path, name)))
        ; modules =
            find_singleton_map
              bindings.modules
              (module Module_name)
              Module_name.of_ustring_unchecked
              (fun name -> Or_imported.Imported (Module_path.append path [ name ]))
        }
      in
      if bindings_are_empty bindings_to_import then import_not_found path name;
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
        ; effects =
            Map.mapi bindings.effects ~f:(fun ~key:name ~data:_ ->
              Some (Or_imported.Imported (path, name)))
        ; modules =
            Map.mapi bindings.modules ~f:(fun ~key:name ~data:_ ->
              Or_imported.Imported (Module_path.append path [ name ]))
        }
      in
      merge_no_shadow acc bindings_to_import
    in
    match sigs_or_defs with
    | Sigs sigs -> import_from_bindings sigs
    | Defs defs -> import_from_bindings defs
  in
  let exclude_imported_name acc import_bindings path_so_far name =
    let name = Unidentified_name.to_ustring name in
    let value_name = Value_name.of_ustring_unchecked name in
    let type_name = Type_name.of_ustring_unchecked name in
    let effect_name = Effect_name.of_ustring_unchecked name in
    let module_name = Module_name.of_ustring_unchecked name in
    let ensure_imported_name_exists bindings =
      if not
           (Map.mem bindings.names value_name
            || Map.mem bindings.types type_name
            || Map.mem bindings.modules module_name)
      then import_not_found path_so_far name
    in
    (match import_bindings with
     | Sigs sigs -> ensure_imported_name_exists sigs
     | Defs defs -> ensure_imported_name_exists defs);
    { names = Map.remove acc.names value_name
    ; types = Map.remove acc.types type_name
    ; effects = Map.remove acc.effects effect_name
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
    | Name_excluded name -> exclude_imported_name acc import_bindings path_so_far name
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
    let path_so_far, import_bindings =
      resolve_absolute_path_exn t src_path ~defs_only:false
    in
    let bindings_to_import = loop empty_bindings import_bindings paths ~path_so_far in
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

let absolutify_type_scheme t type_ =
  Type_scheme.map'
    type_
    ~type_name:(absolutify_type_name t)
    ~effect_name:(absolutify_effect_name t)
;;

let absolutify_type_scheme' t type_ =
  Type_scheme.map
    type_
    ~type_name:(absolutify_type_name t)
    ~effect_name:(absolutify_effect_name t)
;;

let prelude = lazy (into_parent (t_of_sexp (force Sites.prelude_names)))

let add_val_or_extern ?extern_name t name fixity scheme ~constrain ~type_source =
  let f bindings =
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None ->
            compiler_bug [%message "Missing placeholder name entry" (name : Value_name.t)]
          | Some (Local existing_entry) ->
            (match existing_entry.type_source with
             | Placeholder | Let_inferred -> ()
             | Val_declared | Val_and_let | Extern_declared | Effect_operation ->
               Compilation_error.raise
                 Name_error
                 ~msg:[%message "Multiple definitions for name" (name : Value_name.t)]);
            constrain
              ~subtype:existing_entry.type_
              ~supertype:(Name_entry.Type_or_scheme.Scheme scheme);
            Local
              (Name_entry.merge
                 existing_entry
                 { ids = Name_entry.Id.Set.singleton (Name_entry.Id.create ())
                 ; type_source
                 ; type_ = Scheme scheme
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

let add_extern t name fixity typ extern_name ~constrain =
  add_val_or_extern t name fixity typ ~extern_name ~constrain ~type_source:Extern_declared
;;

let absolutify_type_decl t = Type_decl.map_exprs ~f:(absolutify_type_scheme' t)
let absolutify_effect t = Effect.map_exprs ~f:(absolutify_type_scheme' t)

let add_to_types types name decl ~err_msg =
  Map.update types name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error ~msg:err_msg (Type_name.to_ustring name))
;;

let add_to_effects effects name decl ~err_msg =
  Map.update effects name ~f:(function
    | None | Some None -> decl
    | Some _ -> name_error ~msg:err_msg (Effect_name.to_ustring name))
;;

let add_type_decl t type_name decl =
  if not (Type_decl.no_free_params decl)
  then
    Compilation_error.raise
      Type_error
      ~msg:
        [%message
          "Free parameters in type declaration" (decl : Module_path.absolute Type_decl.t)];
  let f bindings =
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
           let result_type : _ Type_scheme.type_ =
             let path = current_path t in
             let params =
               List.map (params :> Type_param_name.t list) ~f:Type_scheme.var
             in
             Type_app ((path, type_name), params)
           in
           List.fold cnstrs ~init:bindings.names ~f:(fun names (cnstr_name, args) ->
             let entry =
               Name_entry.val_declared
                 (match Nonempty.of_list args with
                  | Some args ->
                    (* TODO: This should probably have an effect for allocation. *)
                    Function (args, Effect_union [], result_type), []
                  | None -> result_type, [])
             in
             Map.set names ~key:(Value_name.of_cnstr_name cnstr_name) ~data:(Local entry))
         | _ -> bindings.names)
    }
  in
  update_current t ~f:{ f }
;;

(* TODO: This AST handling stuff being in Name_bindings feels like kind of a
   responsibility explosion. It also makes us want to have circular dependencies between
   Name_bindings and Type_bindings. Should probably make another module for this to go in
   or put it in typed.ml. *)

(* TODO: use new logic similar to val/extern/let. Also we could probably share code. *)
let add_name_entry names name new_entry =
  Map.update names name ~f:(function
    | None ->
      compiler_bug [%message "Missing placeholder name entry" (name : Value_name.t)]
    | Some (Local existing_entry : (Name_entry.t, _) Or_imported.t) ->
      (match existing_entry.type_source with
       | Placeholder -> ()
       | _ ->
         compiler_bug
           [%message
             "Unexpected non-placeholder name entry" (existing_entry : Name_entry.t)]);
      Local (Name_entry.merge existing_entry new_entry)
    | Some (Imported imported_name) ->
      name_error
        ~msg:"Name clashes with imported item"
        Ustring.(
          Value_name.to_ustring name
          ^ of_string_exn " vs "
          ^ Value_name.Absolute.to_ustring imported_name))
;;

let add_effect t effect_name effect =
  if not (Effect.no_free_params effect)
  then
    Compilation_error.raise
      Type_error
      ~msg:
        [%message
          "Free parameters in effect declaration" (effect : Module_path.absolute Effect.t)];
  let f bindings =
    let effects : _ Type_scheme.effects =
      Effect
        ( (current_path t, effect_name)
        , List.map (effect.params :> Type_param_name.t list) ~f:Type_scheme.var )
    in
    { bindings with
      effects =
        add_to_effects
          bindings.effects
          effect_name
          (Some (Local (Effect_entry.create effect)))
          ~err_msg:"Duplicate effect declarations"
    ; names =
        Effect.fold_operations
          effect
          ~init:bindings.names
          ~f:(fun names { name; args; result } ->
          let scheme : _ Type_scheme.t = Function (args, effects, result), [] in
          let new_entry : Name_entry.t =
            { ids = Name_entry.Id.Set.singleton (Name_entry.Id.create ())
            ; type_source = Effect_operation
            ; type_ = Scheme scheme
            ; fixity = None
            ; extern_name = None
            }
          in
          add_name_entry names name new_entry)
    }
  in
  update_current t ~f:{ f }
;;

let set_inferred_scheme t name scheme ~shadowing_allowed ~check_existing =
  let f bindings =
    let inferred_entry : Name_entry.t =
      { ids = Name_entry.Id.Set.singleton (Name_entry.Id.create ())
      ; type_source = Let_inferred
      ; type_ = Scheme scheme
      ; fixity = None
      ; extern_name = None
      }
    in
    { bindings with
      names =
        Map.update bindings.names name ~f:(function
          | None -> Local inferred_entry
          | Some (Local existing_entry) ->
            let multiple_definitions () =
              Compilation_error.raise
                Name_error
                ~msg:[%message "Multiple definitions for name" (name : Value_name.t)]
            in
            (match existing_entry.type_source with
             | Placeholder | Val_declared -> ()
             | Extern_declared | Effect_operation -> multiple_definitions ()
             | Let_inferred | Val_and_let ->
               if not shadowing_allowed then multiple_definitions ());
            check_existing existing_entry;
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
    | None -> Or_imported.Local (Name_entry.placeholder ())
    | Some existing_entry ->
      (* This can happen in normal cases if we have both `val` and `let` statements for a
         name. It can also happen in error cases where there are duplicate defintions of a
         name. We handle those errors later. *)
      existing_entry)
;;

let add_name_placeholder t name =
  let f bindings =
    { bindings with names = add_name_placeholder_internal bindings.names name }
  in
  update_current t ~f:{ f }
;;

let add_type_decl_placeholder t type_name (decl : _ Type_decl.t) =
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

let add_effect_placeholder t effect_name effect =
  let f bindings =
    { bindings with
      effects =
        add_to_effects bindings.effects effect_name None ~err_msg:"Duplicate effect name"
    ; names =
        Effect.fold_operations effect ~init:bindings.names ~f:(fun names operation ->
          add_name_placeholder_internal names operation.name)
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

let find_absolute_type_entry ?(defs_only = false) t type_name =
  Option.value_or_thunk
    (snd (find_absolute_type_entry_with_path ~defs_only t type_name))
    ~default:(fun () ->
      compiler_bug
        [%message
          "Placeholder decl not replaced"
            (type_name : Type_name.Absolute.t)
            (without_std t : t)])
;;

let find_absolute_effect_entry ?(defs_only = false) t effect_name =
  Option.value_or_thunk
    (snd (find_absolute_effect_entry ~defs_only t effect_name))
    ~default:(fun () ->
      compiler_bug
        [%message
          "Placeholder decl not replaced"
            (effect_name : Effect_name.Absolute.t)
            (without_std t : t)])
;;

let find_absolute_effect_decl ?defs_only t effect_name =
  (find_absolute_effect_entry ?defs_only t effect_name).decl
;;

let find_absolute_type_decl ?defs_only t type_name =
  (find_absolute_type_entry ?defs_only t type_name).decl
;;

let relativize_name_internal t (path, name) ~find_absolute ~find_relative ~identical =
  (* Idea: if we have "A.B.foo", try "foo", then "B.foo", then "A.B.Foo". This finds local
     entries, then keeps checking outer modules. It can fail to find the entry if the
     name is shadowed. *)
  if Module_path.Absolute.equal (current_path t) path
  then Some (Module_path.Relative.empty, name)
  else (
    let entry = find_absolute t (path, name) in
    let rec loop to_try current_path =
      let candidate_path = Module_path.Relative.of_module_names current_path, name in
      let found =
        try identical entry (find_relative t candidate_path) with
        | Compilation_error.Compilation_error { kind = Name_error; _ } -> false
      in
      if found
      then Some candidate_path
      else (
        match to_try with
        | [] -> None
        | module_name :: to_try -> loop to_try (module_name :: current_path))
    in
    loop (List.rev (Module_path.to_module_names path)) [])
;;

let relativize_value_name =
  relativize_name_internal
    ~find_absolute:find_absolute_entry
    ~find_relative:find_entry
    ~identical:Name_entry.identical
;;

let relativize_type_name =
  relativize_name_internal
    ~find_absolute:find_absolute_type_entry
    ~find_relative:(fun t name ->
      Option.value_exn ~here:[%here] (snd (find_type_entry_with_path t name)))
    ~identical:Type_entry.identical
;;

let relativize_effect_name =
  relativize_name_internal
    ~find_absolute:find_absolute_effect_entry
    ~find_relative:(fun t name ->
      Option.value_exn ~here:[%here] (snd (find_effect_entry_with_path t name)))
    ~identical:Effect_entry.identical
;;

let resolve_type_or_import t (decl_or_import : _ Or_imported.t option) =
  match decl_or_import with
  | Some (Local entry) -> entry
  | Some (Imported path) -> find_absolute_type_entry t path
  | None ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (decl_or_import : (Type_entry.t, Type_name.Absolute.t) Or_imported.t option)
          (without_std t : t)]
;;

let resolve_effect_or_import t (decl_or_import : _ Or_imported.t option) =
  match decl_or_import with
  | Some (Local entry) -> entry
  | Some (Imported path) -> find_absolute_effect_entry t path
  | None ->
    compiler_bug
      [%message
        "Placeholder decl not replaced"
          (decl_or_import : (Effect_entry.t, Effect_name.Absolute.t) Or_imported.t option)
          (without_std t : t)]
;;

let find_sigs_and_defs t path module_name =
  let rec loop t path module_name =
    find
      t
      (path, module_name)
      ~f:(fun absolute_path module_name -> function
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
  type t = sigs_or_defs [@@deriving sexp_of]

  let names = function
    | Sigs { names; _ } | Defs { names; _ } -> names
  ;;

  let value_names = Map.key_set << names

  let types = function
    | Sigs { types; _ } | Defs { types; _ } -> types
  ;;

  let type_names = Map.key_set << types

  let effects = function
    | Sigs { effects; _ } | Defs { effects; _ } -> effects
  ;;

  let effect_names = Map.key_set << effects

  let module_names = function
    | Sigs sigs -> Map.key_set sigs.modules
    | Defs defs -> Map.key_set defs.modules
  ;;

  let make_find ~into_bindings ~resolve t bindings name =
    Option.value_or_thunk
      (Map.find (into_bindings bindings) name)
      ~default:(fun () ->
        compiler_bug [%message "Sigs_or_defs.find failed" (bindings : sigs_or_defs)])
    |> resolve t
  ;;

  let find_entry = make_find ~into_bindings:names ~resolve:resolve_name_or_import

  let find_type_decl names t type_name =
    (make_find names t type_name ~into_bindings:types ~resolve:resolve_type_or_import)
      .decl
  ;;

  let find_effect_decl names t effect_name =
    (make_find
       names
       t
       effect_name
       ~into_bindings:effects
       ~resolve:resolve_effect_or_import)
      .decl
  ;;

  let find_module t bindings module_name =
    let open Option.Let_syntax in
    match bindings with
    | Sigs bindings ->
      (match%bind Map.find bindings.modules module_name with
       | Imported path ->
         resolve_absolute_path t path ~defs_only:false |> Option.map ~f:snd
       | Local (None, sigs) -> Some (Sigs sigs)
       | Local (Some _, _) -> .)
    | Defs bindings ->
      (match%bind Map.find bindings.modules module_name with
       | Imported path ->
         resolve_absolute_path t path ~defs_only:false |> Option.map ~f:snd
       | Local (Some sigs, _) -> Some (Sigs sigs)
       | Local (None, defs) -> Some (Defs defs))
  ;;
end

module For_testing = struct
  let create ~names =
    { core with
      toplevel = { core.toplevel with names = Map.map names ~f:Or_imported.local }
    }
  ;;
end
