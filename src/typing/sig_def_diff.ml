open! Core
open! Import
open Names
module Sigs_or_defs = Name_bindings.Sigs_or_defs

type 'a diff =
  | Missing_from_def
  | Incompatible of 'a * 'a
[@@deriving sexp_of]

type t =
  { name_diff : (Value_name.t * Name_bindings.Name_entry.t diff) Sequence.t
       [@sexp.omit_nil]
  ; type_diff : (Type_name.t * Module_path.absolute Type_decl.t diff) Sequence.t
       [@sexp.omit_nil]
  ; effect_diff : (Effect_name.t * Module_path.absolute Effect.t diff) Sequence.t
       [@sexp.omit_nil]
  ; module_diff : (Module_name.t * module_diff) Sequence.t [@sexp.omit_nil]
  }
[@@deriving sexp_of]

and module_diff =
  | Missing_module
  | Module_diff of t

let empty =
  { name_diff = Sequence.empty
  ; type_diff = Sequence.empty
  ; effect_diff = Sequence.empty
  ; module_diff = Sequence.empty
  }
;;

let is_empty { name_diff; type_diff; effect_diff; module_diff } =
  Sequence.is_empty name_diff
  && Sequence.is_empty type_diff
  && Sequence.is_empty effect_diff
  && Sequence.is_empty module_diff
;;

module By_kind = struct
  type 'a t =
    { sig_ : 'a
    ; def : 'a
    }
  [@@deriving sexp_of]
end

exception Compatibility_error

let no_errors f =
  match f () with
  | () -> true
  | (exception Compatibility_error)
  | (exception Compilation_error.Compilation_error { kind = Type_error; _ }) -> false
;;

let fold2 xs ys ~init ~f =
  match List.fold2 xs ys ~init ~f with
  | Ok result -> result
  | Unequal_lengths -> raise Compatibility_error
;;

let iter2 xs ys ~f = fold2 xs ys ~init:() ~f:(fun () x -> f x)

let create_skolemized_type ~names =
  Name_bindings.with_path_into_defs names Module_path.Absolute.empty ~f:(fun names ->
    let type_name = Type_name.create_skolemized () in
    ( Name_bindings.add_type_decl names type_name (Unique_list.empty, Abstract)
    , Type_scheme.Type_app ((Module_path.Absolute.empty, type_name), []) ))
;;

(** Skolemization means replacing all type variables in a type expression with fresh
    abstract types. e.g. `a -> b -> c` becomes something like `A -> B -> C` where `A`,
    `B`, and `C` are fresh abstract types. *)
let skolemize ~names ~types ~types_by_param scheme =
  let types_by_param, create_skolemized_type =
    match types_by_param with
    | None -> Type_param.Table.create (), create_skolemized_type
    | Some types_by_param ->
      ( types_by_param
      , fun ~names:_ ->
          compiler_bug
            [%message
              "Missing declaration for skolemized param"
                (scheme : _ Type_scheme.t)
                (types_by_param : _ Type_scheme.type_ Type_param.Table.t)] )
  in
  let names = ref names in
  let type_ =
    Type_bindings.instantiate_type_scheme
      ~names:!names
      ~types
      (Type_scheme.map' scheme ~type_name:Fn.id ~effect_name:Fn.id ~f:(function
        | Var param ->
          Halt
            (Hashtbl.find_or_add types_by_param param ~default:(fun () ->
               let names', type_ = create_skolemized_type ~names:!names in
               names := names';
               type_))
        | type_ -> Defer type_))
  in
  !names, type_
;;

(* TODO: This doesn't check that constraints are compatible - we'd need to also map those.
   Also applies to check_val_scheme_vs_inferred_scheme. This is sound because
   [Type_bindings.generalize] never produces a type with any constraints. *)
(** A `val` item in a signature is compatible with a `let` in a defintion if the
    signature is a "more specific" version of the defintion. We can check this by
    skolemizing the signature, instantiating the defintion, and then unifying the two. *)
let check_val_type_schemes ~names ({ sig_ = sig_scheme; def = def_scheme } : _ By_kind.t) =
  let types = Type_bindings.create () in
  let names, sig_type = skolemize ~names ~types sig_scheme ~types_by_param:None in
  let def_type = Type_bindings.instantiate_type_scheme ~names ~types def_scheme in
  Type_bindings.constrain ~names ~types ~subtype:def_type ~supertype:sig_type
;;

(** Type definitions in a signature an defintion are compatible if they are the same
    modulo type aliases and type variable renamings. Unlike for `val`s, the compatibility
    is symmetrical. *)
let check_type_decl_schemes
  ~names
  ~sig_params
  ~def_params
  ({ sig_ = sig_scheme; def = def_scheme } : _ By_kind.t)
  =
  let types = Type_bindings.create () in
  let names, sig_type =
    skolemize ~names ~types ~types_by_param:(Some sig_params) sig_scheme
  in
  let names, def_type =
    skolemize ~names ~types ~types_by_param:(Some def_params) def_scheme
  in
  Type_bindings.constrain ~names ~types ~subtype:def_type ~supertype:sig_type
;;

let compatible_name_entries ~names ~sig_:sig_entry ~def:def_entry =
  let get_scheme entry =
    match Name_bindings.Name_entry.type_ entry with
    | Scheme scheme -> scheme
    | Type _ ->
      compiler_bug
        [%message
          "Type binding not generalized when diffing sigs/defs"
            (entry : Name_bindings.Name_entry.t)]
  in
  let compatible_fixities =
    Comparable.lift [%equal: Fixity.t option] ~f:Name_bindings.Name_entry.fixity
  in
  let compatible_extern_names sig_entry def_entry =
    match
      ( Name_bindings.Name_entry.extern_name sig_entry
      , Name_bindings.Name_entry.extern_name def_entry )
    with
    | None, (None | Some _) -> true
    | Some _, None -> false
    | Some sig_extern_name, Some def_extern_name ->
      Extern_name.equal sig_extern_name def_extern_name
  in
  no_errors (fun () ->
    check_val_type_schemes
      ~names
      { sig_ = get_scheme sig_entry; def = get_scheme def_entry };
    if not
         (compatible_fixities sig_entry def_entry
          && compatible_extern_names sig_entry def_entry)
    then raise Compatibility_error)
;;

let process_type_param_lists
  ~names
  ({ sig_ = sig_param_list; def = def_param_list } :
    Type_param_name.t Unique_list.t By_kind.t)
  : Name_bindings.t * _ By_kind.t
  =
  let sig_params = Type_param.Table.create () in
  let def_params = Type_param.Table.create () in
  let names =
    fold2
      (sig_param_list :> Type_param_name.t list)
      (def_param_list :> Type_param_name.t list)
      ~init:names
      ~f:(fun names sig_param def_param ->
        let names, type_ = create_skolemized_type ~names in
        Hashtbl.add_exn sig_params ~key:sig_param ~data:type_;
        Hashtbl.add_exn def_params ~key:def_param ~data:type_;
        names)
  in
  names, { sig_ = sig_params; def = def_params }
;;

let compatible_type_decls
  ~names
  ~sig_:((sig_param_list, sig_type) : _ Type_decl.t)
  ~def:((def_param_list, def_type) : _ Type_decl.t)
  =
  no_errors (fun () ->
    let names, ({ sig_ = sig_params; def = def_params } : _ By_kind.t) =
      process_type_param_lists ~names { sig_ = sig_param_list; def = def_param_list }
    in
    match sig_type, def_type with
    | Abstract, _ -> ()
    | Alias sig_scheme, Alias def_scheme ->
      check_type_decl_schemes
        ~names
        ~sig_params
        ~def_params
        { sig_ = sig_scheme, []; def = def_scheme, [] }
    | Variants cnstrs1, Variants cnstrs2 ->
      iter2 cnstrs1 cnstrs2 ~f:(fun (cnstr1, args1) (cnstr2, args2) ->
        if not (Cnstr_name.equal cnstr1 cnstr2) then raise Compatibility_error;
        iter2 args1 args2 ~f:(fun sig_scheme def_scheme ->
          check_type_decl_schemes
            ~names
            ~sig_params
            ~def_params
            { sig_ = sig_scheme, []; def = def_scheme, [] }))
    | Record _, Record _ -> failwith "TODO: record types in compatibility checks"
    (* Records, variants and (in definitions) abstract type declarations always introduce
       new types, so they are never compatible with aliases. *)
    | Record _, (Abstract | Alias _ | Variants _)
    | Variants _, (Abstract | Alias _ | Record _)
    | Alias _, (Abstract | Variants _ | Record _) -> raise Compatibility_error)
;;

let compatible_effect_decls
  ~names
  ~sig_:({ params = sig_param_list; operations = sig_operations } : _ Effect.t)
  ~def:({ params = def_param_list; operations = def_operations } : _ Effect.t)
  =
  no_errors (fun () ->
    let names, ({ sig_ = sig_params; def = def_params } : _ By_kind.t) =
      process_type_param_lists ~names { sig_ = sig_param_list; def = def_param_list }
    in
    match sig_operations, def_operations with
    | None, _ -> ()
    | Some _, None -> raise Compatibility_error
    | Some sig_operations, Some def_operations ->
      iter2
        sig_operations
        def_operations
        ~f:(fun
             { name = sig_name; args = sig_args; result = sig_result }
             { name = def_name; args = def_args; result = def_result }
           ->
        if not (Value_name.equal sig_name def_name) then raise Compatibility_error;
        check_type_decl_schemes
          ~names
          ~sig_params
          ~def_params
          { sig_ = Function (sig_args, Effect_union [], sig_result), []
          ; def = Function (def_args, Effect_union [], def_result), []
          }))
;;

(* TODO: Maybe we should get rid of [filter] and just have constructor names show up in
   defs? *)
let do_simple_diff
  ?(filter : 'name -> bool = fun _ -> true)
  (names : Name_bindings.t)
  ((name_set1, bindings1) : ('name, _) Set.t * Sigs_or_defs.t)
  ((name_set2, bindings2) : ('name, _) Set.t * Sigs_or_defs.t)
  ~(compatible : names:Name_bindings.t -> sig_:'data -> def:'data -> bool)
  ~(find : Name_bindings.t -> Sigs_or_defs.t -> 'name -> 'data)
  : ('name * 'data diff) Sequence.t
  =
  Sequence.filter_map (Set.merge_to_sequence name_set1 name_set2) ~f:(function
    | Right _ -> (* Ok to just appear in defs *) None
    | Left name -> if filter name then Some (name, Missing_from_def) else None
    | Both (name, _) ->
      if not (filter name)
      then None
      else (
        let data1, data2 = find names bindings1 name, find names bindings2 name in
        if compatible ~names ~sig_:data1 ~def:data2
        then None
        else Some (name, Incompatible (data1, data2))))
;;

let create ~names module_name =
  let rec loop ~names ~parent_module_name bindings1 bindings2 =
    let inner_names = Name_bindings.into_module names parent_module_name ~place:`Def in
    { name_diff =
        do_simple_diff
          inner_names
          (Sigs_or_defs.value_names bindings1, bindings1)
          (Sigs_or_defs.value_names bindings2, bindings2)
          ~filter:(not << Value_name.is_cnstr_name)
          ~compatible:compatible_name_entries
          ~find:Sigs_or_defs.find_entry
    ; type_diff =
        do_simple_diff
          inner_names
          (Sigs_or_defs.type_names bindings1, bindings1)
          (Sigs_or_defs.type_names bindings2, bindings2)
          ~compatible:compatible_type_decls
          ~find:Sigs_or_defs.find_type_decl
    ; effect_diff =
        do_simple_diff
          inner_names
          (Sigs_or_defs.effect_names bindings1, bindings1)
          (Sigs_or_defs.effect_names bindings2, bindings2)
          ~compatible:compatible_effect_decls
          ~find:Sigs_or_defs.find_effect_decl
    ; module_diff = do_module_diff ~names ~inner_names bindings1 bindings2
    }
  and do_module_diff ~names ~inner_names bindings1 bindings2 =
    Set.union (Sigs_or_defs.module_names bindings1) (Sigs_or_defs.module_names bindings2)
    |> Set.to_sequence
    |> Sequence.filter_map ~f:(fun module_name ->
         match
           ( Sigs_or_defs.find_module names bindings1 module_name
           , Sigs_or_defs.find_module names bindings2 module_name )
         with
         | None, Some _ -> None
         | Some _, None -> Some (module_name, Missing_module)
         | Some bindings1, Some bindings2 ->
           let module_diff =
             loop ~names:inner_names ~parent_module_name:module_name bindings1 bindings2
           in
           if is_empty module_diff
           then None
           else Some (module_name, Module_diff module_diff)
         | None, None -> compiler_bug [%message "Both sig and def module missing"])
  in
  let sigs, defs =
    Name_bindings.find_sigs_and_defs names Module_path.Relative.empty module_name
  in
  match sigs with
  | Some sigs -> loop ~names ~parent_module_name:module_name sigs defs
  | None -> empty
;;

let raise_if_nonempty t ~module_name =
  if not (is_empty t)
  then
    Compilation_error.raise
      Type_error
      ~msg:
        [%message
          "The signature of this module does not match its definition"
            (module_name : Module_name.t)
            ~_:(t : t)]
;;

let check ~names module_name = create ~names module_name |> raise_if_nonempty ~module_name
