open Import
open Names
module Sigs_or_defs = Name_bindings.Sigs_or_defs

type 'a diff =
  | Missing_from_def
  | Incompatible of 'a * 'a
[@@deriving sexp_of]

type t =
  { name_diff : (Value_name.t * Name_bindings.Name_entry.t diff) Sequence.t
  ; type_diff : (Type_name.t * Module_path.absolute Type.Decl.t diff) Sequence.t
  ; module_diff : (Module_name.t * module_diff) Sequence.t
  }
[@@deriving sexp_of]

and module_diff =
  | Missing_module
  | Module_diff of t

let empty =
  { name_diff = Sequence.empty; type_diff = Sequence.empty; module_diff = Sequence.empty }
;;

let is_empty { name_diff; type_diff; module_diff } =
  Sequence.is_empty name_diff
  && Sequence.is_empty type_diff
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
  | (exception Compatibility_error) | (exception Type_bindings.Type_error _) -> false
;;

let iter2 xs ys ~f =
  match List.iter2 xs ys ~f with
  | Ok x -> x
  | Unequal_lengths -> raise Compatibility_error
;;

(** Skolemization means replacing all type variables in a type expression with fresh
    abstract types. e.g. `a -> b -> c` becomes something like `A -> B -> C` where `A`,
    `B`, and `C` are fresh abstract types. *)
let skomelize ~names scheme : Name_bindings.t * Type.t =
  Name_bindings.with_path_into_defs names Module_path.Absolute.empty ~f:(fun names ->
    let names = ref names in
    let types_by_param = Type.Param.Table.create () in
    let type_ =
      Type.Expr.map
        scheme
        ~var:(fun var -> compiler_bug [%message "Unskolemized var" (var : Type.Param.t)])
        ~pf:Nothing.unreachable_code
        ~name:Fn.id
        ~f:
          (function
           | Var param ->
             Halt
               (Hashtbl.find_or_add types_by_param param ~default:(fun () ->
                  let type_name = Type_name.create_skolemized () in
                  names := Name_bindings.add_type_decl !names type_name ([], Abstract);
                  Type.Expr.Type_app ((Module_path.Absolute.empty, type_name), [])))
           | type_ -> Defer type_)
    in
    !names, type_)
;;

(** A `val` item in a signature is compatible with a `let` in a defintion if the
    signature is a "more specific" version of the defintion. We can check this by
    skolemizing the signature, instatiating the defintion, and then unifying the two. *)
let check_val_type_schemes ~names ({ sig_ = sig_scheme; def = def_scheme } : _ By_kind.t) =
  (* FIXME: Do we need to absolutify type app names in aliases? Likely yes?
     Shouldn't we have done that earlier? Maybe we make absolute paths a type variable? *)
  (* let map_alias param_matching ~param_table expr =
    Type.Expr.map expr ~var:Fn.id ~pf:Nothing.unreachable_code ~f:(function
      | Var v -> Map.find_exn params v
      | typ -> Defer typ)
  in *)
  let names, sig_type = skomelize ~names sig_scheme in
  let def_type = Type.Scheme.instantiate def_scheme in
  let types = Type_bindings.create () in
  Type_bindings.unify ~names ~types sig_type def_type
;;

(* FIXME: Do we need to skolemize at the level of type declarations rather than individual
   schemes? Not sure if "mutating" the name bindings makes things go wrong. *)
(** Type definitions in a signature an defintion are compatible if they are the same
    modulo type aliases and type variable renamings. Unlike for `val`s, the compatibility
    is symmetrical. *)
let check_type_decl_schemes ~names schemes =
  check_val_type_schemes ~names schemes;
  check_val_type_schemes ~names { sig_ = schemes.def; def = schemes.sig_ }
;;

let compatible_name_entries ~names ~sig_:sig_entry ~def:def_entry =
  let get_scheme entry =
    option_or_default (Name_bindings.Name_entry.scheme entry) ~f:(fun () ->
      compiler_bug
        [%message
          "Type binding not generalized when diffing sigs/defs"
            (entry : Name_bindings.Name_entry.t)])
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

(* TODO: test/look at this for correctness, there are probably bugs here *)
let compatible_type_decls ~names ~sig_:(sig_params, sig_type) ~def:(def_params, def_type) =
  no_errors (fun () ->
    let types = Type_bindings.create () in
    let params = Type.Param.Env_to_vars.create () in
    iter2 sig_params def_params ~f:(fun sig_param def_param ->
      let sig_var = Type.Param.Env_to_vars.find_or_add params sig_param in
      let def_var = Type.Param.Env_to_vars.find_or_add params def_param in
      Type_bindings.unify ~names ~types (Var sig_var) (Var def_var));
    match (sig_type : _ Type.Decl.decl), (def_type : _ Type.Decl.decl) with
    | Abstract, _ -> ()
    | Alias sig_scheme, Alias def_scheme ->
      check_type_decl_schemes ~names { sig_ = sig_scheme; def = def_scheme }
    | Variants cnstrs1, Variants cnstrs2 ->
      iter2 cnstrs1 cnstrs2 ~f:(fun (cnstr1, args1) (cnstr2, args2) ->
        if not (Cnstr_name.equal cnstr1 cnstr2) then raise Compatibility_error;
        iter2 args1 args2 ~f:(fun sig_ def ->
          check_type_decl_schemes ~names { sig_; def }))
    | Record _, Record _ -> failwith "TODO: record types in compatibility checks"
    (* Records, variants and (in definitions) abstract type declarations always introduce
       new types, so they are never compatible with aliases. *)
    | Record _, (Abstract | Alias _ | Variants _)
    | Variants _, (Abstract | Alias _ | Record _)
    | Alias _, (Abstract | Variants _ | Record _) -> raise Compatibility_error)
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
