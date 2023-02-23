open Import
open Names
module Sigs_or_defs = Name_bindings.Sigs_or_defs

type 'a diff =
  | Missing_from_def
  | Incompatible of 'a * 'a
[@@deriving sexp_of]

type t =
  { name_diff : (Value_name.t * Name_bindings.Name_entry.t diff) Sequence.t
  ; type_diff : (Type_name.t * Type.Decl.t diff) Sequence.t
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

(* FIXME: cleanup*)
module By_kind = struct
  type 'a t =
    { sig_ : 'a
    ; def : 'a
    }
  [@@deriving sexp_of]

  (* let get t kind =
    match kind with
    | `Sig -> t.sig_
    | `Def -> t.def
  ;; *)

  let set t kind new_ =
    match kind with
    | `Sig -> { t with sig_ = new_ }
    | `Def -> { t with def = new_ }
  ;;

  (* let map t ~f = { sig_ = f t.sig_; def = f t.def }

  let map1 t kind ~f =
    match kind with
    | `Sig -> { t with sig_ = f t.sig_ }
    | `Def -> { t with def = f t.def }
  ;;

  let other = function
    | `Sig -> `Def
    | `Def -> `Sig
  ;; *)
end

exception Compatibility_error

let no_errors f =
  match f () with
  | () -> true
  | exception Compatibility_error -> false
;;

let iter2 xs ys ~f =
  match List.iter2 xs ys ~f with
  | Ok x -> x
  | Unequal_lengths -> raise Compatibility_error
;;

(* FIXME: This is wrong because it will allow something like:
   `module : { val plus_one : a -> a } let plus_one x = x + 1`.
   The issue is in using unification to treat them symmetrically.
     
   We want different handling for vars in type declarations/value declarations.
   In values, the signature must be a supertype, so it can be less general or more
   abstract. But in types, we want the declarations to match exactly. (The type in the
   def can't be more general than the one in the sig.) I think we just need different
   handling around Var.
   
   In the type def case, we want to map between the params. In the val case, we want to
   try "instantiating" each type param in the sig to what the def says.*)
(* FIXME: This needs some different logic to unification but will share some too. Try to
   extract out the common part. 
   
   Hmmm, maybe I can do something dumb to avoid having to rewrite this. How about:
   instantiate the sig and def types, then unify them, then generalize each one.

   - In the val case, we want the generalized sig case to be equal to the generalized
     val case? This can't be quite right as there's no asymmetry.
   - In the type decl case, we want them to just be equal modulo type param renaming. We
     should just assign each type param a canonical name based on position as we compare
     them. One way to do this is instantiating each type, then generalizing them again,
     then checking equality (via unification). 
*)

let check_type_schemes =
  let rec check_type_schemes ~names ~param_matching ~param_table ~schemes =
    (* FIXME: Do we need to absolutify type app names in aliases? Likely yes? *)
    (* let map_alias param_matching ~param_table expr =
    Type.Expr.map expr ~var:Fn.id ~pf:Nothing.unreachable_code ~f:(function
      | Var v -> Map.find_exn params v
      | typ -> Defer typ)
  in *)
    (* FIXME: Should we map params to other expressions or just map aliases in-place and
     use param_matching ~param_table for mapping params to type variables/integers. *)
    (* let extend_params params ~param_list ~args =
    match
      List.fold2 param_list args ~init:params ~f:(fun params param arg ->
        Map.set params ~key:param ~data:arg)
    with
    | Ok params -> params
    | Unequal_lengths -> raise Compatibility_error
  in *)
    let substitute_alias ~params ~args alias =
      let args_by_parm = List.zip_exn params args in
      Type.Expr.map alias ~var:Fn.id ~pf:Fn.id ~f:(function
        | Var var -> Halt (List.Assoc.find_exn args_by_parm var ~equal:Type.Param.equal)
        | expr -> Defer expr)
    in
    let check_type_app ~name ~args ~kind ~on_non_alias =
      match Name_bindings.find_absolute_type_decl names name with
      | params, Alias alias ->
        let alias = substitute_alias ~params ~args alias in
        check_type_schemes
          ~names
          ~param_matching
          ~param_table
          ~schemes:(By_kind.set schemes kind alias)
      | decl -> on_non_alias decl
    in
    match (schemes : Type.Scheme.t By_kind.t), param_matching with
    | { sig_ = Var sig_param; def = Var def_param }, `None ->
      if not (Type.Param.equal sig_param def_param) then raise Compatibility_error
    | { sig_ = sig_scheme; def = Var def_param }, `Lenient
    | { sig_ = Var _ as sig_scheme; def = Var def_param }, `Strict ->
      (match Hashtbl.find param_table def_param with
       | None -> Hashtbl.set param_table ~key:def_param ~data:sig_scheme
       | Some def_scheme ->
         (* FIXME: Make sure this doesn't infinite-loop. We want to express scheme
            equality, expanding aliases. *)
         check_type_schemes
           ~names
           ~param_matching:`None
           ~param_table
           ~schemes:{ sig_ = sig_scheme; def = def_scheme })
    | { sig_ = Type_app (name1, args1); def = Type_app (name2, args2) }, _ ->
      check_type_app ~name:name1 ~args:args1 ~kind:`Sig ~on_non_alias:(fun decl1 ->
        check_type_app ~name:name2 ~args:args2 ~kind:`Def ~on_non_alias:(fun decl2 ->
          if not (phys_equal decl1 decl2) then raise Compatibility_error;
          iter2 args1 args2 ~f:(fun sig_ def ->
            check_type_schemes ~names ~param_matching ~param_table ~schemes:{ sig_; def })))
    | { sig_ = Type_app (name, args); def = Tuple _ | Function _ | Partial_function _ }, _
    | { sig_ = Type_app (name, args); def = Var _ }, `None ->
      check_type_app ~name ~args ~kind:`Sig ~on_non_alias:(fun _ ->
        raise Compatibility_error)
    | ( { sig_ = Var _ | Tuple _ | Function _ | Partial_function _
        ; def = Type_app (name, args)
        }
      , _ ) ->
      check_type_app ~name ~args ~kind:`Def ~on_non_alias:(fun _ ->
        raise Compatibility_error)
    | { sig_ = Function (args1, res1); def = Function (args2, res2) }, _ ->
      iter2 (Nonempty.to_list args1) (Nonempty.to_list args2) ~f:(fun sig_ def ->
        check_type_schemes ~names ~param_matching ~param_table ~schemes:{ sig_; def });
      check_type_schemes
        ~names
        ~param_matching
        ~param_table
        ~schemes:{ sig_ = res1; def = res2 }
    | { sig_ = Tuple args1; def = Tuple args2 }, _ ->
      iter2 args1 args2 ~f:(fun sig_ def ->
        check_type_schemes ~names ~param_matching ~param_table ~schemes:{ sig_; def })
    | ( ( { sig_ = Var _; def = Tuple _ | Function _ }
        | { sig_ = Tuple _; def = Function _ }
        | { sig_ = Function _; def = Tuple _ } )
      , _ )
    | { sig_ = Type_app _ | Tuple _ | Function _; def = Var _ }, `Strict
    | { sig_ = Tuple _ | Function _; def = Var _ }, `None -> raise Compatibility_error
    | ({ sig_ = Partial_function _; def = _ } | { sig_ = _; def = Partial_function _ }), _
      -> .
  in
  fun ~names ~param_matching ~schemes ->
    check_type_schemes
      ~names
      ~param_matching
      ~param_table:(Type.Param.Table.create ())
      ~schemes
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
  (* FIXME: The behavior we want is subtyping on variables: variables in the def may get
     mapped to concrete types. Similar to unification, but type variables in the def can't
     get unified with each other, since that would change the type. So something like some
     kind of fold2 would help.
     
     Probably the easiest way to write this is with mutable param maps and then raise
     exceptions if there are conflicts. *)
  no_errors (fun () ->
    check_type_schemes
      ~names
      ~param_matching:`Lenient
      ~schemes:{ sig_ = get_scheme sig_entry; def = get_scheme def_entry };
    if not
         (compatible_fixities sig_entry def_entry
         && compatible_extern_names sig_entry def_entry)
    then raise Compatibility_error)
;;

(* TODO: test/look at this for correctness, there are probably bugs here *)
let compatible_type_decls ~names ~sig_:(sig_params, sig_type) ~def:(def_params, def_type) =
  (* FIXME: I think this has a bug where it will say these are not compatible:
     ```
       module :
         val id : b -> b
       val id : a -> a
       let id x = x
     ``` 
     Since it uses the same env for each, the names have to match up, which seems wrong.
     Should write a test to demonstrate this, then fix it. *)
  (* FIXME: This shouldn't be able to come up with constraints between different type
     parameters e.g. that they're equal.
     
     Proper idea: For types to be compatible, they must be the same, modulo Abstract and
     type parameter renamingn (the important thing is the position of the parameters).
     Meaning if you replace the type parameters in one with the other they should then be
     equal.
     
     So, we should just come up with the renaming by zipping over the parameter lists,
     then we're good. def -> sig renaming must work 
     
     Actually not quite: For val definitions we allow types to be more general in the def
     than in the sig, but not in type declarations. *)
  (* FIXME: Should put in type param mappings instead of unifying *)
  no_errors (fun () ->
    let types = Type_bindings.create () in
    let params = Type.Param.Env_to_vars.create () in
    iter2 sig_params def_params ~f:(fun sig_param def_param ->
      let sig_var = Type.Param.Env_to_vars.find_or_add params sig_param in
      let def_var = Type.Param.Env_to_vars.find_or_add params def_param in
      Type_bindings.unify ~names ~types (Var sig_var) (Var def_var));
    match (sig_type : Type.Decl.decl), (def_type : Type.Decl.decl) with
    | Abstract, _ -> ()
    | Alias sig_scheme, Alias def_scheme ->
      (* FIXME: Instead of trying to map def params to sig types, we want to map def
         params to sig *params* only. *)
      check_type_schemes
        ~names
        ~param_matching:`Strict
        ~schemes:{ sig_ = sig_scheme; def = def_scheme }
    | Variants cnstrs1, Variants cnstrs2 ->
      iter2 cnstrs1 cnstrs2 ~f:(fun (cnstr1, args1) (cnstr2, args2) ->
        if not (Cnstr_name.equal cnstr1 cnstr2) then raise Compatibility_error;
        iter2 args1 args2 ~f:(fun sig_ def ->
          check_type_schemes ~names ~param_matching:`Strict ~schemes:{ sig_; def }))
    | Record _, Record _ -> failwith "TODO: record types in compatibiltiy checks"
    (* FIXME: Should be following aliases, maybe use check_type_app *)
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
  let rec loop names bindings1 bindings2 =
    { name_diff =
        do_simple_diff
          names
          (Sigs_or_defs.value_names bindings1, bindings1)
          (Sigs_or_defs.value_names bindings2, bindings2)
          ~filter:(not << Value_name.is_cnstr_name)
          ~compatible:compatible_name_entries
          ~find:Sigs_or_defs.find_entry
    ; type_diff =
        do_simple_diff
          names
          (Sigs_or_defs.type_names bindings1, bindings1)
          (Sigs_or_defs.type_names bindings2, bindings2)
          ~compatible:compatible_type_decls
          ~find:Sigs_or_defs.find_type_decl
    ; module_diff = do_module_diff names bindings1 bindings2
    }
  and do_module_diff names bindings1 bindings2 =
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
           let module_diff = loop names bindings1 bindings2 in
           if is_empty module_diff
           then None
           else Some (module_name, Module_diff module_diff)
         | None, None -> compiler_bug [%message "Both sig and def module missing"])
  in
  let sigs, defs = Name_bindings.find_sigs_and_defs names [] module_name in
  match sigs with
  | Some sigs -> loop names sigs defs
  | None -> empty
;;

let raise_if_nonempty t =
  if not (is_empty t) then Compilation_error.raise ~msg:(sexp_of_t t) Type_error
;;
