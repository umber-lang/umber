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

let compatible_type_schemes ~names scheme1 scheme2 =
  let type1, type2 = Type.Scheme.instantiate scheme1, Type.Scheme.instantiate scheme2 in
  try
    Type_bindings.unify ~names ~types:(Type_bindings.create ()) type1 type2;
    true
  with
  | Type_bindings.Type_error _ -> false
;;

let compatible_name_entries ~names ~sig_:sig_entry ~def:def_entry =
  let get_scheme entry =
    option_or_default (Name_bindings.Name_entry.scheme entry) ~f:(fun () ->
      compiler_bug
        [%message
          "Type binding not generalized when diffing sigs/defs"
            (entry : Name_bindings.Name_entry.t)])
  in
  let sig_scheme, def_scheme = get_scheme sig_entry, get_scheme def_entry in
  compatible_type_schemes ~names sig_scheme def_scheme
  && Option.equal
       Fixity.equal
       (Name_bindings.Name_entry.fixity sig_entry)
       (Name_bindings.Name_entry.fixity def_entry)
;;

(* TODO: test/look at this for correctness, there are probably bugs here *)
let compatible_type_decls ~names ~sig_:(sig_params, sig_type) ~def:(def_params, def_type) =
  let for_all_zipped xs ys ~f =
    match List.zip xs ys with
    | Ok xys -> List.for_all xys ~f
    | Unequal_lengths -> false
  in
  let unify_schemes ~names ~types params scheme1 scheme2 =
    let expr1 = Type.Scheme.instantiate ~params scheme1 in
    let expr2 = Type.Scheme.instantiate ~params scheme2 in
    Type_bindings.unify ~names ~types expr1 expr2;
    true
  in
  let types = Type_bindings.create () in
  let param_env = Type.Param.Env_to_vars.create () in
  try
    for_all_zipped sig_params def_params ~f:(fun (sig_param, def_param) ->
      let sig_param_type = Type.Param.Env_to_vars.find_or_add param_env sig_param in
      let def_param_type = Type.Param.Env_to_vars.find_or_add param_env def_param in
      Type_bindings.unify ~names ~types (Var sig_param_type) (Var def_param_type);
      true)
    &&
    match (sig_type : Type.Decl.decl), (def_type : Type.Decl.decl) with
    | Abstract, _ -> true
    | Alias scheme1, Alias scheme2 ->
      unify_schemes ~names ~types param_env scheme1 scheme2
    | Variants cnstrs1, Variants cnstrs2 ->
      for_all_zipped cnstrs1 cnstrs2 ~f:(fun ((cnstr1, args1), (cnstr2, args2)) ->
        Cnstr_name.equal cnstr1 cnstr2
        && for_all_zipped args1 args2 ~f:(fun (arg1, arg2) ->
             unify_schemes ~names ~types param_env arg1 arg2))
    | Record _, Record _ -> failwith "TODO: record types in compatibiltiy checks"
    | Record _, (Abstract | Alias _ | Variants _)
    | Variants _, (Abstract | Alias _ | Record _)
    | Alias _, (Abstract | Variants _ | Record _) -> false
  with
  | Type_bindings.Type_error _ -> false
;;

let do_simple_diff
  ?(filter : 'name -> bool = fun _ -> true)
  (names : Name_bindings.t)
  ((name_set1, bindings1) : ('name, _) Set.t * Sigs_or_defs.t)
  ((name_set2, bindings2) : ('name, _) Set.t * Sigs_or_defs.t)
  ~(compatible : names:Name_bindings.t -> sig_:'data -> def:'data -> bool)
  ~(find : Name_bindings.t -> Sigs_or_defs.t -> 'name -> 'data)
  : ('name * 'data diff) Sequence.t
  =
  let different = Set.symmetric_diff name_set1 name_set2 in
  let same = Set.inter name_set1 name_set2 |> Set.to_sequence in
  Sequence.append
    (Sequence.filter_map different ~f:(function
      | First name -> if filter name then Some (name, Missing_from_def) else None
      | Second _ -> None))
    (Sequence.filter_map (Sequence.filter ~f:filter same) ~f:(fun name ->
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

(* TODO: remove all cases where we raise real compiler errors as just Failure. Failure
   should be reserved for todos and compiler bugs. *)
let raise_if_nonempty t = if not (is_empty t) then raise_s (sexp_of_t t)
