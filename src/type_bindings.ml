open! Import
open! Names

(* FIXME: cleanup *)
let eprint_s = ignore

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
   GADTs (local type equality/type narrowing)
   Some of these features can make local let-generalization difficult, see:
   https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
   for an argument for just abolishing local let-generalization *)

(* TODO: Consider integrating source locations into stored types to give better type
   errors. *)

(** Prevent unstable [Type_var.t]s from appearing in test output sexps. *)
let replace_vars_in_sexp env sexp =
  (* FIXME: Need a way to show an internal type with stable names for the variables.
     Hmm, maybe it would be good enough if each type expression had its own variable scope,
     instead of it being global. Maybe generalize what Mir_name.Name_table does? Or make a
     wrapper for Unique_id.Int ()? Or an easier way: map over the sexp. *)
  (* TODO: This is a hack to remove unstable type variables from the output in tests. 
     We should probably have type variables be scopied properly to let binding groups for
     one [Type_bindings.t]. We also need to think of a smarter way to display an
     [Internal_type.t] in a user-friendly format. (A sexp is definitely not it.) *)
  let rec loop : Sexp.t -> Sexp.t = function
    | List sexps -> List (List.map sexps ~f:loop)
    | Atom str as atom ->
      (match Type_var.of_string str with
       | var -> [%sexp (Type_param.Env_of_vars.find_or_add env var : Type_param_name.t)]
       | exception _ -> atom)
  in
  loop sexp
;;

let type_error msg t1 t2 =
  let env = Type_param.Env_of_vars.create () in
  Compilation_error.raise
    Type_error
    ~msg:
      [%message
        msg
          ~type1:(replace_vars_in_sexp env [%sexp (t1 : Internal_type.t)] : Sexp.t)
          ~type2:(replace_vars_in_sexp env [%sexp (t2 : Internal_type.t)] : Sexp.t)]
;;

type 'a var_bounds =
  { lower_bounds : 'a Queue.t
  ; upper_bounds : 'a Queue.t
  }
[@@deriving sexp_of]

module Effect_type = struct
  module T = struct
    type t = Internal_type.effects [@@deriving hash, compare, sexp]
  end

  include T
  include Hashable.Make (T)
end

module Type_pair = struct
  include Tuple.Make (Internal_type) (Internal_type)
  include Tuple.Hashable (Internal_type) (Internal_type)
end

module Effect_type_pair = struct
  include Tuple.Make (Effect_type) (Effect_type)
  include Tuple.Hashable (Effect_type) (Effect_type)
end

module Constraints : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val add : t -> subtype:Type_var.t -> supertype:Type_var.t -> unit

  (* FIXME: Make this interface less confusing*)
  val remove_vars : t -> Type_var.t -> Type_var.Set.t -> unit
  val find_vars_with_same_shape : t -> Type_var.t -> Type_var.Set.t

  val to_constraint_list
    :  t
    -> params:Type_param.Env_of_vars.t
    -> Type_scheme.constraint_ list

  val print : t -> unit
end = struct
  type t =
    { lower_bounds : Type_var.Hash_set.t Type_var.Table.t
    ; upper_bounds : Type_var.Hash_set.t Type_var.Table.t
    }
  [@@deriving sexp_of]

  let create () =
    { lower_bounds = Type_var.Table.create (); upper_bounds = Type_var.Table.create () }
  ;;

  let get_bounds bounds var =
    Hashtbl.find_or_add bounds var ~default:(fun () ->
      let vars = Type_var.Hash_set.create () in
      Hash_set.add vars var;
      vars)
  ;;

  let add_bounds { lower_bounds; upper_bounds } ~subtype ~supertype =
    Hash_set.add (get_bounds upper_bounds subtype) supertype;
    Hash_set.add (get_bounds lower_bounds supertype) subtype
  ;;

  (* If we get a constraint [a <: b], we also need to add all constraints transitively
     implied by that, so that the constraints remain closed under logical implication.
     So we need to add all constraints of the form a' <: b' for all a' <: a and b' >: b. *)
  let add t ~subtype ~supertype =
    List.iter
      (subtype :: Hash_set.to_list (get_bounds t.lower_bounds subtype))
      ~f:(fun subtype' ->
        List.iter
          (supertype :: Hash_set.to_list (get_bounds t.upper_bounds supertype))
          ~f:(fun supertype' -> add_bounds t ~subtype:subtype' ~supertype:supertype'))
  ;;

  let remove_vars { lower_bounds; upper_bounds } var vars =
    Hash_set.filter_inplace (get_bounds lower_bounds var) ~f:(not << Set.mem vars);
    Hash_set.filter_inplace (get_bounds upper_bounds var) ~f:(not << Set.mem vars)
  ;;

  let find_vars_with_same_shape { lower_bounds; upper_bounds } var =
    let rec loop collected_vars var =
      let add_vars_from_bounds bounds collected_vars =
        Hash_set.fold
          (get_bounds bounds var)
          ~init:collected_vars
          ~f:(fun collected_vars var ->
          if Set.mem collected_vars var then collected_vars else loop collected_vars var)
      in
      Set.add collected_vars var
      |> add_vars_from_bounds lower_bounds
      |> add_vars_from_bounds upper_bounds
    in
    loop Type_var.Set.empty var
  ;;

  let to_constraint_list t ~params =
    Hashtbl.to_alist t.upper_bounds
    |> List.concat_map ~f:(fun (subtype, supertypes) ->
         let subtype_relevant = Type_param.Env_of_vars.mem params subtype in
         let subtype = Type_param.Env_of_vars.find_or_add params subtype in
         Hash_set.to_list supertypes
         |> List.filter_map ~f:(fun supertype : Type_scheme.constraint_ option ->
              let supertype_relevant = Type_param.Env_of_vars.mem params supertype in
              if subtype_relevant || supertype_relevant
              then (
                let supertype = Type_param.Env_of_vars.find_or_add params supertype in
                if Type_param_name.equal subtype supertype
                then None
                else Some { subtype; supertype })
              else None))
  ;;

  let print t =
    Hashtbl.to_alist t.upper_bounds
    |> List.sort ~compare:[%compare: Type_var.t * _]
    |> List.iter ~f:(fun (subtype, supertypes) ->
         let supertypes =
           Type_var.Set.of_hash_set supertypes |> Fn.flip Set.remove subtype
         in
         if not (Set.is_empty supertypes)
         then (
           let supertypes =
             Set.to_list supertypes
             |> List.sort ~compare:Type_var.compare
             |> List.map ~f:Type_var.to_string
             |> String.concat ~sep:", "
           in
           print_endline [%string "%{subtype#Type_var} <: %{supertypes}"]))
  ;;
end

module Substitution : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val set_type : t -> Type_var.t -> Internal_type.t -> unit
  val set_effects : t -> Type_var.t -> Internal_type.effects -> unit
  val compose : t -> t -> unit
  val apply_to_type : t -> Internal_type.t -> Internal_type.t
  val apply_to_effects : t -> Internal_type.effects -> Internal_type.effects
  val is_empty : t -> bool
end = struct
  type data =
    | Type of Internal_type.t
    | Effects of Internal_type.effects
  [@@deriving sexp_of]

  type t = data Type_var.Table.t [@@deriving sexp_of]

  let create () = Type_var.Table.create ()
  let set_type t var type_ = Hashtbl.set t ~key:var ~data:(Type type_)
  let set_effects t var effects = Hashtbl.set t ~key:var ~data:(Effects effects)

  let rec apply_to_type t type_ =
    Internal_type.map type_ ~f:(function
      | Var var -> Halt (apply_to_type_var t var)
      | Function (args, effects, result) ->
        Halt
          (Function
             ( Nonempty.map args ~f:(apply_to_type t)
             , apply_to_effects t effects
             , apply_to_type t result ))
      | Partial_function (args, effects, result_var) ->
        let args = Nonempty.map args ~f:(apply_to_type t) in
        let effects = apply_to_effects t effects in
        (match apply_to_type_var t result_var with
         | Var result_var -> Halt (Partial_function (args, effects, result_var))
         | Partial_function (args', effects', result_var) as result_type ->
           (* Effects must be pure for us to combine partial functions. However, we don't
              know at this stage whether effects are pure. For now, let's do something
              naive. *)
           if Internal_type.equal_effects effects Internal_type.no_effects
           then Halt (Partial_function (Nonempty.append args args', effects', result_var))
           else Halt (Function (args, effects, result_type))
         | (Function _ | Type_app _ | Tuple _) as result_type ->
           Halt (Function (args, effects, result_type)))
      | (Type_app _ | Tuple _) as type_ -> Defer type_)

  and apply_to_effects t ({ effects; effect_var } : Internal_type.effects)
    : Internal_type.effects
    =
    let effects = Map.map effects ~f:(List.map ~f:(apply_to_type t)) in
    match Option.map effect_var ~f:(apply_to_effect_var t) with
    | None -> { effects; effect_var }
    | Some
        ({ effects = new_effects; effect_var = new_effect_var } : Internal_type.effects)
      ->
      { effects =
          Map.merge_skewed
            effects
            new_effects
            ~combine:(fun ~key:effect_name args args' ->
            (* TODO: We almost surely want some kind of unification here. *)
            if [%equal: Internal_type.t list] args args'
            then args
            else
              Compilation_error.raise
                Type_error
                ~msg:
                  [%message
                    "Multiple instances of the same effect with differing types"
                      (effect_name : Effect_name.Absolute.t)
                      (args : Internal_type.t list)
                      (args' : Internal_type.t list)])
      ; effect_var = new_effect_var
      }

  and apply_to_type_var t var =
    match Hashtbl.find t var with
    | Some (Type type_) -> apply_to_type t type_
    | None -> Var var
    | Some (Effects _) -> compiler_bug [%message "Expected type, got effects"]

  and apply_to_effect_var t var =
    match Hashtbl.find t var with
    | Some (Effects effects) -> apply_to_effects t effects
    | None -> { effects = Effect_name.Absolute.Map.empty; effect_var = Some var }
    | Some (Type _) -> compiler_bug [%message "Expected effects, got type"]
  ;;

  let compose t t' =
    Hashtbl.iteri t' ~f:(fun ~key:var ~data:new_value ->
      Hashtbl.update t var ~f:(function
        | None -> new_value
        | Some (Type type_) -> Type (apply_to_type t' type_)
        | Some (Effects effects) -> Effects (apply_to_effects t' effects)))
  ;;

  let is_empty = Hashtbl.is_empty
end

type t =
  { constraints : Constraints.t
  ; substitution : Substitution.t
  ; constrained_types : Type_pair.Hash_set.t [@sexp_drop_if const true]
  ; constrained_effects : Effect_type_pair.Hash_set.t [@sexp_drop_if const true]
  }
[@@deriving sexp_of]

let create () =
  { constraints = Constraints.create ()
  ; substitution = Substitution.create ()
  ; constrained_types = Type_pair.Hash_set.create ()
  ; constrained_effects = Effect_type_pair.Hash_set.create ()
  }
;;

let rec occurs_in id : Internal_type.t -> bool = function
  | Var id' -> Type_var.(id = id')
  | Type_app (_, fields) | Tuple fields -> List.exists fields ~f:(occurs_in id)
  | Function (args, effects, body) ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effects id effects
    || occurs_in id body
  | Partial_function (args, effects, id') ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effects id effects
    || Type_var.(id = id')

and occurs_in_effects id ({ effects; effect_var } : Internal_type.effects) =
  Map.exists effects ~f:(List.exists ~f:(occurs_in id))
  || Option.exists effect_var ~f:(Type_var.( = ) id)
;;

let fun_arg_number_mismatch = type_error "Function argument number mismatch"

let iter2_types xs ys ~subtype ~supertype ~f =
  match List.iter2 ~f xs ys with
  | Ok () -> ()
  | Unequal_lengths -> type_error "Type item length mismatch" subtype supertype
;;

let refresh_type type_ =
  let vars = Type_var.Table.create () in
  let refresh_var = Hashtbl.find_or_add vars ~default:Type_var.create in
  Internal_type.map_vars type_ ~f:refresh_var
;;

(* FIXME: Just use this for types, not effects *)
let check_var_vs_type
  ~names
  ~types
  ~var
  ~type_
  ~var_side
  ~constrain
  ~to_var
  ~set_substitution
  ~refresh
  =
  let orient f ~var ~type_ =
    match var_side with
    | `Left -> f (to_var var) type_
    | `Right -> f type_ (to_var var)
  in
  let vars_with_same_shape =
    Constraints.find_vars_with_same_shape types.constraints var
  in
  Set.iter vars_with_same_shape ~f:(fun var ->
    Constraints.remove_vars types.constraints var vars_with_same_shape);
  let new_var_substitution =
    let substitution = Substitution.create () in
    Set.iter vars_with_same_shape ~f:(fun var ->
      set_substitution substitution var (refresh type_));
    substitution
  in
  (* FIXME: cleanup *)
  eprint_s
    [%message
      "check_var_vs_type"
        (var : Type_var.t)
        (var_side : [ `Left | `Right ])
        (vars_with_same_shape : Type_var.Set.t)
        (new_var_substitution : Substitution.t)];
  Substitution.compose types.substitution new_var_substitution;
  (* After applying the substitution, add constraints between the refreshed types and
     their original. The refreshed type will be substituted in using the substitution to
     take the place of the var. *)
  Set.iter vars_with_same_shape ~f:(fun var ->
    orient ~var ~type_ (fun subtype supertype ->
      constrain ~names ~types ~subtype ~supertype));
  (* FIXME: Should we be skipping this constrain check if v = v'?
     (Maybe we should put an extra check in [constrain] for that case actually.) *)
  Set.iter vars_with_same_shape ~f:(fun v ->
    Set.iter vars_with_same_shape ~f:(fun v' ->
      constrain ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')))
;;

let rec constrain ~names ~types ~subtype ~supertype =
  let instantiate_alias (param_list : Type_param_name.t Unique_list.t) expr =
    (* FIXME: Isn't this code setting up [params] redundant? *)
    let params = Type_param.Env_to_vars.create () in
    List.iter
      (param_list :> Type_param_name.t list)
      ~f:(fun p -> ignore (Type_param.Env_to_vars.find_or_add params p : Type_var.t));
    instantiate_type_scheme ~names ~types expr ~params
  in
  let lookup_type names name args =
    let type_entry = Name_bindings.find_absolute_type_entry names name in
    if List.length args = Type_decl.arity (Name_bindings.Type_entry.decl type_entry)
    then type_entry
    else type_error "Partially applied type constructor" subtype supertype
  in
  eprint_s
    [%message
      "Type_bindings.constrain_internal (pre-substitution)"
        (subtype : Internal_type.t)
        (supertype : Internal_type.t)
        (types.constraints : Constraints.t)
        (types.substitution : Substitution.t)];
  (* It's important to apply the substitution before checking [constrained_types] since
     the substitution can change as we do more [constrain] calls. *)
  let subtype = Substitution.apply_to_type types.substitution subtype in
  let supertype = Substitution.apply_to_type types.substitution supertype in
  if not (Hash_set.mem types.constrained_types (subtype, supertype))
  then (
    (* FIXME: cleanup *)
    eprint_s
      [%message
        "Type_bindings.constrain (post-substitution)"
          (subtype : Internal_type.t)
          (supertype : Internal_type.t)];
    Hash_set.add types.constrained_types (subtype, supertype);
    match subtype, supertype with
    | Var var1, Var var2 ->
      if not (Type_var.equal var1 var2)
      then Constraints.add types.constraints ~subtype:var1 ~supertype:var2
    | Var var, type_ ->
      (* FIXME: Need a more robust occurs check *)
      if occurs_in var type_ then (type_error "Occurs check failed") subtype supertype;
      check_var_vs_type
        ~names
        ~types
        ~var
        ~type_
        ~var_side:`Left
        ~constrain
        ~to_var:Internal_type.var
        ~set_substitution:Substitution.set_type
        ~refresh:refresh_type
    | type_, Var var ->
      (* FIXME: Need a more robust occurs check *)
      if occurs_in var type_ then (type_error "Occurs check failed") subtype supertype;
      check_var_vs_type
        ~names
        ~types
        ~var
        ~type_
        ~var_side:`Right
        ~constrain
        ~to_var:Internal_type.var
        ~set_substitution:Substitution.set_type
        ~refresh:refresh_type
    | Type_app (name1, args1), Type_app (name2, args2) ->
      let type_entry1 = lookup_type names name1 args1 in
      (match Name_bindings.Type_entry.decl type_entry1 with
       | params, Alias expr ->
         constrain ~names ~types ~subtype:(instantiate_alias params (expr, [])) ~supertype
       | (_ : _ Type_decl.t) ->
         let type_entry2 = lookup_type names name2 args2 in
         (match Name_bindings.Type_entry.decl type_entry2 with
          | params, Alias expr ->
            constrain
              ~names
              ~types
              ~subtype
              ~supertype:(instantiate_alias params (expr, []))
          | (_ : _ Type_decl.t) ->
            if not (Name_bindings.Type_entry.identical type_entry1 type_entry2)
            then type_error "Type application mismatch" subtype supertype;
            (* FIXME: Improve this *)
            (* TODO: We don't know what the variance of the type parameters to the type are,
             so we conservatively assume they are invariant. Implement inference and
             manual specification of type parameter variance, similar to what OCaml does. *)
            iter2_types args1 args2 ~subtype ~supertype ~f:(fun arg1 arg2 ->
              constrain ~names ~types ~subtype:arg1 ~supertype:arg2
              (* constrain ~names ~types ~subtype:arg2 ~supertype:arg1 *))))
    | Type_app (name, args), (Tuple _ | Function _ | Partial_function _) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | params, Alias expr ->
         constrain ~names ~types ~subtype:(instantiate_alias params (expr, [])) ~supertype
       | _ -> type_error "Type application mismatch" subtype supertype)
    | (Tuple _ | Function _ | Partial_function _), Type_app (name, args) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | params, Alias expr ->
         constrain ~names ~types ~subtype ~supertype:(instantiate_alias params (expr, []))
       | _ -> type_error "Type application mismatch" subtype supertype)
    | Function (args1, effects1, res1), Function (args2, effects2, res2) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Same_length -> ()
       | Left_trailing _ | Right_trailing _ -> fun_arg_number_mismatch subtype supertype);
      constrain_effects ~names ~types ~subtype:effects1 ~supertype:effects2;
      constrain ~names ~types ~subtype:res1 ~supertype:res2
    | Partial_function (args1, effects1, var1), Partial_function (args2, effects2, var2)
      ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing args1_trailing ->
         (* The left function has more arguments, so the right function is currently
            under-applied and must be total at this point. *)
         constrain_effects_to_be_total ~names ~types effects2;
         constrain
           ~names
           ~types
           ~subtype:(Partial_function (args1_trailing, effects1, var1))
           ~supertype:(Var var2)
       | Right_trailing args2_trailing ->
         (* Similar to the above case, the left function is under-applied, so its effects
            must be total. *)
         constrain_effects_to_be_total ~names ~types effects1;
         constrain
           ~names
           ~types
           ~subtype:(Var var1)
           ~supertype:(Partial_function (args2_trailing, effects2, var2))
       | Same_length ->
         constrain_effects ~names ~types ~subtype:effects1 ~supertype:effects2;
         constrain ~names ~types ~subtype:(Var var1) ~supertype:(Var var2))
    | Partial_function (args1, effects1, var), Function (args2, effects2, res) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing _ -> fun_arg_number_mismatch subtype supertype
       | Right_trailing args2_trailing ->
         constrain_effects_to_be_total ~names ~types effects1;
         let var' = Type_var.create () in
         constrain ~names ~types ~subtype:(Var var') ~supertype:res;
         constrain
           ~names
           ~types
           ~subtype:(Var var)
           ~supertype:(Partial_function (args2_trailing, effects2, var'))
       | Same_length ->
         constrain_effects ~names ~types ~subtype:effects1 ~supertype:effects2;
         constrain ~names ~types ~subtype:(Var var) ~supertype:res)
    | Function (args1, effects1, res), Partial_function (args2, effects2, var) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing args1_trailing ->
         constrain_effects_to_be_total ~names ~types effects2;
         let var' = Type_var.create () in
         constrain ~names ~types ~subtype:res ~supertype:(Var var');
         constrain
           ~names
           ~types
           ~subtype:(Partial_function (args1_trailing, effects1, var'))
           ~supertype:(Var var)
       | Right_trailing _ -> fun_arg_number_mismatch subtype supertype
       | Same_length ->
         constrain_effects ~names ~types ~subtype:effects1 ~supertype:effects2;
         constrain ~names ~types ~subtype:res ~supertype:(Var var))
    | Tuple xs, Tuple ys ->
      iter2_types xs ys ~subtype ~supertype ~f:(fun arg1 arg2 ->
        constrain ~names ~types ~subtype:arg1 ~supertype:arg2)
    | Tuple _, (Function _ | Partial_function _)
    | Function _, Tuple _
    | Partial_function _, Tuple _ -> type_error "Types do not match" subtype supertype)

and constrain_effects ~names ~types ~subtype ~supertype =
  eprint_s
    [%message
      "Type_bindings.constrain_effects (pre-substitution)"
        (subtype : Internal_type.effects)
        (supertype : Internal_type.effects)
        (types.constraints : Constraints.t)
        (types.substitution : Substitution.t)];
  let subtype = Substitution.apply_to_effects types.substitution subtype in
  let supertype = Substitution.apply_to_effects types.substitution supertype in
  if not (Hash_set.mem types.constrained_effects (subtype, supertype))
  then (
    eprint_s
      [%message
        "Type_bindings.constrain_effects (post-substitution)"
          (subtype : Internal_type.effects)
          (supertype : Internal_type.effects)];
    Hash_set.add types.constrained_effects (subtype, supertype);
    let ({ effects = subtype_effects; effect_var = subtype_var } : Internal_type.effects) =
      subtype
    and ({ effects = supertype_effects; effect_var = supertype_var }
          : Internal_type.effects)
      =
      supertype
    in
    let subtype_only, supertype_only =
      Map.fold_symmetric_diff
        subtype_effects
        supertype_effects
        ~init:(Effect_name.Absolute.Map.empty, Effect_name.Absolute.Map.empty)
        ~data_equal:(fun _ _ -> false)
        ~f:(fun (subtype_only, supertype_only) (effect_name, diff) ->
          match diff with
          | `Left args ->
            Map.add_exn subtype_only ~key:effect_name ~data:args, supertype_only
          | `Right args ->
            subtype_only, Map.add_exn supertype_only ~key:effect_name ~data:args
          | `Unequal (args1, args2) ->
            (match
               (* TODO: Handle type parameter variance for effects. *)
               List.iter2 args1 args2 ~f:(fun arg1 arg2 ->
                 constrain ~names ~types ~subtype:arg1 ~supertype:arg2;
                 constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
             with
             | Ok () -> subtype_only, supertype_only
             | Unequal_lengths ->
               compiler_bug [%message "Unequal number of arguments to effect types"]))
    in
    let constrain_effect_vars types ~subtype_var ~supertype_var =
      match subtype_var, supertype_var with
      | Some subtype, Some supertype ->
        Constraints.add types.constraints ~subtype ~supertype
      | None, None | None, Some _ -> ()
      | Some subtype_var, None ->
        Substitution.set_effects types.substitution subtype_var Internal_type.no_effects
    in
    let create_substitution ~effects ~var ~new_var ~effects_subtyping_dir =
      let substitution = Substitution.create () in
      if not (Map.is_empty effects)
      then
        Option.iter var ~f:(fun var ->
          let effects =
            Map.map effects ~f:(fun args ->
              List.map args ~f:(fun arg ->
                let arg' = refresh_type arg in
                (match effects_subtyping_dir with
                 | `Subtype -> constrain ~names ~types ~subtype:arg ~supertype:arg'
                 | `Supertype -> constrain ~names ~types ~subtype:arg' ~supertype:arg);
                arg'))
          in
          Substitution.set_effects substitution var { effects; effect_var = new_var });
      substitution
    in
    if Map.is_empty subtype_only && Map.is_empty supertype_only
    then constrain_effect_vars types ~subtype_var ~supertype_var
    else if (not (Map.is_empty subtype_only)) && Option.is_none supertype_var
    then
      Compilation_error.raise
        Type_error
        ~msg:
          [%message
            "Found more effects than expected"
              ~_:(subtype_only : Internal_type.t list Effect_name.Absolute.Map.t)]
    else (
      (* FIXME: Not handling union of O and O', see paper. *)
      let new_subtype_var = Option.map subtype_var ~f:(fun _ -> Type_var.create ()) in
      let new_supertype_var = Option.map supertype_var ~f:(fun _ -> Type_var.create ()) in
      let supertype_only_substitution =
        create_substitution
          ~effects:supertype_only
          ~var:subtype_var
          ~new_var:new_subtype_var
          ~effects_subtyping_dir:`Supertype
      in
      let subtype_only_substitution =
        create_substitution
          ~effects:subtype_only
          ~var:supertype_var
          ~new_var:new_supertype_var
          ~effects_subtyping_dir:`Subtype
      in
      constrain_effect_vars
        types
        ~subtype_var:new_subtype_var
        ~supertype_var:new_supertype_var;
      (* Remove all constraints between vars of the same shape as either var here. *)
      let vars_with_same_shape_as_subtype_var =
        match subtype_var with
        | Some var -> Constraints.find_vars_with_same_shape types.constraints var
        | None -> Type_var.Set.empty
      in
      Set.iter vars_with_same_shape_as_subtype_var ~f:(fun var ->
        Constraints.remove_vars types.constraints var vars_with_same_shape_as_subtype_var);
      let vars_with_same_shape_as_supertype_var =
        match supertype_var with
        | Some var -> Constraints.find_vars_with_same_shape types.constraints var
        | None -> Type_var.Set.empty
      in
      Set.iter vars_with_same_shape_as_supertype_var ~f:(fun var ->
        Constraints.remove_vars
          types.constraints
          var
          vars_with_same_shape_as_supertype_var);
      Substitution.compose types.substitution subtype_only_substitution;
      Substitution.compose types.substitution supertype_only_substitution;
      let to_var = Internal_type.effects_of_var in
      Set.iter vars_with_same_shape_as_subtype_var ~f:(fun v ->
        Set.iter vars_with_same_shape_as_subtype_var ~f:(fun v' ->
          constrain_effects ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')));
      Set.iter vars_with_same_shape_as_supertype_var ~f:(fun v ->
        Set.iter vars_with_same_shape_as_supertype_var ~f:(fun v' ->
          constrain_effects ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')))
      (* FIXME: Do we need an occurs check for effects? *)))

and constrain_effects_to_be_total ~names ~types effects =
  constrain_effects ~names ~types ~subtype:effects ~supertype:Internal_type.no_effects

and instantiate_type_scheme =
  let rec instantiate_type_scheme
    ~names
    ~types
    ~params
    (scheme : Module_path.absolute Type_scheme.type_)
    : Internal_type.t
    =
    match scheme with
    | Var param -> Var (Type_param.Env_to_vars.find_or_add params param)
    | Type_app (type_name, args) ->
      Type_app
        (type_name, List.map args ~f:(instantiate_type_scheme ~names ~types ~params))
    | Tuple fields ->
      Tuple (List.map fields ~f:(instantiate_type_scheme ~names ~types ~params))
    | Function (args, effects, result) ->
      let args = Nonempty.map args ~f:(instantiate_type_scheme ~names ~types ~params) in
      let effects = instantiate_effect_type_scheme ~names ~types ~params effects in
      Function (args, effects, instantiate_type_scheme ~names ~types ~params result)
    | Union [] ->
      (* The empty union is the bottom type, the type which is a subtype of all other
         types. We can get equivalent behavior by using an unconstrained type variable. *)
      Var (Type_var.create ())
    | Union args ->
      let var = Internal_type.fresh_var () in
      let args =
        Non_single_list.map args ~f:(instantiate_type_scheme ~names ~types ~params)
      in
      Non_single_list.iter args ~f:(fun subtype ->
        constrain ~names ~types ~subtype ~supertype:var);
      var
    | Intersection args ->
      let var = Internal_type.fresh_var () in
      let args =
        Non_single_list.map args ~f:(instantiate_type_scheme ~names ~types ~params)
      in
      Non_single_list.iter args ~f:(fun supertype ->
        constrain ~names ~types ~subtype:var ~supertype);
      var
  and instantiate_effect_type_scheme ~names ~types ~params effects =
    match effects with
    | Effect (effect_name, args) ->
      { effects =
          Effect_name.Absolute.Map.singleton
            effect_name
            (List.map args ~f:(instantiate_type_scheme ~names ~types ~params))
      ; effect_var = None
      }
    | Effect_var param ->
      let var = Type_param.Env_to_vars.find_or_add params param in
      { effects = Effect_name.Absolute.Map.empty; effect_var = Some var }
    | Effect_union [] | Effect_intersection [] ->
      { effects = Effect_name.Absolute.Map.empty; effect_var = None }
    | Effect_union args ->
      let var : Internal_type.effects =
        { effects = Effect_name.Absolute.Map.empty
        ; effect_var = Some (Type_var.create ())
        }
      in
      let args =
        Non_single_list.map args ~f:(instantiate_effect_type_scheme ~names ~types ~params)
      in
      Non_single_list.iter args ~f:(fun subtype ->
        constrain_effects ~names ~types ~subtype ~supertype:var);
      var
    | Effect_intersection args ->
      let var : Internal_type.effects =
        { effects = Effect_name.Absolute.Map.empty
        ; effect_var = Some (Type_var.create ())
        }
      in
      let args =
        Non_single_list.map args ~f:(instantiate_effect_type_scheme ~names ~types ~params)
      in
      Non_single_list.iter args ~f:(fun supertype ->
        constrain_effects ~names ~types ~subtype:var ~supertype);
      var
  in
  fun ?(params = Type_param.Env_to_vars.create ())
      ~names
      ~types
      ((scheme, constraints) : Module_path.absolute Type_scheme.t) ->
    List.iter constraints ~f:(fun { subtype; supertype } ->
      constrain
        ~names
        ~types
        ~subtype:(instantiate_type_scheme ~names ~types ~params (Var subtype))
        ~supertype:(instantiate_type_scheme ~names ~types ~params (Var supertype)));
    instantiate_type_scheme ~names ~types scheme ~params
;;

let instantiate_type_or_scheme
  ?params
  ~names
  ~types
  (type_or_scheme : Name_bindings.Name_entry.Type_or_scheme.t)
  =
  match type_or_scheme with
  | Type type_ -> type_
  | Scheme scheme -> instantiate_type_scheme ?params ~names ~types scheme
;;

let constrain' ~names ~types ~subtype ~supertype =
  constrain
    ~names
    ~types
    ~subtype:(instantiate_type_or_scheme ~names ~types subtype)
    ~supertype:(instantiate_type_or_scheme ~names ~types supertype)
;;

(* FIXME: write effect type inference:
   Example:

   ```
   val run_both : (a -> <e1> b), (a -> <e2> c), a -> <e1, e2> (b, c)
   let run_both f g x = (f x, g x)
   ```
   This becomes Lambda ([f; g; x], Tuple [Fun_call (f, [x]); Fun_call (g, [x])])
   Constraints we check for:
   Var f :> Partial_function ([Var a], [Var e1], b)
   | => f gets an upper bound of this type
   Var g :> Partial_function ([Var a], [Var e2], c)
   | => g gets an upper bound of this type
   Doing substitution:
   ...

   Another example:

   ```
   val run_twice : (a -> <e1> b), a, a -> <e1> (b, b)
   let run_twice f x y = (f x, f y)
   ```
   This becomes Lambda ([f; x; y], Tuple [Fun_call (f, [x]); Fun_call (f, [y])])
   Constraints we check for:
   Var f :> Partial_function ([Var a], [Var e1], b)
   | => f gets an upper bound of this type
   Var f :> Partial_function ([Var a], [Var e2], c)
   | => f gets an upper bound of this type
   Substituting means we should 
   ...
*)

(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types outer_type =
  let rec generalize_internal types typ ~env ~polarity
    : Module_path.absolute Type_scheme.type_
    =
    match (typ : Internal_type.t) with
    | Var var -> Var (Type_param.Env_of_vars.find_or_add env var)
    | Function (args, effects, res) ->
      let args =
        Nonempty.map
          args
          ~f:(generalize_internal types ~env ~polarity:(Polarity.flip polarity))
      in
      let effects = generalize_effects_internal types effects ~env ~polarity in
      let res = generalize_internal types res ~env ~polarity in
      Function (args, effects, res)
    | Partial_function (args, effects, result_var) ->
      let args =
        Nonempty.map
          args
          ~f:(generalize_internal types ~env ~polarity:(Polarity.flip polarity))
      in
      let effects = generalize_effects_internal types effects ~env ~polarity in
      let result_var = Type_param.Env_of_vars.find_or_add env result_var in
      Function (args, effects, Var result_var)
    | Type_app (name, fields) ->
      Type_app (name, List.map fields ~f:(generalize_internal types ~env ~polarity))
    | Tuple fields ->
      Tuple (List.map fields ~f:(generalize_internal types ~env ~polarity))
  and generalize_effects_internal types { effects; effect_var } ~env ~polarity =
    let effects =
      List.map
        (Map.to_alist effects)
        ~f:(fun (effect_name, args) : _ Type_scheme.effects ->
        Effect (effect_name, List.map args ~f:(generalize_internal types ~env ~polarity)))
    in
    let effects_from_effect_var : Module_path.absolute Type_scheme.effects option =
      match effect_var with
      | Some var -> Some (Effect_var (Type_param.Env_of_vars.find_or_add env var))
      | None -> None
    in
    Type_scheme.effect_union_list (Option.to_list effects_from_effect_var @ effects)
  in
  (* FIXME: cleanup *)
  eprint_s
    [%message "Type_bindings.generalize" (outer_type : Internal_type.t) (types : t)];
  (* FIXME: We give different names to the params in different places in the same
     statement. We should have 1 type-bindings per statement, and probably just one param
     name generator for that. *)
  let env = Type_param.Env_of_vars.create () in
  let substituted_type = Substitution.apply_to_type types.substitution outer_type in
  let scheme = generalize_internal types substituted_type ~env ~polarity:Positive in
  (* FIXME: Constraints are not propagating correctly.
     Want to replace positive instances of vars with the union of their negative lower
     bounds. (might do this for negative instances of vars too, not sure).
     Then we need to think about what to to do with the constraints. (Only include ones
     between vars which end up in the final type.) - should just work. *)
  let constraints = Constraints.to_constraint_list types.constraints ~params:env in
  let result = Type_simplification.simplify_type (scheme, constraints) in
  (* Constraints.print types.constraints; *)
  eprint_s
    [%message
      "generalize result"
        (outer_type : Internal_type.t)
        (substituted_type : Internal_type.t)
        ~pre_simplified_type:(scheme, constraints : Module_path.absolute Type_scheme.t)
        ~simplified_type:(result : Module_path.absolute Type_scheme.t)
        (env : Type_param.Env_of_vars.t)
        (types.substitution : Substitution.t)];
  result
;;

let%test_module _ =
  (module struct
    let make_vars n =
      Type_var.For_testing.reset_ids ();
      let vars = Array.init n ~f:(fun (_ : int) -> Type_var.create ()) in
      stage (fun i -> vars.(i))
    ;;

    let%expect_test "unification cycles" =
      let types = create () in
      let names = Name_bindings.core in
      let v = unstage (make_vars 4) >> Internal_type.var in
      let print_constraints () =
        [%test_pred: Substitution.t] Substitution.is_empty types.substitution;
        Constraints.print types.constraints;
        let var_partitions =
          [ 0; 1; 2; 3 ]
          |> List.map
               ~f:
                 (Constraints.find_vars_with_same_shape types.constraints
                  << Type_var.of_int)
          |> List.fold ~init:[] ~f:(fun var_partitions vars ->
               let matching_partitions =
                 List.filter_mapi var_partitions ~f:(fun i partition ->
                   let matching_var_count = Set.count vars ~f:(Set.mem partition) in
                   if matching_var_count = 0
                   then None
                   else if matching_var_count = Set.length vars
                   then Some i
                   else failwith "Mismatch")
               in
               match matching_partitions with
               | [] -> vars :: var_partitions
               | [ partition_index ] ->
                 List.mapi var_partitions ~f:(fun i partition ->
                   if i = partition_index
                   then Set.fold vars ~init:partition ~f:Set.add
                   else partition)
               | _ :: _ :: _ -> failwith "More than one matching partition")
        in
        print_s [%message (var_partitions : Type_var.Set.t list)]
      in
      print_constraints ();
      [%expect {| (var_partitions (($3) ($2) ($1) ($0))) |}];
      constrain ~names ~types ~subtype:(v 0) ~supertype:(v 1);
      print_constraints ();
      [%expect {|
        $0 <: $1
        (var_partitions (($3) ($2) ($0 $1))) |}];
      constrain ~names ~types ~subtype:(v 1) ~supertype:(v 2);
      print_constraints ();
      [%expect
        {|
        $0 <: $1, $2
        $1 <: $2
        (var_partitions (($3) ($0 $1 $2))) |}];
      constrain ~names ~types ~subtype:(v 2) ~supertype:(v 3);
      print_constraints ();
      [%expect
        {|
        $0 <: $1, $2, $3
        $1 <: $2, $3
        $2 <: $3
        (var_partitions (($0 $1 $2 $3))) |}];
      constrain ~names ~types ~subtype:(v 3) ~supertype:(v 0);
      print_constraints ();
      [%expect
        {|
        $0 <: $1, $2, $3
        $1 <: $0, $2, $3
        $2 <: $0, $1, $3
        $3 <: $0, $1, $2
        (var_partitions (($0 $1 $2 $3))) |}]
    ;;

    let%expect_test "simple [constrain] example" =
      let types = create () in
      let names = Name_bindings.core in
      let v = unstage (make_vars 3) in
      let bool_type = instantiate_type_scheme ~names ~types Intrinsics.Bool.typ in
      (* [v0 <: (v1 -> <v2> Bool)] *)
      constrain
        ~names
        ~types
        ~subtype:(Var (v 0))
        ~supertype:
          (Function ([ Var (v 1) ], Internal_type.effects_of_var (v 2), bool_type));
      print_s [%message (types.substitution : Substitution.t)];
      Constraints.print types.constraints;
      [%expect
        {|
        (types.substitution
         (($0
           (Type
            (Function ((Var $4)) ((effects ()) (effect_var ($3))) (Type_app Bool ()))))))
        $1 <: $4
        $3 <: $2 |}]
    ;;

    let foo_effect_name = Effect_name.of_string_exn "Foo"
    let bar_effect_name = Effect_name.of_string_exn "Bar"

    let%expect_test "simple [constrain_effects] example" =
      let types = create () in
      let names = Name_bindings.core in
      let v = unstage (make_vars 3) in
      (* <Foo, v0> <: <Bar v2, v1> *)
      constrain_effects
        ~names
        ~types
        ~subtype:
          { effects =
              Effect_name.Absolute.Map.singleton
                (Module_path.Absolute.empty, foo_effect_name)
                []
          ; effect_var = Some (v 0)
          }
        ~supertype:
          { effects =
              Effect_name.Absolute.Map.singleton
                (Module_path.Absolute.empty, bar_effect_name)
                [ Internal_type.Var (v 2) ]
          ; effect_var = Some (v 1)
          };
      print_s [%message (types.substitution : Substitution.t)];
      Constraints.print types.constraints;
      [%expect
        {|
        (types.substitution
         (($0 (Effects ((effects ((Bar ((Var $5))))) (effect_var ($3)))))
          ($1 (Effects ((effects ((Foo ()))) (effect_var ($4)))))))
        $3 <: $4
        $5 <: $2 |}]
    ;;

    let%expect_test "sanity check for effects subtyping semantics" =
      let types = create () in
      let names = Name_bindings.core in
      (* <Foo> <: <Foo, Bar> succeeds and produces no constraints. *)
      let foo : Internal_type.effects =
        { effects =
            Effect_name.Absolute.Map.singleton
              (Module_path.Absolute.empty, foo_effect_name)
              []
        ; effect_var = None
        }
      in
      let foo_bar : Internal_type.effects =
        { effects =
            Effect_name.Absolute.Map.of_alist_exn
              [ (Module_path.Absolute.empty, foo_effect_name), []
              ; (Module_path.Absolute.empty, bar_effect_name), []
              ]
        ; effect_var = None
        }
      in
      constrain_effects ~names ~types ~subtype:foo ~supertype:foo_bar;
      print_s [%message (types.substitution : Substitution.t)];
      Constraints.print types.constraints;
      [%expect {| (types.substitution ()) |}];
      (* <Foo,Bar> <: <Foo> fails with a type error. *)
      Compilation_error.try_with' (fun () ->
        constrain_effects ~names ~types ~subtype:foo_bar ~supertype:foo)
      |> Result.iter_error ~f:(fun error ->
           print_s [%sexp ({ error with backtrace = None } : Compilation_error.t)]);
      [%expect
        {| ((kind Type_error) (msg ("Found more effects than expected" ((Bar ()))))) |}]
    ;;

    let%expect_test "the 'same shape' equivalence relation is properly transitive" =
      let v = unstage (make_vars 3) in
      let constraints = Constraints.create () in
      List.iter
        [ 0, 2; 1, 2 ]
        ~f:(fun (a, b) -> Constraints.add constraints ~subtype:(v a) ~supertype:(v b));
      print_s
        [%sexp (Constraints.find_vars_with_same_shape constraints (v 0) : Type_var.Set.t)];
      [%expect {| ($0 $1 $2) |}]
    ;;
  end)
;;

(* TODO: Write some code to verify that the inferred types are correct which is run after
   type inference and asserts correctness. Then we could more easily catch bugs and even
   test the type inference pass with quickcheck/other fuzzing. *)
