open! Import
open! Names

let eprint_s = eprint_s [%here]

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
   GADTs (local type equality/type narrowing)
   Some of these features can make local let-generalization difficult, see:
   https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
   for an argument for just abolishing local let-generalization *)

(* TODO: Consider integrating source locations into stored types to give better type
   errors. *)

(** Prevent unstable [Type_var.t]s from appearing in test output sexps. *)
let replace_vars_in_sexp env sexp =
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

let type_error msg ~subtype ~supertype =
  let env = Type_param.Env_of_vars.create () in
  Compilation_error.raise
    Type_error
    ~msg:
      [%message
        msg
          ~subtype:(replace_vars_in_sexp env [%sexp (subtype : Internal_type.t)] : Sexp.t)
          ~supertype:
            (replace_vars_in_sexp env [%sexp (supertype : Internal_type.t)] : Sexp.t)]
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
  val remove_vars : t -> Type_var.t -> Type_var.Set.t -> unit
  val find_vars_with_same_shape : t -> Type_var.t -> Type_var.Set.t
  val mark_effects_as_not_included : t -> Type_var.t -> Effect_name.Absolute.Set.t -> unit
  val find_effects_not_included : t -> Type_var.t -> Effect_name.Absolute.Set.t

  val get_relevant_constraints
    :  t
    -> params:Type_param.Env_of_vars.t
    -> vars_by_polarity:_ Type_param.Map.t By_polarity.t
    -> Type_scheme.constraint_ list

  val print : t -> unit
end = struct
  type t =
    { lower_bounds : Type_var.Hash_set.t Type_var.Table.t
    ; upper_bounds : Type_var.Hash_set.t Type_var.Table.t
    ; effects_not_included : Effect_name.Absolute.Set.t Type_var.Table.t
    }
  [@@deriving sexp_of]

  let create () =
    { lower_bounds = Type_var.Table.create ()
    ; upper_bounds = Type_var.Table.create ()
    ; effects_not_included = Type_var.Table.create ()
    }
  ;;

  let get_bounds bounds var =
    Hashtbl.find_or_add bounds var ~default:(fun () ->
      let vars = Type_var.Hash_set.create () in
      Hash_set.add vars var;
      vars)
  ;;

  let add_bounds { lower_bounds; upper_bounds; _ } ~subtype ~supertype =
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

  let remove_vars { lower_bounds; upper_bounds; _ } var vars =
    Hash_set.filter_inplace (get_bounds lower_bounds var) ~f:(not << Set.mem vars);
    Hash_set.filter_inplace (get_bounds upper_bounds var) ~f:(not << Set.mem vars)
  ;;

  let find_vars_with_same_shape { lower_bounds; upper_bounds; _ } var =
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

  let mark_effects_as_not_included t var effects =
    Hashtbl.update t.effects_not_included var ~f:(function
      | None -> effects
      | Some existing_effects -> Set.union existing_effects effects)
  ;;

  let find_effects_not_included t var =
    Hash_set.fold
      (get_bounds t.lower_bounds var)
      ~init:Effect_name.Absolute.Set.empty
      ~f:(fun effects var ->
      match Hashtbl.find t.effects_not_included var with
      | None -> effects
      | Some effects' -> Set.union effects effects')
  ;;

  let get_relevant_constraints t ~params ~(vars_by_polarity : _ By_polarity.t) =
    Hashtbl.to_alist t.upper_bounds
    |> List.concat_map ~f:(fun (subtype, supertypes) ->
         match Type_param.Env_of_vars.find params subtype with
         | None -> []
         | Some subtype ->
           if not (Map.mem vars_by_polarity.negative subtype)
           then []
           else
             Hash_set.to_list supertypes
             |> List.filter_map ~f:(fun supertype : Type_scheme.constraint_ option ->
                  match Type_param.Env_of_vars.find params supertype with
                  | None -> None
                  | Some supertype ->
                    if Type_param_name.equal subtype supertype
                       || not (Map.mem vars_by_polarity.positive supertype)
                    then None
                    else Some { subtype; supertype }))
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

  val iter_reachable_vars
    :  t
    -> Type_var.t
    -> polarity:Polarity.t
    -> f:(Type_var.t -> polarity:Polarity.t -> unit)
    -> unit

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
      | (Never | Any) as typ -> Halt typ
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
         | (Function _ | Type_app _ | Tuple _ | Never | Any) as result_type ->
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

  let rec iter_reachable_vars t var ~polarity ~f =
    Option.iter (Hashtbl.find t var) ~f:(function
      | Type type_ ->
        Internal_type.iter_vars type_ ~polarity ~f:(fun var ~polarity ->
          f var ~polarity;
          iter_reachable_vars t var ~polarity ~f)
      | Effects effects ->
        Internal_type.iter_effects_vars effects ~polarity ~f:(fun var ~polarity ->
          f var ~polarity;
          iter_reachable_vars t var ~polarity ~f))
  ;;

  let is_empty = Hashtbl.is_empty
end

type t =
  { constraints : Constraints.t (** The constraints on type variables. *)
  ; substitution : Substitution.t
      (** A mapping from type variables to type or effect expressions that can be
          substituted in their place. *)
  ; context_vars : Type_var.Hash_set.t By_polarity.t
      (** The polarities of type variables which are associated with regular variables
          (names) from the context. These are created when instantiating type schemes or
          when adding new names from a pattern. *)
  ; generalized_vars : Type_param.Env_of_vars.t
      (** A mapping from type variables to their names as generalized type parameters.
          Used to ensure that the same type variable in different places generalizes to
          the same type parameter name. *)
  ; constrained_types : Type_pair.Hash_set.t [@sexp_drop_if const true]
  ; constrained_effects : Effect_type_pair.Hash_set.t [@sexp_drop_if const true]
  }
[@@deriving sexp_of]

let create () =
  { constraints = Constraints.create ()
  ; substitution = Substitution.create ()
  ; context_vars = By_polarity.init (fun (_ : Polarity.t) -> Type_var.Hash_set.create ())
  ; generalized_vars = Type_param.Env_of_vars.create ()
  ; constrained_types = Type_pair.Hash_set.create ()
  ; constrained_effects = Effect_type_pair.Hash_set.create ()
  }
;;

let rec occurs_in id : Internal_type.t -> bool = function
  | Var id' -> Type_var.(id = id')
  | Never | Any -> false
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
  | Unequal_lengths -> type_error "Type item length mismatch" ~subtype ~supertype
;;

let record_context_var types var ~polarity =
  Hash_set.add (By_polarity.get types.context_vars ~polarity) var
;;

let refresh_type type_ ~subtyping_dir =
  let vars = Type_var.Table.create () in
  let refresh_var = Hashtbl.find_or_add vars ~default:Type_var.create in
  Internal_type.map_vars type_ ~f:refresh_var
  |> Internal_type.map ~f:(fun type_ ->
       (* It's ok to special-case Any and Never here because we use this after expanding
          all type aliases. The special-casing is to represent that e.g. $1 <: List Any
          shouldn't generate a substitution like $1 := List Any. It should use a type
          variable like $1 := List $2. *)
       match type_, subtyping_dir with
       | Any, `Supertype | Never, `Subtype -> Halt (Internal_type.fresh_var ())
       | _, (`Subtype | `Supertype) -> Defer type_)
;;

let rec constrain ~names ~types ~subtype ~supertype =
  let lookup_type names name args =
    let type_entry = Name_bindings.find_absolute_type_entry names name in
    if List.length args = Type_decl.arity (Name_bindings.Type_entry.decl type_entry)
    then type_entry
    else type_error "Partially applied type constructor" ~subtype ~supertype
  in
  let rec expand_aliases type_ =
    Internal_type.map type_ ~f:(fun type_ ->
      match type_ with
      | Type_app (name, args) ->
        let type_entry = lookup_type names name args in
        (match Name_bindings.Type_entry.decl type_entry with
         | params, Alias alias ->
           let args = List.map args ~f:expand_aliases in
           let params_to_vars = Type_param.Env_to_vars.create () in
           let instantiated_alias =
             instantiate_type_scheme ~names ~types ~params:params_to_vars (alias, [])
           in
           let arg_substitution = Substitution.create () in
           List.zip_exn (params :> Type_param_name.t list) args
           |> List.iter ~f:(fun (param, arg) ->
                let var = Type_param.Env_to_vars.find_or_add params_to_vars param in
                Substitution.set_type arg_substitution var arg);
           Retry (Substitution.apply_to_type arg_substitution instantiated_alias)
         | _ -> Defer type_)
      | _ -> Defer type_)
  in
  eprint_s
    [%lazy_message
      "Type_bindings.constrain_internal (pre-substitution)"
        (subtype : Internal_type.t)
        (supertype : Internal_type.t)
        (types.constraints : Constraints.t)
        (types.substitution : Substitution.t)
        (types.context_vars : Type_var.Hash_set.t By_polarity.t)
        (types.generalized_vars : Type_param.Env_of_vars.t)];
  (* It's important to apply the substitution before checking [constrained_types] since
     the substitution can change as we do more [constrain] calls. *)
  let subtype = Substitution.apply_to_type types.substitution subtype in
  let supertype = Substitution.apply_to_type types.substitution supertype in
  if not (Hash_set.mem types.constrained_types (subtype, supertype))
  then (
    eprint_s
      [%lazy_message
        "Type_bindings.constrain (post-substitution)"
          (subtype : Internal_type.t)
          (supertype : Internal_type.t)];
    Hash_set.add types.constrained_types (subtype, supertype);
    let subtype = expand_aliases subtype in
    let supertype = expand_aliases supertype in
    eprint_s
      [%lazy_message
        "Type_bindings.constrain (after alias expansion)"
          (subtype : Internal_type.t)
          (supertype : Internal_type.t)];
    match subtype, supertype with
    | _, Any | Never, _ -> ()
    | Any, Never
    | Any, (Type_app _ | Tuple _ | Function _ | Partial_function _)
    | (Type_app _ | Tuple _ | Function _ | Partial_function _), Never ->
      type_error "Type mismatch" ~subtype ~supertype
    | Var var1, Var var2 ->
      if not (Type_var.equal var1 var2)
      then Constraints.add types.constraints ~subtype:var1 ~supertype:var2
    | Var var, type_ ->
      (* FIXME: Need a more robust occurs check *)
      if occurs_in var type_ then type_error "Occurs check failed" ~subtype ~supertype;
      (match type_ with
       | Any -> ()
       | _ -> check_var_vs_type ~names ~types ~var ~type_ ~var_side:`Left)
    | type_, Var var ->
      (* FIXME: Need a more robust occurs check *)
      if occurs_in var type_ then type_error "Occurs check failed" ~subtype ~supertype;
      (match type_ with
       | Never -> ()
       | _ -> check_var_vs_type ~names ~types ~var ~type_ ~var_side:`Right)
    | Type_app (name1, args1), Type_app (name2, args2) ->
      let type_entry1 = lookup_type names name1 args1 in
      let type_entry2 = lookup_type names name2 args2 in
      if not (Name_bindings.Type_entry.identical type_entry1 type_entry2)
      then type_error "Type application mismatch" ~subtype ~supertype;
      (* TODO: This effectively assumes the arguments are covariant, which isn't sound.
         Implement inference and manual specification of type parameter variance, similar
         to what OCaml does. *)
      iter2_types args1 args2 ~subtype ~supertype ~f:(fun arg1 arg2 ->
        constrain ~names ~types ~subtype:arg1 ~supertype:arg2)
    | Type_app (name, args), (Tuple _ | Function _ | Partial_function _) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | _params, Alias expr ->
         constrain
           ~names
           ~types
           ~subtype:(instantiate_type_scheme ~names ~types (expr, []))
           ~supertype
       | _ -> type_error "Type application mismatch" ~subtype ~supertype)
    | (Tuple _ | Function _ | Partial_function _), Type_app (name, args) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | _params, Alias expr ->
         constrain
           ~names
           ~types
           ~subtype
           ~supertype:(instantiate_type_scheme ~names ~types (expr, []))
       | _ -> type_error "Type application mismatch" ~subtype ~supertype)
    | Function (args1, effects1, res1), Function (args2, effects2, res2) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Same_length -> ()
       | Left_trailing _ | Right_trailing _ -> fun_arg_number_mismatch ~subtype ~supertype);
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
       | Left_trailing _ -> fun_arg_number_mismatch ~subtype ~supertype
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
       | Right_trailing _ -> fun_arg_number_mismatch ~subtype ~supertype
       | Same_length ->
         constrain_effects ~names ~types ~subtype:effects1 ~supertype:effects2;
         constrain ~names ~types ~subtype:res ~supertype:(Var var))
    | Tuple xs, Tuple ys ->
      iter2_types xs ys ~subtype ~supertype ~f:(fun arg1 arg2 ->
        constrain ~names ~types ~subtype:arg1 ~supertype:arg2)
    | Tuple _, (Function _ | Partial_function _)
    | Function _, Tuple _
    | Partial_function _, Tuple _ -> type_error "Types do not match" ~subtype ~supertype)

and check_var_vs_type ~names ~types ~var ~type_ ~var_side =
  let vars_with_same_shape =
    Constraints.find_vars_with_same_shape types.constraints var
  in
  Set.iter vars_with_same_shape ~f:(fun var ->
    Constraints.remove_vars types.constraints var vars_with_same_shape);
  let new_var_substitution =
    let substitution = Substitution.create () in
    Set.iter vars_with_same_shape ~f:(fun var ->
      let subtyping_dir =
        match var_side with
        | `Left -> `Supertype
        | `Right -> `Subtype
      in
      Substitution.set_type substitution var (refresh_type type_ ~subtyping_dir));
    substitution
  in
  eprint_s
    [%lazy_message
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
    match var_side with
    | `Left -> constrain ~names ~types ~subtype:(Var var) ~supertype:type_
    | `Right -> constrain ~names ~types ~subtype:type_ ~supertype:(Var var));
  Set.iter vars_with_same_shape ~f:(fun v ->
    Set.iter vars_with_same_shape ~f:(fun v' ->
      constrain ~names ~types ~subtype:(Var v) ~supertype:(Var v')))

and constrain_effects ~names ~types ~subtype ~supertype =
  eprint_s
    [%lazy_message
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
      [%lazy_message
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
    Option.iter subtype_var ~f:(fun subtype_var ->
      Constraints.mark_effects_as_not_included
        types.constraints
        subtype_var
        (Map.key_set subtype_effects));
    Option.iter supertype_var ~f:(fun supertype_var ->
      Constraints.mark_effects_as_not_included
        types.constraints
        supertype_var
        (Map.key_set supertype_effects));
    (* FIXME: Should it be a type error if we have e not including Foo, then try to
       constrain a :> Foo? Or is that a compiler bug? *)
    let supertype_effects' =
      match subtype_var with
      | None -> supertype_effects
      | Some subtype_var ->
        let effects_not_included =
          Constraints.find_effects_not_included types.constraints subtype_var
        in
        Map.filter_keys supertype_effects ~f:(fun supertype_effect ->
          (not (Set.mem effects_not_included supertype_effect))
          || Map.mem subtype_effects supertype_effect)
    in
    if not ([%equal: _ Effect_name.Absolute.Map.t] supertype_effects supertype_effects')
    then
      eprint_s
        [%lazy_message
          "Removed some effects"
            (supertype_effects : _ Effect_name.Absolute.Map.t)
            (supertype_effects' : _ Effect_name.Absolute.Map.t)];
    let supertype_effects = supertype_effects' in
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
               (* TODO: Handle type parameter variance for effects. This conservatively
                  assumes invariance. *)
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
    let create_substitution ~effects ~var ~effects_subtyping_dir ~all_mentioned_effects =
      let substitution = Substitution.create () in
      let new_var =
        if Map.is_empty effects
        then None
        else
          Option.map var ~f:(fun var ->
            let new_var = Type_var.create () in
            let effects =
              Map.map effects ~f:(fun args ->
                List.map args ~f:(fun arg ->
                  (* FIXME: Is the subtyping direction right here? Maybe it's backwards? *)
                  let arg' = refresh_type arg ~subtyping_dir:effects_subtyping_dir in
                  (match effects_subtyping_dir with
                   | `Subtype -> constrain ~names ~types ~subtype:arg ~supertype:arg'
                   | `Supertype -> constrain ~names ~types ~subtype:arg' ~supertype:arg);
                  arg'))
            in
            Substitution.set_effects
              substitution
              var
              { effects; effect_var = Some new_var };
            Constraints.mark_effects_as_not_included
              types.constraints
              new_var
              all_mentioned_effects;
            new_var)
      in
      substitution, new_var
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
      let all_mentioned_effects =
        Set.union (Map.key_set subtype_effects) (Map.key_set supertype_effects)
      in
      let supertype_only_substitution, new_subtype_var =
        create_substitution
          ~effects:supertype_only
          ~var:subtype_var
          ~effects_subtyping_dir:`Supertype
          ~all_mentioned_effects
      in
      let subtype_only_substitution, new_supertype_var =
        create_substitution
          ~effects:subtype_only
          ~var:supertype_var
          ~effects_subtyping_dir:`Subtype
          ~all_mentioned_effects
      in
      eprint_s
        [%lazy_message
          "constraining effects"
            (subtype_var : Type_var.t option)
            (new_subtype_var : Type_var.t option)
            (supertype_only_substitution : Substitution.t)
            (supertype_var : Type_var.t option)
            (new_supertype_var : Type_var.t option)
            (subtype_only_substitution : Substitution.t)];
      (* Remove all constraints between vars of the same shape as either var here, then
         re-add them with the new substitution applied. *)
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
      (* Apply the new substitution and re-add the constraints. *)
      Substitution.compose types.substitution subtype_only_substitution;
      Substitution.compose types.substitution supertype_only_substitution;
      let to_var = Internal_type.effects_of_var in
      Set.iter vars_with_same_shape_as_subtype_var ~f:(fun v ->
        Set.iter vars_with_same_shape_as_subtype_var ~f:(fun v' ->
          constrain_effects ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')));
      Set.iter vars_with_same_shape_as_supertype_var ~f:(fun v ->
        Set.iter vars_with_same_shape_as_supertype_var ~f:(fun v' ->
          constrain_effects ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')));
      (* Add constraints between the residual effect variables. *)
      constrain_effect_vars
        types
        ~subtype_var:(Option.first_some new_subtype_var subtype_var)
        ~supertype_var:(Option.first_some new_supertype_var supertype_var)))

and constrain_effects_to_be_total ~names ~types effects =
  constrain_effects ~names ~types ~subtype:effects ~supertype:Internal_type.no_effects

(* FIXME: Every time a new variable is minted, we need to record its polarity *)

and instantiate_type_scheme =
  (* FIXME: Actually, I think it should be fine to restrict the instantiated variables of
     other functions we use. *)
  (* let record_context_var _ _ ~polarity:_ = () in *)
  let rec instantiate_type_scheme
    ~names
    ~types
    ~params
    ~polarity
    (scheme : Module_path.absolute Type_scheme.type_)
    : Internal_type.t
    =
    match scheme with
    | Var param ->
      let var = Type_param.Env_to_vars.find_or_add params param in
      record_context_var types var ~polarity;
      Var var
    | Type_app (type_name, args) ->
      (* FIXME: Handle contravariant type parameters in type application *)
      Type_app
        ( type_name
        , List.map args ~f:(instantiate_type_scheme ~names ~types ~params ~polarity) )
    | Tuple fields ->
      Tuple (List.map fields ~f:(instantiate_type_scheme ~names ~types ~params ~polarity))
    | Function (args, effects, result) ->
      let args =
        let polarity = Polarity.flip polarity in
        Nonempty.map args ~f:(instantiate_type_scheme ~names ~types ~params ~polarity)
      in
      let effects =
        instantiate_effect_type_scheme ~names ~types ~params ~polarity effects
      in
      Function
        (args, effects, instantiate_type_scheme ~names ~types ~params result ~polarity)
    | Union [] -> Never
    | Intersection [] -> Any
    | Union args ->
      let var = Type_var.create () in
      record_context_var types var ~polarity;
      let var : Internal_type.t = Var var in
      let args =
        Non_single_list.map
          args
          ~f:(instantiate_type_scheme ~names ~types ~params ~polarity)
      in
      Non_single_list.iter args ~f:(fun subtype ->
        constrain ~names ~types ~subtype ~supertype:var);
      var
    | Intersection args ->
      let var = Type_var.create () in
      record_context_var types var ~polarity;
      let var : Internal_type.t = Var var in
      let args =
        Non_single_list.map
          args
          ~f:(instantiate_type_scheme ~names ~types ~params ~polarity)
      in
      Non_single_list.iter args ~f:(fun supertype ->
        constrain ~names ~types ~subtype:var ~supertype);
      var
  and instantiate_effect_type_scheme ~names ~types ~params ~polarity effects =
    let collect_effects (effects : _ Type_scheme.effects Non_single_list.t)
      : Internal_type.effects
      =
      let concrete_effects, effect_vars =
        Non_single_list.fold
          effects
          ~init:(Effect_name.Absolute.Map.empty, [])
          ~f:(fun (concrete_effects, effect_vars) new_effects ->
          match new_effects with
          | Effect (effect_name, args) ->
            let args =
              List.map args ~f:(instantiate_type_scheme ~names ~types ~params ~polarity)
            in
            (match Map.add concrete_effects ~key:effect_name ~data:args with
             | `Ok concrete_effects -> concrete_effects, effect_vars
             | `Duplicate ->
               Compilation_error.raise
                 Type_error
                 ~msg:
                   [%message "Duplicate effects" (effect_name : Effect_name.Absolute.t)])
          | Effect_var param ->
            let var = Type_param.Env_to_vars.find_or_add params param in
            record_context_var types var ~polarity;
            concrete_effects, var :: effect_vars
          | Effect_union [] ->
            (* FIXME: how are these empty unions/intersections even getting around? I
               think we might need them as vars for correctness. Just converting them to
               vars solve the problem for now, but isn't very principled. *)
            let var = Type_var.create () in
            record_context_var types var ~polarity;
            concrete_effects, var :: effect_vars
          | Effect_union (_ :: _) ->
            (* TODO: Decide if this should be allowed, or statically prevent it from
               happening by changing the type. I think the syntax doesn't currently allow
               it, and we won't generate it. *)
            compiler_bug
              [%message "Nested complex effects" (new_effects : _ Type_scheme.effects)])
      in
      let effect_var =
        if List.is_empty effect_vars
        then None
        else (
          let effect_var = Type_var.create () in
          record_context_var types effect_var ~polarity;
          List.iter effect_vars ~f:(fun other_var ->
            Constraints.add types.constraints ~subtype:other_var ~supertype:effect_var);
          Some effect_var)
      in
      { effects = concrete_effects; effect_var }
    in
    match effects with
    | Effect (effect_name, args) ->
      { effects =
          Effect_name.Absolute.Map.singleton
            effect_name
            (List.map args ~f:(instantiate_type_scheme ~names ~types ~params ~polarity))
      ; effect_var = None
      }
    | Effect_var param ->
      let var = Type_param.Env_to_vars.find_or_add params param in
      record_context_var types var ~polarity;
      { effects = Effect_name.Absolute.Map.empty; effect_var = Some var }
    | Effect_union args -> collect_effects args
  in
  fun ?(params = Type_param.Env_to_vars.create ())
      ~names
      ~types
      ((scheme, constraints) : Module_path.absolute Type_scheme.t) ->
    List.iter constraints ~f:(fun { subtype; supertype } ->
      constrain
        ~names
        ~types
        ~subtype:(Var (Type_param.Env_to_vars.find_or_add params subtype))
        ~supertype:(Var (Type_param.Env_to_vars.find_or_add params supertype)));
    instantiate_type_scheme ~names ~types scheme ~params ~polarity:Positive
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

let record_context_vars types pat_names =
  eprint_s
    [%lazy_message "Type_bindings.record_context_vars" (pat_names : Pattern_names.t)];
  Map.iteri pat_names ~f:(fun ~key:name ~data:name_entry ->
    match Name_bindings.Name_entry.type_ name_entry with
    | Type type_ ->
      (* TODO: Singling out `resume` is a bit of a hack. Is there a better, more general
         way of handling this? I suspect `resume` really is kinda special though, since
         it's inherently recursive. It should work similarly to recursive bound variables. *)
      (* The `resume` keyword works differently to other pattern-bound names. The type
         variables in its type are guaranteed to come from within the same expression,
         not as an input. So, it's safe to not consider its type to include context
         variables. *)
      if not (Value_name.equal name Value_name.resume_keyword)
      then Internal_type.iter_vars type_ ~polarity:Positive ~f:(record_context_var types)
    | Scheme scheme ->
      compiler_bug
        [%message "Unexpected pattern name entry type" (scheme : _ Type_scheme.t)])
;;

let constrain' ~names ~types ~subtype ~supertype =
  constrain
    ~names
    ~types
    ~subtype:(instantiate_type_or_scheme ~names ~types subtype)
    ~supertype:(instantiate_type_or_scheme ~names ~types supertype)
;;

(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types outer_type =
  let rec generalize_internal types typ ~polarity : Module_path.absolute Type_scheme.type_
    =
    match (typ : Internal_type.t) with
    | Never -> Union []
    | Any -> Intersection []
    | Var var -> Var (Type_param.Env_of_vars.find_or_add types.generalized_vars var)
    | Function (args, effects, res) ->
      let args =
        Nonempty.map
          args
          ~f:(generalize_internal types ~polarity:(Polarity.flip polarity))
      in
      let effects = generalize_effects_internal types effects ~polarity in
      let res = generalize_internal types res ~polarity in
      Function (args, effects, res)
    | Partial_function (args, effects, result_var) ->
      let args =
        Nonempty.map
          args
          ~f:(generalize_internal types ~polarity:(Polarity.flip polarity))
      in
      let effects = generalize_effects_internal types effects ~polarity in
      let result_var =
        Type_param.Env_of_vars.find_or_add types.generalized_vars result_var
      in
      Function (args, effects, Var result_var)
    | Type_app (name, fields) ->
      Type_app (name, List.map fields ~f:(generalize_internal types ~polarity))
    | Tuple fields -> Tuple (List.map fields ~f:(generalize_internal types ~polarity))
  and generalize_effects_internal types { effects; effect_var } ~polarity =
    let effects =
      List.map
        (Map.to_alist effects)
        ~f:(fun (effect_name, args) : _ Type_scheme.effects ->
        Effect (effect_name, List.map args ~f:(generalize_internal types ~polarity)))
    in
    let effects_from_effect_var : Module_path.absolute Type_scheme.effects option =
      match effect_var with
      | Some var ->
        Some (Effect_var (Type_param.Env_of_vars.find_or_add types.generalized_vars var))
      | None -> None
    in
    Type_scheme.effect_union_list (Option.to_list effects_from_effect_var @ effects)
  in
  eprint_s
    [%lazy_message "Type_bindings.generalize" (outer_type : Internal_type.t) (types : t)];
  (* FIXME: We give different names to the params in different places in the same
     statement. We should have 1 type-bindings per statement, and probably just one param
     name generator for that. *)
  let substituted_type = Substitution.apply_to_type types.substitution outer_type in
  let scheme = generalize_internal types substituted_type ~polarity:Positive in
  (* FIXME: We could filter the constraints just used to simplify this expression at this
     point - that should reduce a lot of problems, right? We don't need to worry about
     removing constraints if we just do it temporarily. Might work ok .*)
  let context_vars =
    (* FIXME: I think we also need to extend this with the substitution - anything we'd
       substitute in place of context vars should count as "from the context". e.g. if
       we have a function as an argument, it starts out with just one var as the known
       type. We'll later get some constraints on that. Hopefully just looking at the
       substitution is good enough?
       
       How can we efficiently keep the context vars updated? They can change whenever we
       change the substitution? Maybe keep them as part of the substitution? Or just
       apply the substitution here every time?
       
       Actually I'm not 100% sure we need this, hmmm.
       This seems expensive, can we amortize the cost by doing it each time we edit the
       substitution or add a context var? (instead of each time we generalize anything,
       which should be more often)
       
       I think this is most likely needed, it makes sense.

       One problem: when typing a recursive definition, the context vars include the var
       itself, which is weird. Maybe those should be exempted? We could add them as
       context vars after typing the body. They aren't really "inputs" in the same
       sense, I think? Yeah, they never appeared in negative position, or something
       equivalent. (It's kinda similar to instantiated vars though?) *)
    eprint_s
      [%lazy_message
        "context vars before checking substitutions"
          (types.context_vars : Type_var.Hash_set.t By_polarity.t)];
    By_polarity.iter types.context_vars ~f:(fun context_vars ~polarity ->
      Array.iter (Hash_set.to_array context_vars) ~f:(fun var ->
        Substitution.iter_reachable_vars
          types.substitution
          var
          ~polarity
          ~f:(fun var ~polarity ->
          eprint_s [%lazy_message "iter var" (var : Type_var.t) (polarity : Polarity.t)];
          Hash_set.add (By_polarity.get types.context_vars ~polarity) var)));
    By_polarity.map types.context_vars ~f:(fun vars ->
      Hash_set.fold vars ~init:Type_param.Map.empty ~f:(fun acc var ->
        let var = Type_param.Env_of_vars.find_or_add types.generalized_vars var in
        Map.set acc ~key:var ~data:1))
  in
  eprint_s
    [%lazy_message
      "context vars after checking substitutions"
        (types.context_vars : Type_var.Hash_set.t By_polarity.t)
        (context_vars : int Type_param.Map.t By_polarity.t)];
  let constraints =
    let vars_by_polarity =
      Type_simplification.get_positive_and_negative_vars scheme ~context_vars
    in
    eprint_s
      [%lazy_message
        "Getting relevant constraints"
          (vars_by_polarity : int Type_param.Map.t By_polarity.t)];
    Constraints.get_relevant_constraints
      types.constraints
      ~params:types.generalized_vars
      ~vars_by_polarity
  in
  let pre_simplified_type = scheme, constraints in
  let simplified_type =
    Type_simplification.simplify_type (scheme, constraints) ~context_vars
  in
  eprint_s
    [%lazy_message
      "generalize result"
        (outer_type : Internal_type.t)
        (substituted_type : Internal_type.t)
        (pre_simplified_type : Module_path.absolute Type_scheme.t)
        (simplified_type : Module_path.absolute Type_scheme.t)
        (types.generalized_vars : Type_param.Env_of_vars.t)
        (types.substitution : Substitution.t)];
  simplified_type
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
         (($0 (Effects ((effects ((Bar ((Var $4))))) (effect_var ($3)))))
          ($1 (Effects ((effects ((Foo ()))) (effect_var ($5)))))))
        $3 <: $5
        $4 <: $2 |}]
    ;;

    let%expect_test "[constrain_effects]: var <: effect, var" =
      let types = create () in
      let names = Name_bindings.core in
      let v = unstage (make_vars 2) in
      (* <v0> <: <Foo, v1> *)
      constrain_effects
        ~names
        ~types
        ~subtype:{ effects = Effect_name.Absolute.Map.empty; effect_var = Some (v 0) }
        ~supertype:
          { effects =
              Effect_name.Absolute.Map.singleton
                (Module_path.Absolute.empty, foo_effect_name)
                []
          ; effect_var = Some (v 1)
          };
      print_s [%message (types.substitution : Substitution.t)];
      Constraints.print types.constraints;
      [%expect
        {|
        (types.substitution
         (($0 (Effects ((effects ((Foo ()))) (effect_var ($2)))))))
        $2 <: $1 |}]
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
