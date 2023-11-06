open! Import
open! Names

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
   GADTs (local type equality/type narrowing)
   Some of these features can make local let-generalization difficult, see:
   https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
   for an argument for just abolishing local let-generalization *)

(* TODO: Consider integrating source locations into stored types to give better type
   errors. *)

let type_error msg t1 t2 =
  (* Prevent unstable Var_ids from appearing in test output *)
  let env = Type_param.Env_of_vars.create () in
  (* FIXME: Need a way to show an internnal type with stable names for the variables.
     Hmm, maybe it would be good enough if each type expression had its own variable scope,
     instead of it being global. Maybe generalize what Mir_name.Name_table does? Or make a
     wrapper for Unique_id.Int ()? Or an easier way: map over the sexp. *)
  (* TODO: This is a hack to remove unstable type variables from the output in tests. 
     We should probably have type variables be scopied properly to let binding groups for
     one [Type_bindings.t]. We also need to think of a smarter way to display an
     [Internal_type.t] in a user-friendly format. (A sexp is definitely not it.) *)
  let map_type t =
    let rec loop : Sexp.t -> Sexp.t = function
      | List sexps -> List (List.map sexps ~f:loop)
      | Atom str as atom ->
        (match Type_var.of_string str with
         | var -> [%sexp (Type_param.Env_of_vars.find_or_add env var : Type_param_name.t)]
         | exception _ -> atom)
    in
    loop [%sexp (t : Internal_type.t)]
  in
  Compilation_error.raise
    Type_error
    ~msg:[%message msg ~type1:(map_type t1 : Sexp.t) ~type2:(map_type t2 : Sexp.t)]
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
end = struct
  type t =
    { lower_bounds : Type_var.t Queue.t Type_var.Table.t
    ; upper_bounds : Type_var.t Queue.t Type_var.Table.t
    }
  [@@deriving sexp_of]

  let create () =
    { lower_bounds = Type_var.Table.create (); upper_bounds = Type_var.Table.create () }
  ;;

  let get_bounds = Hashtbl.find_or_add ~default:Queue.create

  let add_bounds { lower_bounds; upper_bounds } ~subtype ~supertype =
    Queue.enqueue (get_bounds upper_bounds subtype) supertype;
    Queue.enqueue (get_bounds lower_bounds supertype) subtype
  ;;

  (* If we get a constraint [a <= b], we also need to add all constraints transitively
     implied by that, so that the constraints remain closed under logical implication.
     So we need to add all constraints of the form a' <= b' for all a' <= a and b' >= b. *)
  let add t ~subtype ~supertype =
    Queue.iter (get_bounds t.lower_bounds subtype) ~f:(fun subtype' ->
      Queue.iter (get_bounds t.upper_bounds supertype) ~f:(fun supertype' ->
        add_bounds t ~subtype:subtype' ~supertype:supertype'));
    add_bounds t ~subtype ~supertype
  ;;

  let remove_vars { lower_bounds; upper_bounds } var vars =
    Queue.filter_inplace (get_bounds lower_bounds var) ~f:(not << Set.mem vars);
    Queue.filter_inplace (get_bounds upper_bounds var) ~f:(not << Set.mem vars)
  ;;

  let find_vars_with_same_shape { lower_bounds; upper_bounds } var =
    let add_vars bounds vars = Queue.fold ~init:vars (get_bounds bounds var) ~f:Set.add in
    Type_var.Set.empty |> add_vars lower_bounds |> add_vars upper_bounds
  ;;
end

module Substitution : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val set_type : t -> Type_var.t -> Internal_type.t -> unit
  val set_effects : t -> Type_var.t -> Internal_type.effects -> unit
  val compose : t -> t -> unit
  val apply_to_type : t -> Internal_type.t -> Internal_type.t
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

  let apply_to_type_var t var =
    match Hashtbl.find t var with
    | Some (Type type_) -> type_
    | None -> Var var
    | Some (Effects _) -> compiler_bug [%message "Expected type, got effects"]
  ;;

  let rec apply_to_type t type_ =
    Internal_type.map type_ ~f:(function
      | Var var ->
        (* FIXME: Should this be Retry? No, I don't think we want to recursively apply
           the substitution, do we? *)
        Halt (apply_to_type_var t var)
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
           (* FIXME: Effects must be pure for us to combine partial functions.
              PROBLEM: We might not know at this stage whether effects are pure. For now,
              let's do something naive. *)
           if Internal_type.equal_effects effects Internal_type.no_effects
           then Halt (Partial_function (Nonempty.append args args', effects', result_var))
           else Halt (Function (args, effects, result_type))
         | (Function _ | Type_app _ | Tuple _) as result_type ->
           Halt (Function (args, effects, result_type)))
      | (Type_app _ | Tuple _) as type_ -> Defer type_)

  and apply_to_effects t ({ effects; effect_var } : Internal_type.effects)
    : Internal_type.effects
    =
    let new_effects =
      let%bind.Option effect_var = effect_var in
      match%map.Option Hashtbl.find t effect_var with
      | Effects effects -> effects
      | Type _ -> compiler_bug [%message "Expected effects, got type"]
    in
    (* FIXME: apply substitutions to effect vars. We should be able to do that, right? *)
    { effects = Map.map effects ~f:(List.map ~f:(apply_to_type t))
    ; effect_var = (if Option.is_some new_effects then None else effect_var)
    }
  ;;

  let compose t t' =
    Hashtbl.map_inplace t ~f:(function
      | Type type_ -> Type (apply_to_type t' type_)
      | Effects effects -> Effects (apply_to_effects t' effects))
  ;;

  let is_empty = Hashtbl.is_empty
end

type t =
  { constraints : Constraints.t
  ; substitution : Substitution.t
  ; constrained_types : Type_pair.Hash_set.t
  ; constrained_effects : Effect_type_pair.Hash_set.t
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

(* FIXME: cleanup *)
(* let unhandled_effects effects =
  Compilation_error.raise
    Type_error
    ~msg:
      [%message
        "Unhandled effects"
          (effects
            : (Type_var.t, Type_var.t, Module_path.absolute) Internal_type.effects)]
;; *)

let iter2_types xs ys ~subtype ~supertype ~f =
  match List.iter2 ~f xs ys with
  | Ok () -> ()
  | Unequal_lengths -> type_error "Type item length mismatch" subtype supertype
;;

(* FIXME: cleanup *)
(* let get_var_bounds =
  Hashtbl.find_or_add ~default:(fun () ->
    { lower_bounds = Queue.create (); upper_bounds = Queue.create () })
;; *)

let refresh_type type_ =
  let vars = Type_var.Table.create () in
  let refresh_var = Hashtbl.find_or_add vars ~default:Type_var.create in
  Internal_type.map_vars type_ ~f:refresh_var
;;

let refresh_effects ({ effects; effect_var } : Internal_type.effects)
  : Internal_type.effects
  =
  let vars = Type_var.Table.create () in
  let refresh_var = Hashtbl.find_or_add vars ~default:Type_var.create in
  { effects = Map.map effects ~f:(List.map ~f:(Internal_type.map_vars ~f:refresh_var))
  ; effect_var = Option.map effect_var ~f:refresh_var
  }
;;

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
  (* FIXME: simplify `orient` - How? *)
  let orient f ~var ~type_ =
    match var_side with
    | `Left -> f (to_var var) type_
    | `Right -> f type_ (to_var var)
  in
  (* FIXME: Clean up pseudocode:
     Let S be the current substitution, C be the current constraints.
     
     - Make a new substitution S' with all vars of the same shape as id1 in C each
       mapping to different refreshed versions of type2
     - Make a new set of constraints C' with subtyping relations between all pairs of
       vars of the same shape as id1 in C.
     - Recurse with:
       - S = S' composed with S. (Map all vars with the same shape as id1 in C to
         unique refreshed versions of type2, then map that over S)
       - C = C - C' (Remove all subtyping constraints involving vars of the same shape
         as id1 in C.)
       - S' applied to all future constraints. (So apply S to constraints first.)
         FIXME: Wait, but freshly added constraints when decomposing vars won't get
         the substitution applied, right? Add another function for that?
       - Also process these new constraints (without applying S' to them...):
         - S'(id1) <= type2 (Constraint for the new version of id1)
         - S'(C') (Constraints for all the new versions of vars with the same shape
           as id1)
  *)
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
  Substitution.compose types.substitution new_var_substitution;
  orient ~var ~type_ (fun subtype supertype ->
    constrain ~names ~types ~subtype ~supertype);
  Set.iter vars_with_same_shape ~f:(fun v ->
    Set.iter vars_with_same_shape ~f:(fun v' ->
      constrain ~names ~types ~subtype:(to_var v) ~supertype:(to_var v')))
;;

let rec constrain ~names ~types ~subtype ~supertype =
  let instantiate_alias (param_list : Type_param_name.t Unique_list.t) expr =
    let params = Type_param.Env_to_vars.create () in
    List.iter
      (param_list :> Type_param_name.t list)
      ~f:(fun p -> ignore (Type_param.Env_to_vars.find_or_add params p : Type_var.t));
    Internal_type.of_type_scheme expr ~params
  in
  let lookup_type names name args =
    let type_entry = Name_bindings.find_absolute_type_entry names name in
    if List.length args = Type_decl.arity (Name_bindings.Type_entry.decl type_entry)
    then type_entry
    else type_error "Partially applied type constructor" subtype supertype
  in
  if not (Hash_set.mem types.constrained_types (subtype, supertype))
  then (
    (* FIXME: cleanup *)
    (* print_s
      [%message
        "Type_bindings.constrain"
          (subtype : Internal_type.t)
          (supertype : Internal_type.t)
          (types : t)]; *)
    Hash_set.add types.constrained_types (subtype, supertype);
    (* FIXME: Must apply the current substitution to all generated constraints. *)
    let subtype = Substitution.apply_to_type types.substitution subtype in
    let supertype = Substitution.apply_to_type types.substitution supertype in
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
         constrain ~names ~types ~subtype:(instantiate_alias params expr) ~supertype
       | (_ : _ Type_decl.t) ->
         let type_entry2 = lookup_type names name2 args2 in
         (match Name_bindings.Type_entry.decl type_entry2 with
          | params, Alias expr ->
            constrain ~names ~types ~subtype ~supertype:(instantiate_alias params expr)
          | (_ : _ Type_decl.t) ->
            if not (Name_bindings.Type_entry.identical type_entry1 type_entry2)
            then type_error "Type application mismatch" subtype supertype;
            (* TODO: We don't know what the variance of the type parameters to the type are,
             so we conservatively assume they are invariant. Implement inference and
             manual specification of type parameter variance, similar to what OCaml does. *)
            iter2_types args1 args2 ~subtype ~supertype ~f:(fun arg1 arg2 ->
              constrain ~names ~types ~subtype:arg1 ~supertype:arg2;
              constrain ~names ~types ~subtype:arg2 ~supertype:arg1)))
    | Type_app (name, args), (Tuple _ | Function _ | Partial_function _) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | params, Alias expr ->
         constrain ~names ~types ~subtype:(instantiate_alias params expr) ~supertype
       | _ -> type_error "Type application mismatch" subtype supertype)
    | (Tuple _ | Function _ | Partial_function _), Type_app (name, args) ->
      (match Name_bindings.Type_entry.decl (lookup_type names name args) with
       | params, Alias expr ->
         constrain ~names ~types ~subtype ~supertype:(instantiate_alias params expr)
       | _ -> type_error "Type application mismatch" subtype supertype)
    | Function (args1, _effect_row1, res1), Function (args2, _effect_row2, res2) ->
      (* FIXME: We aren't constraining the effect rows here! (Also not in the other cases
         either.) It's also pretty unclear how subtyping on effect rows should work. This
         doesn't seem to make sense as-is. *)
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Same_length -> ()
       | Left_trailing _ | Right_trailing _ -> fun_arg_number_mismatch subtype supertype);
      constrain ~names ~types ~subtype:res1 ~supertype:res2
    | ( Partial_function (args1, effect_row1, id1)
      , Partial_function (args2, effect_row2, id2) ) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing args1_trailing ->
         (* FIXME: Left fun has more args, right fun is under-applied and must be total. *)
         constrain_effects_to_be_total ~names ~types effect_row2;
         (* FIXME: what's the subtyping direction for the remainder of function args? 
            Maybe it should work like returning a value, so covariant? *)
         constrain
           ~names
           ~types
           ~subtype:(Partial_function (args1_trailing, effect_row1, id1))
           ~supertype:(Var id2)
       | Right_trailing args2_trailing ->
         constrain_effects_to_be_total ~names ~types effect_row1;
         constrain
           ~names
           ~types
           ~subtype:(Var id1)
           ~supertype:(Partial_function (args2_trailing, effect_row2, id2))
       | Same_length -> constrain ~names ~types ~subtype:(Var id1) ~supertype:(Var id2))
    | Partial_function (args1, effect_row1, id), Function (args2, effect_row2, res) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing _ -> fun_arg_number_mismatch subtype supertype
       | Right_trailing args2_trailing ->
         constrain_effects_to_be_total ~names ~types effect_row1;
         let id' = Type_var.create () in
         constrain ~names ~types ~subtype:(Var id') ~supertype:res;
         constrain
           ~names
           ~types
           ~subtype:(Var id)
           ~supertype:(Partial_function (args2_trailing, effect_row2, id'))
       | Same_length -> constrain ~names ~types ~subtype:(Var id) ~supertype:res)
    | Function (args1, effect_row1, res), Partial_function (args2, effect_row2, id) ->
      (match
         Nonempty.iter2 args1 args2 ~f:(fun arg1 arg2 ->
           constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
       with
       | Left_trailing args1_trailing ->
         constrain_effects_to_be_total ~names ~types effect_row2;
         let id' = Type_var.create () in
         constrain ~names ~types ~subtype:res ~supertype:(Var id');
         constrain
           ~names
           ~types
           ~subtype:(Partial_function (args1_trailing, effect_row1, id'))
           ~supertype:(Var id)
       | Right_trailing _ -> fun_arg_number_mismatch subtype supertype
       | Same_length -> constrain ~names ~types ~subtype:res ~supertype:(Var id))
    | Tuple xs, Tuple ys ->
      iter2_types xs ys ~subtype ~supertype ~f:(fun arg1 arg2 ->
        constrain ~names ~types ~subtype:arg1 ~supertype:arg2)
    | Tuple _, (Function _ | Partial_function _)
    | Function _, Tuple _
    | Partial_function _, Tuple _ -> type_error "Types do not match" subtype supertype)

and constrain_effects ~names ~types ~subtype ~supertype =
  if not (Hash_set.mem types.constrained_effects (subtype, supertype))
  then (
    Hash_set.add types.constrained_effects (subtype, supertype);
    let ({ effects = subtype_effects; effect_var = subtype_var } : Internal_type.effects) =
      subtype
    and ({ effects = supertype_effects; effect_var = supertype_var }
          : Internal_type.effects)
      =
      supertype
    in
    (* FIXME: I think we need to just use variables only? Otherwise we'd have to represent
     subtracting some effects from a variable here. Maybe we have to do that anyway, to be
     able to represent handling effects, though. Yeah, we do. *)
    let subtype_only =
      Map.fold_symmetric_diff
        subtype_effects
        supertype_effects
        ~init:Effect_name.Absolute.Map.empty
        ~data_equal:(fun _ _ -> false)
        ~f:(fun subtype_only (effect_name, diff) ->
          (* FIXME: Somehow need to unify the unmatched parts with the variable *)
          match diff with
          | `Left args -> Map.add_exn subtype_only ~key:effect_name ~data:args
          | `Right _ -> subtype_only
          | `Unequal (args1, args2) ->
            (match
               (* TODO: Handle type parameter variance for effects. *)
               List.iter2 args1 args2 ~f:(fun arg1 arg2 ->
                 constrain ~names ~types ~subtype:arg1 ~supertype:arg2;
                 constrain ~names ~types ~subtype:arg2 ~supertype:arg1)
             with
             | Ok () -> subtype_only
             | Unequal_lengths ->
               compiler_bug [%message "Unequal number of arguments to effect types"]))
    in
    match subtype_var, supertype_var with
    | None, None ->
      if not (Map.is_empty subtype_only)
      then
        Compilation_error.raise
          Type_error
          ~msg:
            [%message
              "Found more effects than expected"
                ~_:(subtype_only : Internal_type.t list Effect_name.Absolute.Map.t)]
    | Some subtype_var, _ ->
      (* FIXME: Code duplication with code for types *)
      (* FIXME: Is it really ok to ignore the supertype var above? Is this separate
         handling of effect vars and effects correct? (probably not) *)
      (* FIXME: Robust effect occurs check *)
      if occurs_in_effects subtype_var supertype
      then Compilation_error.raise Type_error ~msg:[%message "Occurs check failed"];
      check_var_vs_type
        ~names
        ~types
        ~var:subtype_var
        ~type_:supertype
        ~var_side:`Left
        ~constrain:constrain_effects
        ~to_var:(fun var : Internal_type.effects ->
          { effect_var = Some var; effects = Effect_name.Absolute.Map.empty })
        ~set_substitution:Substitution.set_effects
        ~refresh:refresh_effects
    | None, Some supertype_var ->
      (* FIXME: Robust effect occurs check *)
      if occurs_in_effects supertype_var subtype
      then Compilation_error.raise Type_error ~msg:[%message "Occurs check failed"];
      check_var_vs_type
        ~names
        ~types
        ~var:supertype_var
        ~type_:subtype
        ~var_side:`Right
        ~constrain:constrain_effects
        ~to_var:(fun var : Internal_type.effects ->
          { effect_var = Some var; effects = Effect_name.Absolute.Map.empty })
        ~set_substitution:Substitution.set_effects
        ~refresh:refresh_effects)

and constrain_effects_to_be_total ~names ~types effects =
  constrain_effects ~names ~types ~subtype:effects ~supertype:Internal_type.no_effects
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

(* FIXME: function variance goes from contra -> co.
   You can reduce the set of possible inputs or increase the set of possible outputs
   and get a supertype.
   A -> B is a subtype of C -> D iff C is a subtype of A and B is a subtype of D.

   When we take the "union" of two function types, we're trying to find a function
   type which is a supertype of both of them. Maybe we can just consider the
   effect types? Hmm, but now we've introduced subtyping in, it might be anywhere.

   e.g. For
   `Int -> <> (Int -> <Foo> Int)`
   and
   `Int -> <> (Int -> <Bar> Int)`
   we can say the union is `Int -> <> (Int -> <Foo,Bar> Int)`

   And for
   `(Int -> <Foo> Int) -> <> Int`
   and
   `(Int -> <Bar> Int) -> <> Int`
   we can say the union is `(Int -> <> Int) -> <> Int` by finding a type which is
   a subtype of both of the effects in the argument functions. So this is
   intersection instead of union.
*)
(* FIXME: We can't just eagerly union everything. We need to be able to union variables
   and known types as part of bounds. We need to represent unions explicitly in the
   intermediate types. *)
(* let rec union_types t1 t2 =
  Internal_type.map2 t1 t2 ~var:Fn.const ~eff:union_effects ~f_contra:(fun (t1, t2) ->
    Halt (intersect_types t1 t2))

and intersect_types t1 t2 =
  Internal_type.map2 t1 t2 ~var:Fn.const ~eff:intersect_effects ~f_contra:(fun (t1, t2) ->
    Halt (union_types t1 t2))

(* FIXME: implement *)
and union_effects _ _ = failwith "FIXME: union_effects"
and intersect_effects _ _ = failwith "FIXME: intersect_effects" *)

module Polarity = struct
  module T = struct
    type t =
      | Positive
      | Negative
    [@@deriving sexp, compare]

    let flip = function
      | Positive -> Negative
      | Negative -> Positive
    ;;
  end

  include T
  include Comparable.Make (T)
end

module Polar_var = struct
  include Tuple.Make (Polarity) (Type_var)
  include Tuple.Comparable (Polarity) (Type_var)
end

(* FIXME: Simplify constrains. See https://arxiv.org/pdf/1312.2334.pdf.
   
   IDEA:
   - unify constraints by decomposition. For each bound on a type parameter which is a
     non-parameter type, get a copy of that type with fresh type variables and unify it
     with the parameter. This reflects the fact that this is the only way to satisfy the
     bound.
   - decomposition requires a nontrivial occurs check
   - When decomposing, we need to replace the variable with its new bound. Maybe we could
     do this iteratively as we unify normally?
   *)

(* FIXME: How does generalization work now? We need to reify the information from the
   gathered constraints and substitution. Yes, we need to (optionally) simplify the 
   constraints, then apply the substitution, and return the constraints. *)
(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types outer_type =
  let generalize_type_var ~env ~in_progress ~polarity ~var =
    if Set.mem in_progress (polarity, var)
    then
      Compilation_error.raise
        Type_error
        ~msg:
          [%message
            "Recursive type"
              ~recursive_var:(polarity, var : Polar_var.t)
              (outer_type : Internal_type.t)]
    else Type_param.Env_of_vars.find_or_add env var
    (* let bounds = find_var_bounds types var ~polarity in
        let in_progress = Set.add in_progress (polarity, var) in
        collapse_var_bounds types var bounds ~env ~in_progress ~polarity *)
  in
  let rec generalize_internal types typ ~env ~in_progress ~(polarity : Polarity.t)
    : Module_path.absolute Type_scheme.t
    =
    (* FIXME: cleanup *)
    print_s
      [%message "generalize_internal" (typ : Internal_type.t) (polarity : Polarity.t)];
    match typ with
    | Var var -> Var (generalize_type_var ~env ~in_progress ~polarity ~var)
    | Function (args, effects, res) ->
      Function
        ( Nonempty.map
            args
            ~f:
              (generalize_internal
                 types
                 ~env
                 ~in_progress
                 ~polarity:(Polarity.flip polarity))
        , generalize_effects_internal types effects ~env ~in_progress ~polarity
        , generalize_internal types res ~env ~in_progress ~polarity )
    | Partial_function (args, effects, result_var) ->
      Function
        ( Nonempty.map
            args
            ~f:
              (generalize_internal
                 types
                 ~env
                 ~in_progress
                 ~polarity:(Polarity.flip polarity))
        , generalize_effects_internal types effects ~env ~in_progress ~polarity
        , Var (generalize_type_var ~env ~in_progress ~polarity ~var:result_var) )
      (* combine_partial_functions types args effects id ~env ~in_progress ~polarity *)
    | Type_app (name, fields) ->
      Type_app
        (name, List.map fields ~f:(generalize_internal types ~env ~in_progress ~polarity))
    | Tuple fields ->
      Tuple (List.map fields ~f:(generalize_internal types ~env ~in_progress ~polarity))
  (* FIXME: cleanup/remove *)
  (* and _find_var_bounds _types var ~polarity =
    (* FIXME: var bounds kinda aren't a thing anymore? What should we do here? *)
    match Hashtbl.find (failwith "type vars") var with
    (* FIXME: How do we do substitution for type variables now that we have effect
          types? I think we need to encode the constraints in the type expression, maybe?
          Hmm, I think all the lower/upper bounds are going to look basically the same,
          modulo effect types. So, roughly, the lowermost bound produces the fewest
          effects, and the uppermost bound produces the most.  *)
    (* FIXME: Can we avoid infinite recursion without supporting recursive types?
          Maybe we could eagerly collapse upper/lower bounds into single types? Slightly
          annoying since we'd need to introduce the concept of polarity to [constrain],
          but it might work? It should be fine to always start at Positive. But this might
          effectively be eager substitution which could be questionable. *)
    (* FIXME: union/intersection (at least currently) don't work for type variables, so
          the below is always going to fail (if it doesn't infinitely recurse).
          We can't handle vars being in bounds with other types. *)
    (* FIXME: example
          We want `(::) : a, List a -> List a` and have `(::) : 124`

          Constraints:
          124
            > 143, List 143 -> List 143
            < 125, List 125 -> List 125
          125
            < 143; 143
          143
            < 125

          Are 124 < 143 and 143 < 125 supposed to be impossible constraints to generate?
          Maybe that's the problem? Not sure.

          The algorithm in the paper would give:
          

          Maybe we just don't need subtyping?? I think we want it so that when we return
          things that contain functions, the types can be implicitly upcasted to contain 
          any effect. We could theoretically do that with Koka's row types which are 
          given fresh type variables for each use. We want the inferred effect types to be
          nice though.
       *)
    | Some { lower_bounds; upper_bounds } ->
      Queue.to_list
        (match polarity with
         | Positive -> lower_bounds
         | Negative -> upper_bounds)
    | None -> [] *)
  (* and collapse_var_bounds types var bounds ~env ~in_progress ~polarity =
    let combine_types =
      match polarity with
      | Positive -> Type_scheme.union
      | Negative -> Type_scheme.intersection
    in
    (* FIXME: The algorithm from the paper includes [typ], the type itself, in the
     union/intersection of the bounds. Do we have to do that? (I think yes) *)
    let bounds : _ Type_scheme.t Nonempty.t =
      Var (Type_param.Env_of_vars.find_or_add env var)
      :: List.map bounds ~f:(generalize_internal types ~env ~in_progress ~polarity)
    in
    (* FIXME: Also need to simplify away type variables. *)
    combine_types bounds *)
  (* FIXME: Implement this *)
  (* and substitute_effects types {effects ; effect_vars} ~polarity =
  let () =
    (* FIXME: deduplicate var handling code with regular types *)
    match Hashtbl.find types.effect_vars id with
    | Some { lower_bounds; upper_bounds } ->
      let bounds, combine_types =
        match polarity with
        | Positive -> lower_bounds, union_types
        | Negative -> upper_bounds, intersect_types
      in
      (* FIXME: The algorithm from the paper includes [typ], the type itself, in the
            union/intersection of the bounds. Do we have to do that? (I think yes) *)
      (match Queue.to_list bounds with
       | [] -> Halt typ
       | bounds ->
         (* FIXME: Combining types won't work when there are var and non-var
               constraints. Need to figure out something for that. *)
         Halt
           (List.map bounds ~f:(substitute_internal types ~polarity)
            |> List.reduce_exn ~f:combine_types))
    | None -> Halt typ
  in
  () *)

  (* FIXME: The way we handle partial functions seems pretty sus. I don't think we can
   properly distinguish between a partial function that then gets later applied in the
   same function, or a function that really returns another function. Which one should we
   assume? *)
  (* and combine_partial_functions types args effects id ~env ~in_progress ~polarity =
    (* FIXME: We need to determine if [id] is constrained to the same shape as some
       function type (either subtype or supertype). Actually, I think what should happen
       is that the var gets substituted with a function type already. *)
    (* FIXME: Does this make any sense? In the var case we're just returning a var instead
     of the function?? *)
    (* FIXME: We should put some simplification steps in [find_var_bounds] so this
     identification of [Partial_function]s actually works. *)
    match find_var_bounds types id ~polarity with
    | [ Partial_function (args', effects', id') ] ->
      (* FIXME: Do we need this? (Surely yes) *)
      (* constrain_effects_to_be_total ~names ~types effect_row; *)
      let args_combined = Nonempty.(args @ args') in
      combine_partial_functions
        types
        args_combined
        effects'
        id'
        ~env
        ~in_progress:(Set.add in_progress (polarity, id))
        ~polarity
    | bounds ->
      Function
        ( Nonempty.map
            args
            ~f:
              (generalize_internal
                 types
                 ~env
                 ~in_progress
                 ~polarity:(Polarity.flip polarity))
        , generalize_effects_internal types effects ~env ~in_progress ~polarity
        , collapse_var_bounds types id bounds ~env ~in_progress ~polarity ) *)
  and generalize_effects_internal types effects ~env ~in_progress ~polarity =
    match effects with
    | { effects; effect_var = None } ->
      Option.map
        (convert_effect_map types effects ~env ~in_progress ~polarity)
        ~f:Type_scheme.effect_union
    | { effects; effect_var = Some effect_var } ->
      let effect_var = Type_param.Env_of_vars.find_or_add env effect_var in
      (match convert_effect_map types effects ~env ~in_progress ~polarity with
       | None -> Some (Effect_var effect_var)
       | Some effects ->
         Some (Effect_union (Effect_var effect_var :: Nonempty.to_list effects)))
  and convert_effect_map types effects ~env ~in_progress ~polarity =
    Option.map
      (Nonempty.of_list (Map.to_alist effects))
      ~f:(fun effects ->
        Nonempty.map effects ~f:(fun (effect_name, args) : _ Type_scheme.effects ->
          Effect
            ( effect_name
            , List.map args ~f:(generalize_internal types ~env ~in_progress ~polarity) )))
  in
  (* FIXME: cleanup *)
  print_s [%message "Type_bindings.generalize" (outer_type : Internal_type.t) (types : t)];
  generalize_internal
    types
    outer_type
    ~env:(Type_param.Env_of_vars.create ())
    ~in_progress:Polar_var.Set.empty
    ~polarity:Positive
;;

let%expect_test "unification cycles" =
  let types = create () in
  let names = Name_bindings.core in
  let print_constraints () =
    [%test_pred: Substitution.t] Substitution.is_empty types.substitution;
    print_s [%sexp (types.constraints : Constraints.t)]
  in
  print_constraints ();
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  let a = Internal_type.fresh_var () in
  let b = Internal_type.fresh_var () in
  let c = Internal_type.fresh_var () in
  let d = Internal_type.fresh_var () in
  constrain ~names ~types ~subtype:a ~supertype:b;
  print_constraints ();
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  [%expect {| ((type_vars ((0 (Var 1)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:b ~supertype:c;
  print_constraints ();
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  print_constraints ();
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  print_constraints ();
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:c ~supertype:d;
  print_constraints ();
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:d ~supertype:a;
  print_constraints ();
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}]
;;
