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

type t =
  { type_vars : Internal_type.t var_bounds Type_var.Table.t
  ; effect_vars : Effect_type.t var_bounds Type_var.Table.t
  ; constrained_types : Type_pair.Hash_set.t
  ; constrained_effects : Effect_type_pair.Hash_set.t
  }
[@@deriving sexp_of]

let create () =
  { type_vars = Type_var.Table.create ()
  ; effect_vars = Type_var.Table.create ()
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

let get_var_bounds =
  Hashtbl.find_or_add ~default:(fun () ->
    { lower_bounds = Queue.create (); upper_bounds = Queue.create () })
;;

let rec constrain ~names ~types ~subtype ~supertype =
  (* FIXME: Need a constraint cache to avoid infinite recursion *)
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
    Hash_set.add types.constrained_types (subtype, supertype);
    match subtype, supertype with
    | Var id1, Var id2 when Type_var.(id1 = id2) -> ()
    | Var id1, type2 ->
      if occurs_in id1 type2 then type_error "Occurs check failed" subtype supertype;
      let id1_bounds = get_var_bounds types.type_vars id1 in
      Queue.enqueue id1_bounds.upper_bounds type2;
      Queue.iter id1_bounds.lower_bounds ~f:(fun subtype ->
        constrain ~names ~types ~subtype ~supertype:type2)
    | type1, Var id2 ->
      if occurs_in id2 type1 then type_error "Occurs check failed" subtype supertype;
      let id2_bounds = get_var_bounds types.type_vars id2 in
      Queue.enqueue id2_bounds.lower_bounds type1;
      Queue.iter id2_bounds.upper_bounds ~f:(fun supertype ->
        constrain ~names ~types ~subtype:type1 ~supertype)
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
       doesn't seemm to make sense as-is. *)
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
      if occurs_in_effects subtype_var supertype
      then Compilation_error.raise Type_error ~msg:[%message "Occurs check failed"];
      let subtype_bounds = get_var_bounds types.effect_vars subtype_var in
      Queue.enqueue subtype_bounds.upper_bounds supertype;
      Queue.iter subtype_bounds.lower_bounds ~f:(fun bound ->
        constrain_effects ~names ~types ~subtype:bound ~supertype)
    | None, Some supertype_var ->
      if occurs_in_effects supertype_var subtype
      then Compilation_error.raise Type_error ~msg:[%message "Occurs check failed"];
      let supertype_bounds = get_var_bounds types.effect_vars supertype_var in
      Queue.enqueue supertype_bounds.lower_bounds subtype;
      Queue.iter supertype_bounds.upper_bounds ~f:(fun bound ->
        constrain_effects ~names ~types ~subtype ~supertype:bound))

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
  type t =
    | Positive
    | Negative
  [@@deriving sexp_of]

  let flip = function
    | Positive -> Negative
    | Negative -> Positive
  ;;
end

(* FIXME: Need to simplify effect types using intersection and union. Also, maybe we
   shouldn't use Internal_type.map given how many of the variants we special-case. *)
let rec generalize_internal types typ ~env ~(polarity : Polarity.t)
  : Module_path.absolute Type_scheme.t
  =
  (* FIXME: cleanup *)
  print_s [%message "generalize_internal" (typ : Internal_type.t) (polarity : Polarity.t)];
  match typ with
  | Var var ->
    let bounds = find_var_bounds types var ~polarity in
    collapse_var_bounds types var bounds ~env ~polarity
  | Function (args, effects, res) ->
    (* FIXME: substitute effects (on full/partial functions) *)
    Function
      ( Nonempty.map
          args
          ~f:(generalize_internal types ~env ~polarity:(Polarity.flip polarity))
      , generalize_effects_internal types effects ~env ~polarity
      , generalize_internal types res ~env ~polarity )
  | Partial_function (args, effects, id) ->
    combine_partial_functions types args effects id ~env ~polarity
  | Type_app (name, fields) ->
    Type_app (name, List.map fields ~f:(generalize_internal types ~env ~polarity))
  | Tuple fields -> Tuple (List.map fields ~f:(generalize_internal types ~env ~polarity))

and find_var_bounds types var ~polarity =
  match Hashtbl.find types.type_vars var with
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
  | None -> []

and collapse_var_bounds types var bounds ~env ~polarity =
  let combine_types =
    match polarity with
    | Positive -> Type_scheme.union
    | Negative -> Type_scheme.intersection
  in
  (* FIXME: The algorithm from the paper includes [typ], the type itself, in the
     union/intersection of the bounds. Do we have to do that? (I think yes) *)
  let bounds : _ Type_scheme.t Nonempty.t =
    Var (Type_param.Env_of_vars.find_or_add env var)
    :: List.map bounds ~f:(generalize_internal types ~env ~polarity)
  in
  (* FIXME: Also need to simplify away type variables. *)
  combine_types bounds

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

and combine_partial_functions types args effects id ~env ~polarity =
  (* FIXME: Does this make any sense? In the var case we're just returning a var instead
     of the function?? *)
  (* FIXME: We should put some simplification steps in [find_var_bounds] so this
     identification of [Partial_function]s actually works. *)
  match find_var_bounds types id ~polarity with
  | [ Partial_function (args', effects', id') ] ->
    (* FIXME: Do we need this? (Surely yes) *)
    (* constrain_effects_to_be_total ~names ~types effect_row; *)
    let args_combined = Nonempty.(args @ args') in
    combine_partial_functions types args_combined effects' id' ~env ~polarity
  | bounds ->
    Function
      ( Nonempty.map
          args
          ~f:(generalize_internal types ~env ~polarity:(Polarity.flip polarity))
      , generalize_effects_internal types effects ~env ~polarity
      , collapse_var_bounds types id bounds ~env ~polarity )

and generalize_effects_internal types effects ~env ~polarity =
  match effects with
  | { effects; effect_var = None } ->
    Option.map
      (convert_effect_map types effects ~env ~polarity)
      ~f:Type_scheme.effect_union
  | { effects; effect_var = Some effect_var } ->
    let effect_var = Type_param.Env_of_vars.find_or_add env effect_var in
    (match convert_effect_map types effects ~env ~polarity with
     | None -> Some (Effect_var effect_var)
     | Some effects ->
       Some (Effect_union (Effect_var effect_var :: Nonempty.to_list effects)))

and convert_effect_map types effects ~env ~polarity =
  Option.map
    (Nonempty.of_list (Map.to_alist effects))
    ~f:(fun effects ->
      Nonempty.map effects ~f:(fun (effect_name, args) : _ Type_scheme.effects ->
        Effect (effect_name, List.map args ~f:(generalize_internal types ~env ~polarity))))
;;

(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types typ =
  (* FIXME: cleanup *)
  print_s [%message "Type_bindings.generalize" (typ : Internal_type.t) (types : t)];
  generalize_internal types typ ~env:(Type_param.Env_of_vars.create ()) ~polarity:Positive
;;

let%expect_test "unification cycles" =
  let types = create () in
  let names = Name_bindings.core in
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  let a = Internal_type.fresh_var () in
  let b = Internal_type.fresh_var () in
  let c = Internal_type.fresh_var () in
  let d = Internal_type.fresh_var () in
  constrain ~names ~types ~subtype:a ~supertype:b;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:b ~supertype:c;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:c ~supertype:d;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}];
  constrain ~names ~types ~subtype:d ~supertype:a;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}]
;;
