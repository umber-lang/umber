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
  let env = Type.Param.Env_of_vars.create () in
  let handle_var = Type.Param.Env_of_vars.find_or_add env in
  let map_type t =
    Type.Expr.map t ~var:handle_var ~pf:handle_var ~name:Fn.id
    |> [%sexp_of:
         (Type_param_name.t, Type_param_name.t, Module_path.absolute) Type.Expr.t]
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

type t =
  { type_vars : Type.t var_bounds Type.Var_id.Table.t
  ; effect_vars :
      (Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effects var_bounds
      Type.Var_id.Table.t
  }
[@@deriving sexp_of]

let create () =
  { type_vars = Type.Var_id.Table.create (); effect_vars = Type.Var_id.Table.create () }
;;

let rec occurs_in id : Type.t -> bool = function
  | Var id' -> Type.Var_id.(id = id')
  | Type_app (_, fields) | Tuple fields -> List.exists fields ~f:(occurs_in id)
  | Function (args, effects, body) ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effects id effects
    || occurs_in id body
  | Partial_function (args, effects, id') ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effects id effects
    || Type.Var_id.(id = id')

and occurs_in_effects id ({ effects; effect_var } : _ Type.Expr.effects) =
  Map.exists effects ~f:(List.exists ~f:(occurs_in id))
  || Option.exists effect_var ~f:(Type.Var_id.( = ) id)
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
            : (Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effects)]
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
    let params = Type.Param.Env_to_vars.create () in
    List.iter
      (param_list :> Type_param_name.t list)
      ~f:(fun p -> ignore (Type.Param.Env_to_vars.find_or_add params p : Type.Var_id.t));
    Type.Scheme.instantiate expr ~params
  in
  let lookup_type names name args =
    let type_entry = Name_bindings.find_absolute_type_entry names name in
    if List.length args = Type.Decl.arity (Name_bindings.Type_entry.decl type_entry)
    then type_entry
    else type_error "Partially applied type constructor" subtype supertype
  in
  match subtype, supertype with
  | Var id1, Var id2 when Type.Var_id.(id1 = id2) -> ()
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
     | (_ : _ Type.Decl.t) ->
       let type_entry2 = lookup_type names name2 args2 in
       (match Name_bindings.Type_entry.decl type_entry2 with
        | params, Alias expr ->
          constrain ~names ~types ~subtype ~supertype:(instantiate_alias params expr)
        | (_ : _ Type.Decl.t) ->
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
  | Partial_function (args1, effect_row1, id1), Partial_function (args2, effect_row2, id2)
    ->
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
       let id' = Type.Var_id.create () in
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
       let id' = Type.Var_id.create () in
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
  | Partial_function _, Tuple _ -> type_error "Types do not match" subtype supertype

and constrain_effects ~names ~types ~subtype ~supertype =
  let ({ effects = subtype_effects; effect_var = subtype_var } : _ Type.Expr.effects) =
    subtype
  and ({ effects = supertype_effects; effect_var = supertype_var } : _ Type.Expr.effects) =
    supertype
  in
  (* FIXME: I think we need to just use variables only? Otherwise we'd have to represent
     subtracting some effects from a variable here. Maybe we have to do that anyway, to be
     able to represent handling effects, though. Yeah, we do.  *)
  let subtype_only =
    Map.fold_symmetric_diff
      subtype_effects
      supertype_effects
      ~init:Effect_name.Map.empty
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
            "Found more effects than expected" ~_:(subtype_only : _ Effect_name.Map.t)]
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
      constrain_effects ~names ~types ~subtype ~supertype:bound)

and constrain_effects_to_be_total ~names ~types effects =
  constrain_effects ~names ~types ~subtype:effects ~supertype:Type.Expr.no_effects
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
let rec union_types t1 t2 =
  Type.Expr.map2
    t1
    t2
    ~var:Fn.const
    ~pf:Fn.const
    ~name:Fn.id
    ~eff:union_effects
    ~f_contra:(fun (t1, t2) -> Halt (intersect_types t1 t2))

and intersect_types t1 t2 =
  Type.Expr.map2
    t1
    t2
    ~var:Fn.const
    ~pf:Fn.const
    ~name:Fn.id
    ~eff:intersect_effects
    ~f_contra:(fun (t1, t2) -> Halt (union_types t1 t2))

(* FIXME: implement *)
and union_effects _ _ = failwith "FIXME: union_effects"
and intersect_effects _ _ = failwith "FIXME: intersect_effects"

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
   shouldn't use Type.Expr.map given how many of the variants we special-case. *)
let rec substitute_internal types typ ~(polarity : Polarity.t) =
  (* FIXME: cleanup *)
  (* print_s [%message "substitute_internal" (typ : Type.t) (polarity : Polarity.t)]; *)
  Type.Expr.map typ ~var:Fn.id ~pf:Fn.id ~name:Fn.id ~f:(fun typ ->
    match typ with
    | Var id ->
      (match Hashtbl.find types.type_vars id with
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
         let bounds, combine_types =
           match polarity with
           | Positive -> lower_bounds, union_types
           | Negative -> upper_bounds, intersect_types
         in
         Halt
           (Queue.fold bounds ~init:typ ~f:(fun acc bound ->
              combine_types acc (substitute_internal types bound ~polarity)))
       | None -> Halt typ)
    | Function (args, effects, res) ->
      (* FIXME: substitute effects (on full/partial functions) *)
      Halt
        (Function
           ( Nonempty.map
               args
               ~f:(substitute_internal types ~polarity:(Polarity.flip polarity))
           , effects
           , substitute_internal types res ~polarity ))
    | Partial_function (args, effect_row, id) ->
      combine_partial_functions types args effect_row id ~polarity
    | Tuple _ | Type_app _ -> Defer typ)

and combine_partial_functions types args effect_row id ~polarity =
  let args =
    Nonempty.map args ~f:(fun arg ->
      substitute_internal types arg ~polarity:(Polarity.flip polarity))
  in
  match substitute_internal types (Var id) ~polarity with
  | Var _ as typ -> Halt typ
  | Partial_function (args', effect_row', id') ->
    (* FIXME: Do we need this? *)
    (* constrain_effects_to_be_total ~names ~types effect_row; *)
    let args' =
      Nonempty.map args' ~f:(substitute_internal types ~polarity:(Polarity.flip polarity))
    in
    let args_combined = Nonempty.(args @ args') in
    combine_partial_functions types args_combined effect_row' id' ~polarity
  | (Type_app _ | Tuple _ | Function _) as type_sub ->
    Halt (Function (args, effect_row, type_sub))
;;

let substitute = substitute_internal ~polarity:Positive

(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types typ =
  (* FIXME: cleanup *)
  (* print_s [%message "Type_bindings.generalize" (typ : Type.t)]; *)
  let env = Type.Param.Env_of_vars.create () in
  Type.Expr.map
    (substitute types typ)
    ~var:(Type.Param.Env_of_vars.find_or_add env)
    ~pf:(never_happens [%here])
    ~name:Fn.id
    ~f:(function
      | Partial_function (args, effect_row, id) ->
        (* FIXME: fix effects *)
        Defer (Function (args, effect_row, Var id))
      | typ -> Defer typ)
;;

let%expect_test "unification cycles" =
  let types = create () in
  let names = Name_bindings.core in
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  let a = Type.fresh_var () in
  let b = Type.fresh_var () in
  let c = Type.fresh_var () in
  let d = Type.fresh_var () in
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
