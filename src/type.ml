open Import
open Names
module Var_id = Unique_id.Int ()

(* TODO: Type declarations/expressions should have spans like other parts of the AST do. *)

module Expr = struct
  type ('v, 'pf, 'n) t =
    | Var of 'v
    | Type_app of 'n Type_name.Qualified.t * ('v, 'pf, 'n) t list
    | Tuple of ('v, 'pf, 'n) t list
    | Function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * ('v, 'pf, 'n) t
    | Partial_function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * 'pf

  (* FIXME: It should be an effect path, not a name *)
  and ('v, 'pf, 'n) effects =
    { effects : ('v, 'pf, 'n) t list Map.M(Effect_name).t
    ; effect_var : 'v option
    }
  [@@deriving hash, compare, equal, sexp]

  let var v = Var v
  let tuple list = Tuple list

  let rec map ?(f = Map_action.defer) typ ~var ~pf ~name =
    match f typ with
    | Halt typ -> typ
    | Retry typ -> map typ ~f ~var ~pf ~name
    | Defer typ ->
      (match typ with
       | Var v -> Var (var v)
       | Type_app (name', fields) ->
         Type_app (name name', List.map fields ~f:(map ~f ~var ~pf ~name))
       | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var ~pf ~name))
       | Function (args, effects, body) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
         let effects = map_effects effects ~f ~var ~pf ~name in
         Function (args, effects, map ~f ~var ~pf ~name body)
       | Partial_function (args, effects, v) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
         let effects = map_effects effects ~f ~var ~pf ~name in
         Partial_function (args, effects, pf v))

  and map_effects { effects; effect_var } ~f ~var ~pf ~name =
    let effects =
      Map.map effects ~f:(fun args -> List.map args ~f:(map ~f ~var ~pf ~name))
    in
    let effect_var = Option.map effect_var ~f:var in
    { effects; effect_var }
  ;;

  let rec map2 ?(f = Map_action.defer) ?(f_contra = f) type1 type2 ~var ~pf ~name ~eff =
    match f (type1, type2) with
    | Halt typ -> typ
    | Retry (type1, type2) -> map2 type1 type2 ~f ~f_contra ~var ~pf ~name ~eff
    | Defer (type1, type2) ->
      (match type1, type2 with
       | Var v1, Var v2 -> Var (var v1 v2)
       | Type_app (name1, args1), Type_app (name2, args2) ->
         assert_or_compiler_bug
           ~here:[%here]
           ([%equal: _ Type_name.Qualified.t] name1 name2);
         Type_app
           ( name name1
           , List.map2_exn args1 args2 ~f:(map2 ~f ~f_contra ~var ~pf ~name ~eff) )
       | Tuple fields1, Tuple fields2 ->
         Tuple (List.map2_exn fields1 fields2 ~f:(map2 ~f ~f_contra ~var ~pf ~name ~eff))
       | Function (args1, effect_row1, res1), Function (args2, effect_row2, res2) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~name ~eff)
         in
         let effect_row = eff effect_row1 effect_row2 in
         let res = map2 res1 res2 ~f ~f_contra ~var ~pf ~name ~eff in
         Function (args, effect_row, res)
       | ( Partial_function (args1, effect_row1, v1)
         , Partial_function (args2, effect_row2, v2) ) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~name ~eff)
         in
         let effect_row = eff effect_row1 effect_row2 in
         let v = pf v1 v2 in
         Partial_function (args, effect_row, v)
       | Partial_function _, Function _
       | Function _, Partial_function _
       | Var _, (Type_app _ | Tuple _ | Function _ | Partial_function _)
       | Type_app _, (Var _ | Tuple _ | Function _ | Partial_function _)
       | Tuple _, (Var _ | Type_app _ | Function _ | Partial_function _)
       | Function _, (Var _ | Type_app _ | Tuple _)
       | Partial_function _, (Var _ | Type_app _ | Tuple _) ->
         compiler_bug
           [%message
             "Incompatible types for map2" (type1 : (_, _, _) t) (type2 : (_, _, _) t)])
  ;;

  let rec fold_until typ ~init ~f =
    match (f init typ : _ Fold_action.t) with
    | Stop _ as stop -> stop
    | Continue init as continue ->
      (match typ with
       | Var _ -> continue
       | Type_app (_, fields) | Tuple fields ->
         List.fold_until fields ~init ~f:(fun init -> fold_until ~init ~f)
       | Function (args, _, body) ->
         let%bind.Fold_action init =
           Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
         in
         fold_until body ~init ~f
       | Partial_function (args, _, _) ->
         Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f))
  ;;

  let fold_vars typ ~init ~f =
    fold_until typ ~init ~f:(fun acc -> function
      | Var var -> Continue (f acc var)
      | _ -> Continue acc)
    |> Fold_action.id
  ;;

  let for_all_vars typ ~f =
    fold_until typ ~init:true ~f:(fun _ -> function
      | Var var -> if f var then Continue true else Stop false
      | _ -> Continue true)
    |> Fold_action.id
  ;;

  let exists_var typ ~f =
    fold_until typ ~init:false ~f:(fun _ -> function
      | Var var -> if f var then Stop true else Continue false
      | _ -> Continue false)
    |> Fold_action.id
  ;;

  (* FIXME: Idea: sequenced actions add effects to the environment. Conceptually, there
     are a set of effects. Complicating this is effect polymorphism. Calls to function
     parameters produce some variable set of effects, represented with effect variables.
     When combining calls to multiple functions, we want to include both effect variables.
     We can do this by unifying them.
     
     Let's remove effect variables which are unified later. Also want to remove
     duplicates later. *)
  (* let union_effects effects1 effects2 = effects1 @ effects2 *)

  (* FIXME: Maybe just make effects a set? *)
  (* FIXME: Since we don't consider unified type variables to be equal, this may give 
     stricter types than necessary. Integrate this and union with `Type_bindings` so it
     can deal with this.
     
     Is this even going to be correct? Maybe effect_row needs to explicitly model
     intersection? *)
  (* let intersect_effects effects1 effects2 =
    List.filter
      effects1
      ~f:(List.mem effects2 ~equal:[%equal: (Var_id.t, Var_id.t, _) effect])
  ;; *)

  let no_effects = { effects = Effect_name.Map.empty; effect_var = None }
end
