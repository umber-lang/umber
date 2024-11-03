open Import
open Names

module T = struct
  type t =
    | Var of Type_var.t
    | Never
    | Any
    | Type_app of Type_name.Absolute.t * t list
    | Tuple of t list
    | Function of t Nonempty.t * effects * t
    | Partial_function of t Nonempty.t * effects * Type_var.t

  and effects =
    { effects : t list Effect_name.Absolute.Map.t
    ; effect_var : Type_var.t option
    }
  [@@deriving hash, compare, equal, sexp]
end

include T
include Hashable.Make (T)

let fresh_var () = Var (Type_var.create ())
let var v = Var v
let tuple list = Tuple list

let effects_of_var var =
  { effects = Effect_name.Absolute.Map.empty; effect_var = Some var }
;;

let rec map typ ~f =
  match (f typ : _ Map_action.t) with
  | Halt typ -> typ
  | Retry typ -> map typ ~f
  | Defer typ ->
    (match typ with
     | Var _ | Never | Any -> typ
     | Type_app (name, fields) -> Type_app (name, List.map fields ~f:(map ~f))
     | Tuple fields -> Tuple (List.map fields ~f:(map ~f))
     | Function (args, effects, body) ->
       let args = Nonempty.map args ~f:(map ~f) in
       let effects = map_effects effects ~f in
       Function (args, effects, map ~f body)
     | Partial_function (args, effects, v) ->
       let args = Nonempty.map args ~f:(map ~f) in
       let effects = map_effects effects ~f in
       Partial_function (args, effects, v))

and map_effects { effects; effect_var } ~f =
  let effects = Map.map effects ~f:(List.map ~f:(map ~f)) in
  { effects; effect_var }
;;

let map_vars typ ~f =
  let map_effects { effects; effect_var } =
    { effects; effect_var = Option.map effect_var ~f }
  in
  map typ ~f:(function
    | Var v -> Halt (Var (f v))
    | (Never | Any) as typ -> Halt typ
    | Function (args, effects, result) ->
      Defer (Function (args, map_effects effects, result))
    | Partial_function (args, effects, v) ->
      Defer (Partial_function (args, map_effects effects, f v))
    | (Type_app _ | Tuple _) as type_ -> Defer type_)
;;

let rec map2 ?(f = Map_action.defer) ?(f_contra = f) type1 type2 ~var ~eff =
  match f (type1, type2) with
  | Halt typ -> typ
  | Retry (type1, type2) -> map2 type1 type2 ~f ~f_contra ~var ~eff
  | Defer (type1, type2) ->
    (match type1, type2 with
     | Never, Never -> Never
     | Any, Any -> Any
     | Var v1, Var v2 -> Var (var v1 v2)
     | Type_app (name1, args1), Type_app (name2, args2) ->
       assert_or_compiler_bug
         ~here:[%here]
         ([%equal: _ Type_name.Qualified.t] name1 name2);
       Type_app (name1, List.map2_exn args1 args2 ~f:(map2 ~f ~f_contra ~var ~eff))
     | Tuple fields1, Tuple fields2 ->
       Tuple (List.map2_exn fields1 fields2 ~f:(map2 ~f ~f_contra ~var ~eff))
     | Function (args1, effect_row1, res1), Function (args2, effect_row2, res2) ->
       let args = Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~eff) in
       let effect_row = eff effect_row1 effect_row2 in
       let res = map2 res1 res2 ~f ~f_contra ~var ~eff in
       Function (args, effect_row, res)
     | Partial_function (args1, effect_row1, v1), Partial_function (args2, effect_row2, v2)
       ->
       let args = Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~eff) in
       let effect_row = eff effect_row1 effect_row2 in
       let v = var v1 v2 in
       Partial_function (args, effect_row, v)
     | Partial_function _, Function _
     | Function _, Partial_function _
     | Never, (Var _ | Type_app _ | Tuple _ | Function _ | Partial_function _ | Any)
     | Any, (Var _ | Type_app _ | Tuple _ | Function _ | Partial_function _ | Never)
     | Var _, (Type_app _ | Tuple _ | Function _ | Partial_function _ | Never | Any)
     | Type_app _, (Var _ | Tuple _ | Function _ | Partial_function _ | Never | Any)
     | Tuple _, (Var _ | Type_app _ | Function _ | Partial_function _ | Never | Any)
     | Function _, (Var _ | Type_app _ | Tuple _ | Never | Any)
     | Partial_function _, (Var _ | Type_app _ | Tuple _ | Never | Any) ->
       compiler_bug [%message "Incompatible types for map2" (type1 : t) (type2 : t)])
;;

let rec fold_until typ ~init ~f ~f_effects ~polarity =
  match (f init typ ~polarity : _ Fold_action.t) with
  | Stop _ as stop -> stop
  | Continue (`Halt acc) -> Continue acc
  | Continue (`Defer init) ->
    (match typ with
     | Var _ | Never | Any -> Continue init
     | Type_app (_, fields) | Tuple fields ->
       (* FIXME: Handle contravariant type parameters in type applications *)
       List.fold_until fields ~init ~f:(fun init ->
         fold_until ~init ~f ~f_effects ~polarity)
     | Function (args, effects, body) ->
       let%bind.Fold_action init =
         let polarity = Polarity.flip polarity in
         Nonempty.fold_until args ~init ~f:(fun init ->
           fold_until ~init ~f ~f_effects ~polarity)
       in
       let%bind.Fold_action init =
         fold_effects_until effects ~init ~f ~f_effects ~polarity
       in
       fold_until body ~init ~f ~f_effects ~polarity
     | Partial_function (args, effects, _) ->
       let%bind.Fold_action init =
         let polarity = Polarity.flip polarity in
         Nonempty.fold_until args ~init ~f:(fun init ->
           fold_until ~init ~f ~f_effects ~polarity)
       in
       fold_effects_until effects ~init ~f ~f_effects ~polarity)

and fold_effects_until effects ~init ~f ~f_effects ~polarity =
  match (f_effects init effects ~polarity : _ Fold_action.t) with
  | Stop _ as stop -> stop
  | Continue (`Halt acc) -> Continue acc
  | Continue (`Defer init) ->
    let { effect_var = _; effects } = effects in
    Map.fold_until effects ~init ~f:(fun ~key:_ ~data:args init ->
      List.fold_until args ~init ~f:(fun init typ ->
        fold_until typ ~init ~f ~f_effects ~polarity))
;;

let fold_vars typ ~init ~f =
  fold_until
    typ
    ~init
    ~polarity:Positive
    ~f:(fun init typ ~polarity ->
      match typ with
      | Var var -> Continue (`Halt (f init var ~polarity))
      | _ -> Continue (`Defer init))
    ~f_effects:(fun init { effect_var; effects = _ } ~polarity ->
      let init = Option.fold effect_var ~init ~f:(fun init v -> f init v ~polarity) in
      Continue (`Defer init))
  |> Fold_action.id
;;

let iter_vars typ ~f = fold_vars typ ~init:() ~f:(fun () var ~polarity -> f var ~polarity)

(* FIXME: This doesn't include effect vars! That's no good *)
(* let fold_vars typ ~init ~f =
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
;; *)

let no_effects = { effects = Effect_name.Absolute.Map.empty; effect_var = None }
