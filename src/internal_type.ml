open Import
open Names

module T = struct
  type t =
    | Var of Type_var.t
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

let rec map typ ~f =
  match (f typ : _ Map_action.t) with
  | Halt typ -> typ
  | Retry typ -> map typ ~f
  | Defer typ ->
    (match typ with
     | Var v -> Var v
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
     | Var _, (Type_app _ | Tuple _ | Function _ | Partial_function _)
     | Type_app _, (Var _ | Tuple _ | Function _ | Partial_function _)
     | Tuple _, (Var _ | Type_app _ | Function _ | Partial_function _)
     | Function _, (Var _ | Type_app _ | Tuple _)
     | Partial_function _, (Var _ | Type_app _ | Tuple _) ->
       compiler_bug [%message "Incompatible types for map2" (type1 : t) (type2 : t)])
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
       (* FIXME: Ignoring effects *)
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

let no_effects = { effects = Effect_name.Absolute.Map.empty; effect_var = None }
