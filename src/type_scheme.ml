open! Import
open Names

(* TODO: Type declarations/expressions should have spans like other parts of the AST do. *)

(* FIXME: Remove type unions. We should be able to simplify the types by unifying and
   simplifying constraints. *)

type 'n type_ =
  | Var of Type_param_name.t
  | Type_app of 'n Type_name.Qualified.t * 'n type_ list
  | Tuple of 'n type_ list
  | Function of 'n type_ Nonempty.t * 'n effects * 'n type_
  | Union of 'n type_ Non_single_list.t
  | Intersection of 'n type_ Non_single_list.t

and 'n effects =
  | Effect of 'n Effect_name.Qualified.t * 'n type_ list
  | Effect_var of Type_param_name.t
  | Effect_union of 'n effects Non_single_list.t
  | Effect_intersection of 'n effects Non_single_list.t
[@@deriving hash, compare, equal, sexp]

type constraint_ =
  { subtype : Type_param.t
  ; supertype : Type_param.t
  }
[@@deriving hash, compare, equal, sexp]

type 'n t = 'n type_ * constraint_ list [@@deriving hash, compare, equal, sexp]

let var v = Var v
let tuple ts = Tuple ts

(* FIXME: Union and intersection functions should expand nested unions/intersections.
   Except maybe we just replace Union/Intersection with Merged or something because
   it doesn't make sense to e.g. have an intersection in a positive position
   (can't happen based on how we generate them). *)

let union ts = Union ts
let intersection ts = Intersection ts
let effect_var v = Effect_var v

let effect_union_list ts =
  List.concat_map ts ~f:(function
    | Effect_union ts -> Non_single_list.to_list ts
    | (Effect _ | Effect_var _ | Effect_intersection _) as e -> [ e ])
  |> Non_single_list.of_list_convert ~make:(fun ts -> Effect_union ts) ~singleton:Fn.id
;;

let effect_union ts = effect_union_list (Non_single_list.to_list ts)
let effect_intersection ts = Effect_intersection ts
let union_list = Non_single_list.of_list_convert ~make:union ~singleton:Fn.id

let effect_union_list =
  Non_single_list.of_list_convert ~make:effect_union ~singleton:Fn.id
;;

let rec map
  ?(f = Map_action.defer)
  ?(f_effects = Map_action.defer)
  typ
  ~type_name
  ~effect_name
  =
  match f typ with
  | Halt typ -> typ
  | Retry typ -> map typ ~f ~f_effects ~type_name ~effect_name
  | Defer typ ->
    (match typ with
     | Var _ as typ -> typ
     | Type_app (name, fields) ->
       Type_app
         (type_name name, List.map fields ~f:(map ~f ~f_effects ~type_name ~effect_name))
     | Tuple fields ->
       Tuple (List.map fields ~f:(map ~f ~f_effects ~type_name ~effect_name))
     | Function (args, effects, result) ->
       let args = Nonempty.map args ~f:(map ~f ~f_effects ~type_name ~effect_name) in
       let effects = map_effects ~f ~f_effects effects ~type_name ~effect_name in
       Function (args, effects, map ~f ~f_effects ~type_name ~effect_name result)
     | Union types ->
       Union (Non_single_list.map types ~f:(map ~f ~f_effects ~type_name ~effect_name))
     | Intersection types ->
       Intersection
         (Non_single_list.map types ~f:(map ~f ~f_effects ~type_name ~effect_name)))

and map_effects
  ?(f = Map_action.defer)
  ?(f_effects = Map_action.defer)
  effects
  ~type_name
  ~effect_name
  =
  match f_effects effects with
  | Halt effects -> effects
  | Retry effects -> map_effects effects ~f ~f_effects ~type_name ~effect_name
  | Defer effects ->
    (match effects with
     | Effect_var _ as effects -> effects
     | Effect (name, args) ->
       Effect
         (effect_name name, List.map args ~f:(map ~f ~f_effects ~type_name ~effect_name))
     | Effect_union effects ->
       effect_union
         (Non_single_list.map
            effects
            ~f:(map_effects ~f ~f_effects ~type_name ~effect_name))
     | Effect_intersection effects ->
       Effect_intersection
         (Non_single_list.map
            effects
            ~f:(map_effects ~f ~f_effects ~type_name ~effect_name)))
;;

let map' ?f ?f_effects ((type_, constraints) : _ t) ~type_name ~effect_name =
  let type_ = map type_ ?f ?f_effects ~type_name ~effect_name in
  type_, constraints
;;

let rec fold_until typ ~init ~f =
  match (f init typ : _ Fold_action.t) with
  | Stop _ as stop -> stop
  | Continue init as continue ->
    (match typ with
     | Var _ -> continue
     | Type_app (_, fields) | Tuple fields ->
       List.fold_until fields ~init ~f:(fun init -> fold_until ~init ~f)
     | Union types | Intersection types ->
       Non_single_list.fold_until types ~init ~f:(fun init -> fold_until ~init ~f)
     | Function (args, effects, body) ->
       let%bind.Fold_action init =
         Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
       in
       let%bind.Fold_action init = fold_effects_until effects ~init ~f in
       fold_until body ~init ~f)

and fold_effects_until effects ~init ~f =
  match effects with
  | Effect_var _ -> Continue init
  | Effect (_, args) -> List.fold_until args ~init ~f
  | Effect_union effects | Effect_intersection effects ->
    Non_single_list.fold_until effects ~init ~f:(fun init effects ->
      fold_effects_until effects ~init ~f)
;;

(* FIXME: These functions don't include effect vars *)
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

module Bounded = struct
  type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
end
