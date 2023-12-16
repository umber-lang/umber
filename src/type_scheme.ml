open! Import
open Names

(* TODO: Type declarations/expressions should have spans like other parts of the AST do. *)

(* FIXME: Remove type unions. We should be able to simplify the types by unifying and
   simplifying constraints. *)

type 'n type_ =
  | Var of Type_param_name.t
  | Type_app of 'n Type_name.Qualified.t * 'n type_ list
  | Tuple of 'n type_ list
  | Function of 'n type_ Nonempty.t * 'n effects option * 'n type_
  | Union of 'n type_ Nonempty.t
  | Intersection of 'n type_ Nonempty.t

and 'n effects =
  | Effect of 'n Effect_name.Qualified.t * 'n type_ list
  | Effect_var of Type_param_name.t
  | Effect_union of 'n effects Nonempty.t
  | Effect_intersection of 'n effects Nonempty.t
[@@deriving hash, compare, equal, sexp]

type 'n constraint_ =
  { subtype : 'n type_
  ; supertype : 'n type_
  }
[@@deriving hash, compare, equal, sexp]

type 'n t = 'n type_ * 'n constraint_ list [@@deriving hash, compare, equal, sexp]

let var v = Var v
let tuple ts = Tuple ts
let union ts = Union ts
let intersection ts = Intersection ts
let effect_union ts = Effect_union ts

let rec map ?(f = Map_action.defer) typ ~type_name ~effect_name =
  match f typ with
  | Halt typ -> typ
  | Retry typ -> map typ ~f ~type_name ~effect_name
  | Defer typ ->
    (match typ with
     | Var _ as typ -> typ
     | Type_app (name, fields) ->
       Type_app (type_name name, List.map fields ~f:(map ~f ~type_name ~effect_name))
     | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~type_name ~effect_name))
     | Function (args, effects, body) ->
       let args = Nonempty.map args ~f:(map ~f ~type_name ~effect_name) in
       let effects = Option.map effects ~f:(map_effects ~f ~type_name ~effect_name) in
       Function (args, effects, map ~f ~type_name ~effect_name body)
     | Union types -> Union (Nonempty.map types ~f:(map ~f ~type_name ~effect_name))
     | Intersection types ->
       Intersection (Nonempty.map types ~f:(map ~f ~type_name ~effect_name)))

and map_effects effects ~f ~type_name ~effect_name =
  match effects with
  | Effect_var _ as effects -> effects
  | Effect (name, args) ->
    Effect (effect_name name, List.map args ~f:(map ~f ~type_name ~effect_name))
  | Effect_union effects ->
    Effect_union (Nonempty.map effects ~f:(map_effects ~f ~type_name ~effect_name))
  | Effect_intersection effects ->
    Effect_intersection (Nonempty.map effects ~f:(map_effects ~f ~type_name ~effect_name))
;;

let map' ?f ((type_, constraints) : _ t) ~type_name ~effect_name =
  let type_ = map type_ ?f ~type_name ~effect_name in
  let constraints =
    List.map constraints ~f:(fun { subtype; supertype } ->
      { subtype = map subtype ?f ~type_name ~effect_name
      ; supertype = map supertype ?f ~type_name ~effect_name
      })
  in
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
       Nonempty.fold_until types ~init ~f:(fun init -> fold_until ~init ~f)
     | Function (args, effects, body) ->
       let%bind.Fold_action init =
         Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
       in
       let%bind.Fold_action init =
         match effects with
         | None -> Continue init
         | Some effects -> fold_effects_until effects ~init ~f
       in
       fold_until body ~init ~f)

and fold_effects_until effects ~init ~f =
  match effects with
  | Effect_var _ -> Continue init
  | Effect (_, args) -> List.fold_until args ~init ~f
  | Effect_union effects | Effect_intersection effects ->
    Nonempty.fold_until effects ~init ~f:(fun init effects ->
      fold_effects_until effects ~init ~f)
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

module Bounded = struct
  type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
end
