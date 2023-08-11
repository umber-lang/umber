open! Import
open Names

type 'n t =
  | Var of Type_param_name.t
  | Type_app of 'n Type_name.Qualified.t * 'n t list
  | Tuple of 'n t list
  | Function of 'n t Nonempty.t * 'n effects option * 'n t
  | Union of 'n t Nonempty.t
  | Intersection of 'n t Nonempty.t

and 'n effects =
  | Effect of 'n Effect_name.Qualified.t * 'n t list
  | Effect_var of Type_param_name.t
  | Effect_union of 'n effects Nonempty.t
  | Effect_intersection of 'n effects Nonempty.t
[@@deriving hash, compare, equal, sexp]

val var : Type_param_name.t -> 'n t
val tuple : 'n t list -> 'n t

val map
  :  ?f:('n1 t -> ('n1 t, 'n2 t) Map_action.t)
  -> 'n1 t
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 t

val fold_until
  :  'n t
  -> init:'acc
  -> f:('acc -> 'n t -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_vars : 'n t -> init:'acc -> f:('acc -> Type_param_name.t -> 'acc) -> 'acc
val for_all_vars : 'n t -> f:(Type_param_name.t -> bool) -> bool
val exists_var : 'n t -> f:(Type_param_name.t -> bool) -> bool

module Bounded : sig
  type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
end
