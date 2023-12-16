open Import
open Names

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

include Hashable.S with type t := t

val fresh_var : unit -> t
val var : Type_var.t -> t
val tuple : t list -> t
val map : t -> f:(t -> (t, t) Map_action.t) -> t
val map_vars : t -> f:(Type_var.t -> Type_var.t) -> t

val map2
  :  ?f:(t * t -> (t * t, t) Map_action.t)
  -> ?f_contra:(t * t -> (t * t, t) Map_action.t)
  -> t
  -> t
  -> var:(Type_var.t -> Type_var.t -> Type_var.t)
  -> eff:(effects -> effects -> effects)
  -> t

val fold_until
  :  t
  -> init:'acc
  -> f:('acc -> t -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_vars : t -> init:'acc -> f:('acc -> Type_var.t -> 'acc) -> 'acc
val for_all_vars : t -> f:(Type_var.t -> bool) -> bool
val exists_var : t -> f:(Type_var.t -> bool) -> bool
val no_effects : effects
