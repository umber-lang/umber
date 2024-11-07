open Import
open Names

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

include Hashable.S with type t := t

val fresh_var : unit -> t
val var : Type_var.t -> t
val tuple : t list -> t
val effects_of_var : Type_var.t -> effects
val map : t -> f:(t -> (t, t) Map_action.t) -> t
val map_vars : t -> f:(Type_var.t -> Type_var.t) -> t

val iter_vars
  :  t
  -> polarity:Polarity.t
  -> f:(Type_var.t -> polarity:Polarity.t -> unit)
  -> unit

val iter_effects_vars
  :  effects
  -> polarity:Polarity.t
  -> f:(Type_var.t -> polarity:Polarity.t -> unit)
  -> unit

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
  -> f:
       ('acc
        -> t
        -> polarity:Polarity.t
        -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> f_effects:
       ('acc
        -> effects
        -> polarity:Polarity.t
        -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> polarity:Polarity.t
  -> ('acc, 'final) Fold_action.t

val no_effects : effects
