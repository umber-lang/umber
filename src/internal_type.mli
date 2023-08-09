open Import
open Names
module Var_id : Unique_id.Id

(* FIXME: Effects should use a separate var type. *)
type 'n t =
  | Var of Var_id.t
  | Type_app of 'n Type_name.Qualified.t * 'n t list
  | Tuple of 'n t list
  | Function of 'n t Nonempty.t * 'n effects * 'n t
  | Partial_function of 'n t Nonempty.t * 'n effects * Var_id.t

and 'n effects =
  { effects : ('n Effect_name.Qualified.t * 'n t list) list
  ; effect_var : Var_id.t option
  }
[@@deriving hash, compare, equal, sexp]

val var : Var_id.t -> 'n t
val tuple : 'n t list -> 'n t

val map
  :  ?f:('n1 t -> ('n1 t, 'n2 t) Map_action.t)
  -> 'n1 t
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 t

val map2
  :  ?f:('n1 t * 'n1 t -> ('n1 t * 'n1 t, 'n2 t) Map_action.t)
  -> ?f_contra:('n1 t * 'n1 t -> ('n1 t * 'n1 t, 'n2 t) Map_action.t)
  -> 'n1 t
  -> 'n1 t
  -> var:(Var_id.t -> Var_id.t -> Var_id.t)
  -> name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> eff:('n1 effects -> 'n1 effects -> 'n2 effects)
  -> 'n2 t

val fold_until
  :  'n t
  -> init:'acc
  -> f:('acc -> 'n t -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_vars : _ t -> init:'acc -> f:('acc -> Var_id.t -> 'acc) -> 'acc
val for_all_vars : _ t -> f:(Var_id.t -> bool) -> bool
val exists_var : _ t -> f:(Var_id.t -> bool) -> bool
val no_effects : _ effects
