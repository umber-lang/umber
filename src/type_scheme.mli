open! Import
open Names

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

val var : Type_param_name.t -> 'n type_
val tuple : 'n type_ list -> 'n type_
val union : 'n type_ Nonempty.t -> 'n type_
val intersection : 'n type_ Nonempty.t -> 'n type_
val effect_union : 'n effects Nonempty.t -> 'n effects

val map
  :  ?f:('n1 type_ -> ('n1 type_, 'n2 type_) Map_action.t)
  -> 'n1 type_
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 type_

val map'
  :  ?f:('n1 type_ -> ('n1 type_, 'n2 type_) Map_action.t)
  -> 'n1 t
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 t

val fold_until
  :  'n type_
  -> init:'acc
  -> f:('acc -> 'n type_ -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_vars : 'n type_ -> init:'acc -> f:('acc -> Type_param_name.t -> 'acc) -> 'acc
val for_all_vars : 'n type_ -> f:(Type_param_name.t -> bool) -> bool
val exists_var : 'n type_ -> f:(Type_param_name.t -> bool) -> bool

(* TODO: Remove/rework this, since constraints are part of regular type schemes. *)
module Bounded : sig
  type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
end
