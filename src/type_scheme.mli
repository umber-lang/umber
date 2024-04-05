open! Import
open Names

type 'n type_ =
  | Var of Type_param_name.t
  | Type_app of 'n Type_name.Qualified.t * 'n type_ list
  (* FIXME: Have tuple use Non_single_list.t *)
  | Tuple of 'n type_ list
  | Function of 'n type_ Nonempty.t * 'n effects * 'n type_
  | Union of 'n type_ Non_single_list.t
  | Intersection of 'n type_ Non_single_list.t

(* TODO: Make this private and give it smart constructors that automatically simplify
   unions, etc. *)
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

val var : Type_param_name.t -> 'n type_
val tuple : 'n type_ list -> 'n type_
val union : 'n type_ Non_single_list.t -> 'n type_
val union_list : 'n type_ list -> 'n type_
val intersection : 'n type_ Non_single_list.t -> 'n type_
val effect_var : Type_param_name.t -> 'n effects
val effect_union : 'n effects Non_single_list.t -> 'n effects
val effect_union_list : 'n effects list -> 'n effects
val effect_intersection : 'n effects Non_single_list.t -> 'n effects

val map
  :  ?f:('n1 type_ -> ('n1 type_, 'n2 type_) Map_action.t)
  -> ?f_effects:('n1 effects -> ('n1 effects, 'n2 effects) Map_action.t)
  -> 'n1 type_
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 type_

val map'
  :  ?f:('n1 type_ -> ('n1 type_, 'n2 type_) Map_action.t)
  -> ?f_effects:('n1 effects -> ('n1 effects, 'n2 effects) Map_action.t)
  -> 'n1 t
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 t

val map_effects
  :  ?f:('n1 type_ -> ('n1 type_, 'n2 type_) Map_action.t)
  -> ?f_effects:('n1 effects -> ('n1 effects, 'n2 effects) Map_action.t)
  -> 'n1 effects
  -> type_name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
  -> effect_name:('n1 Effect_name.Qualified.t -> 'n2 Effect_name.Qualified.t)
  -> 'n2 effects

val map_vars : 'n type_ -> f:(Type_param.t -> Type_param.t) -> 'n type_

val fold_until
  :  'n type_
  -> init:'acc
  -> f:('acc -> 'n type_ -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> f_effects:
       ('acc -> 'n effects -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_effects_until
  :  'n effects
  -> init:'acc
  -> f:('acc -> 'n type_ -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> f_effects:
       ('acc -> 'n effects -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_vars : 'n type_ -> init:'acc -> f:('acc -> Type_param_name.t -> 'acc) -> 'acc
val for_all_vars : 'n type_ -> f:(Type_param_name.t -> bool) -> bool

(* TODO: Remove/rework this, since constraints are part of regular type schemes. *)
module Bounded : sig
  type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
end
