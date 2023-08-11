open Import
open Names

module Expr : sig
  type ('v, 'pf, 'n) t =
    | Var of 'v
    | Type_app of 'n Type_name.Qualified.t * ('v, 'pf, 'n) t list
    | Tuple of ('v, 'pf, 'n) t list
    | Function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * ('v, 'pf, 'n) t
    | Partial_function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * 'pf

  (* FIXME: When unifying, we should just allow one effect variable, which may have some
     subtyping constraints. When generalizing, we should sub in unions. Lets not use the
     term effect_row, since we aren't using row types. We probably end up needing a type
     to represent user-facing types and an internal type for types during unification.
     
     This is a good opportunity to put the external (input and output) types and internal
     (for type-checking) types. Currently we roll everything into [Type.Expr.t]. This
     would let us simplify each of the types (e.g. external types don't have to mention
     partial application, and internal types can use [Var_id.t]). *)
  and ('v, 'pf, 'n) effects =
    { effects : ('v, 'pf, 'n) t list Effect_name.Map.t
    ; effect_var : 'v option
    }
  [@@deriving hash, compare, equal, sexp]

  val var : 'v -> ('v, 'pf, 'n) t
  val tuple : ('v, 'pf, 'n) t list -> ('v, 'pf, 'n) t

  val map
    :  ?f:(('v1, 'pf1, 'n1) t -> (('v1, 'pf1, 'n1) t, ('v2, 'pf2, 'n2) t) Map_action.t)
    -> ('v1, 'pf1, 'n1) t
    -> var:('v1 -> 'v2)
    -> pf:('pf1 -> 'pf2)
    -> name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
    -> ('v2, 'pf2, 'n2) t

  val map2
    :  ?f:
         (('v1, 'pf1, 'n1) t * ('v1, 'pf1, 'n1) t
          -> (('v1, 'pf1, 'n1) t * ('v1, 'pf1, 'n1) t, ('v2, 'pf2, 'n2) t) Map_action.t)
    -> ?f_contra:
         (('v1, 'pf1, 'n1) t * ('v1, 'pf1, 'n1) t
          -> (('v1, 'pf1, 'n1) t * ('v1, 'pf1, 'n1) t, ('v2, 'pf2, 'n2) t) Map_action.t)
    -> ('v1, 'pf1, 'n1) t
    -> ('v1, 'pf1, 'n1) t
    -> var:('v1 -> 'v1 -> 'v2)
    -> pf:('pf1 -> 'pf1 -> 'pf2)
    -> name:('n1 Type_name.Qualified.t -> 'n2 Type_name.Qualified.t)
    -> eff:
         (('v1, 'pf1, 'n1) effects
          -> ('v1, 'pf1, 'n1) effects
          -> ('v2, 'pf2, 'n2) effects)
    -> ('v2, 'pf2, 'n2) t

  val fold_until
    :  ('v, 'pf, 'n) t
    -> init:'acc
    -> f:('acc -> ('v, 'pf, 'n) t -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t

  val fold_vars : ('v, _, _) t -> init:'acc -> f:('acc -> 'v -> 'acc) -> 'acc
  val for_all_vars : ('v, _, _) t -> f:('v -> bool) -> bool
  val exists_var : ('v, _, _) t -> f:('v -> bool) -> bool
  val no_effects : _ effects
end

module Concrete : sig
  type t = (Nothing.t, Nothing.t, Module_path.absolute) Expr.t
  [@@deriving compare, equal, hash, sexp]

  val cast : (Nothing.t, Nothing.t, 'n) Expr.t -> ('v, 'pf, 'n) Expr.t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end
