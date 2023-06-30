open Import
open Names
module Var_id : module type of Unique_id.Int ()

module Param : sig
  (* TODO: consider adding support for weak type variables here *)
  type t = Type_param_name.t [@@deriving compare, equal, sexp_of]

  val dummy : t

  include Comparable.S_plain with type t := t

  module Map : sig
    include module type of Map

    val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state
  end

  include Hashable.S_plain with type t := t

  module Env_to_vars : sig
    type t

    val create : unit -> t
    val find_or_add : t -> Type_param_name.t -> Var_id.t
  end

  module Env_of_vars : sig
    type t

    val create : unit -> t
    val find_or_add : t -> Var_id.t -> Type_param_name.t
  end
end

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
     to represent user-facing types and an internal type for types during unification. *)
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

  (* FIXME: cleanup*)
  (* val union
    :  (Var_id.t, Var_id.t, 'n) t
    -> (Var_id.t, Var_id.t, 'n) t
    -> (Var_id.t, Var_id.t, 'n) t

  val union_effects
    :  (Var_id.t, Var_id.t, 'n) effect_row
    -> (Var_id.t, Var_id.t, 'n) effect_row
    -> (Var_id.t, Var_id.t, 'n) effect_row

  val intersect
    :  (Var_id.t, Var_id.t, 'n) t
    -> (Var_id.t, Var_id.t, 'n) t
    -> (Var_id.t, Var_id.t, 'n) t

  val intersect_effects
    :  (Var_id.t, Var_id.t, 'n) effect_row
    -> (Var_id.t, Var_id.t, 'n) effect_row
    -> (Var_id.t, Var_id.t, 'n) effect_row

  val effect_is_total : _ effect_row -> bool *)

  val no_effects : _ effects
end

type t = (Var_id.t, Var_id.t, Module_path.absolute) Expr.t
[@@deriving compare, hash, equal, sexp]

val fresh_var : unit -> t

module Scheme : sig
  type nonrec 'n t = (Param.t, Nothing.t, 'n) Expr.t
  [@@deriving compare, hash, equal, sexp]

  type nonrec 'n effects = (Param.t, Nothing.t, 'n) Expr.effects
  [@@deriving compare, hash, equal, sexp]

  module Bounded : sig
    type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
  end

  val instantiate : ?params:Param.Env_to_vars.t -> 'n t -> (Var_id.t, _, 'n) Expr.t

  val instantiate_bounded
    :  ?params:Param.Env_to_vars.t
    -> 'n Bounded.t
    -> (Var_id.t, _, 'n) Expr.t
end

module Concrete : sig
  type t = (Nothing.t, Nothing.t, Module_path.absolute) Expr.t
  [@@deriving compare, equal, hash, sexp]

  val cast : (Nothing.t, Nothing.t, 'n) Expr.t -> ('v, 'pf, 'n) Expr.t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Decl : sig
  type 'n decl =
    | Abstract
    | Alias of 'n Scheme.t
    | Variants of (Cnstr_name.t * 'n Scheme.t list) list
    (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record. One problem with this is you
       can no longer define recursive record types, which is a bit annoying. *)
    | Record of (Value_name.t * 'n Scheme.t) Nonempty.t
  [@@deriving compare, equal, hash, sexp]

  type 'n t = Type_param_name.t Unique_list.t * 'n decl
  [@@deriving compare, equal, hash, sexp]

  val arity : 'n t -> int
  val map_exprs : 'n1 t -> f:('n1 Scheme.t -> 'n2 Scheme.t) -> 'n2 t
  val fold_exprs : 'n t -> init:'acc -> f:('acc -> 'n Scheme.t -> 'acc) -> 'acc
  val iter_exprs : 'n t -> f:('n Scheme.t -> unit) -> unit
  val no_free_params : 'n t -> bool
  val params_of_list : Type_param_name.t list -> Type_param_name.t Unique_list.t
end
