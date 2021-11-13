open Import
open Names
module Var_id : module type of Unique_id.Int ()

module Param : sig
  (* TODO: consider adding support for weak type variables here *)
  type t [@@deriving compare, equal, sexp_of]

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

  val name : t -> Type_param_name.t
  val id : t -> Var_id.t
  val of_name : env:Env_to_vars.t -> Type_param_name.t -> t
  val of_var : env:Env_of_vars.t -> Var_id.t -> t
  val create : Var_id.t -> Type_param_name.t -> t
  val equal_names : t -> t -> bool
  val dummy : t
end

module Expr : sig
  type ('v, 'pf) t =
    | Var of 'v
    | Type_app of Type_name.Qualified.t * ('v, 'pf) t list
    | Tuple of ('v, 'pf) t list
    | Function of ('v, 'pf) t Nonempty.t * ('v, 'pf) t
    | Partial_function of ('v, 'pf) t Nonempty.t * 'pf
  [@@deriving compare, equal, hash, sexp, variants]

  val map
    :  ?f:(('v1, 'pf1) t -> (('v1, 'pf1) t, ('v2, 'pf2) t) Map_action.t)
    -> ('v1, 'pf1) t
    -> var:('v1 -> 'v2)
    -> pf:('pf1 -> 'pf2)
    -> ('v2, 'pf2) t

  val fold_until
    :  ('v, 'pf) t
    -> init:'acc
    -> f:('acc -> ('v, 'pf) t -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t

  val for_all_vars : ('v, _) t -> f:('v -> bool) -> bool
  val exists_var : ('v, _) t -> f:('v -> bool) -> bool
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

val fresh_var : unit -> t

module type Boundable = sig
  type nonrec t [@@deriving compare, hash, equal, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  module Bounded : sig
    type nonrec t = Trait_bound.t * t [@@deriving compare, equal, hash, sexp]
  end

  val instantiate
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> t
    -> (Var_id.t, _) Expr.t

  val instantiate_bounded
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> Bounded.t
    -> (Var_id.t, _) Expr.t
end

module Scheme_plain : Boundable with type t = (Type_param_name.t, Nothing.t) Expr.t

module Scheme : sig
  include Boundable with type t = (Param.t, Nothing.t) Expr.t

  val of_plain : ?env:Param.Env_to_vars.t -> Scheme_plain.t -> t
end

module Concrete : sig
  type t = (Nothing.t, Nothing.t) Expr.t [@@deriving compare, equal, hash, sexp]

  val cast : t -> ('v, 'pf) Expr.t

  (* TODO: Consider deleting this. Do we need it? *)
  val of_polymorphic_exn : ('v, 'pf) Expr.t -> t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Decl : sig
  type decl =
    | Abstract
    | Alias of Scheme_plain.t
    | Variants of (Cnstr_name.t * Scheme_plain.t list) list
    (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record *)
    | Record of (Value_name.t * Scheme_plain.t) Nonempty.t
  [@@deriving compare, equal, hash, sexp]

  type t = Type_param_name.t list * decl [@@deriving compare, equal, hash, sexp]

  val arity : t -> int
  val map_exprs : t -> f:(Scheme_plain.t -> Scheme_plain.t) -> t
  val fold_exprs : t -> init:'acc -> f:('acc -> Scheme_plain.t -> 'acc) -> 'acc
  val iter_exprs : t -> f:(Scheme_plain.t -> unit) -> unit
  val no_free_params : t -> bool
end
