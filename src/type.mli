open Import
open Names
module Var_id : module type of Unique_id.Int ()

module Param : sig
  (* TODO: consider adding support for weak type variables here *)
  (* TODO: consider hiding/breaking this type equality *)
  (* TODO: consider making this a unique int or something so that mapping over it can be
     done across types. *)
  type t = Type_param_name.t [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t

  module Map : sig
    include module type of Map

    val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state
  end

  include Hashable.S with type t := t

  module Env_to_vars : sig
    type param = t
    type t

    val create : unit -> t
    val find_or_add : t -> param -> Var_id.t
  end

  module Env_of_vars : sig
    type param = t
    type t

    val create : unit -> t
    val find_or_add : t -> Var_id.t -> param
  end
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

  (* TODO: cleanup *)
  (*val map_vars : ('v1, 'v1) t -> f:('v1 -> 'v2) -> ('v2, 'v2) t*)
  val fold_vars : ('v, _) t -> init:'acc -> f:('acc -> 'v -> 'acc) -> 'acc
  val for_all_vars : ('v, _) t -> f:('v -> bool) -> bool

  module Bounded : sig
    type nonrec t = Trait_bound.t * (Param.t, Nothing.t) t
    [@@deriving compare, equal, hash, sexp]
  end
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

val fresh_var : unit -> t

module Scheme : sig
  type nonrec t = (Param.t, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val instantiate
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> t
    -> (Var_id.t, _) Expr.t

  val instantiate_bounded
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> Expr.Bounded.t
    -> (Var_id.t, _) Expr.t

  val infer_param_map : template_type:t -> instance_type:t -> t Param.Map.t
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
    | Alias of Scheme.t
    | Variants of (Cnstr_name.t * Scheme.t list) list
    (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record *)
    | Record of (Value_name.t * Scheme.t) Nonempty.t
  [@@deriving compare, equal, hash, sexp]

  type t = Param.t list * decl [@@deriving compare, equal, hash, sexp]

  val arity : t -> int
  val map_exprs : t -> f:(Scheme.t -> Scheme.t) -> t
  val fold_exprs : t -> init:'acc -> f:('acc -> Scheme.t -> 'acc) -> 'acc
  val iter_exprs : t -> f:(Scheme.t -> unit) -> unit
  val no_free_params : t -> bool
end
