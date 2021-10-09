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
  type 'var t =
    | Var of 'var
    | Type_app of Type_name.Qualified.t * 'var t list
    | Function of 'var t Nonempty.t * 'var t
    | Tuple of 'var t list
  [@@deriving compare, equal, hash, sexp, variants]

  val map
    :  'a t
    -> f:('a t -> [< `Halt of 'a t | `Defer of 'a t | `Retry of 'a t ])
    -> 'a t

  val map_vars : 'a t -> f:('a -> 'b) -> 'b t
  val fold_vars : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  val for_all_vars : 'a t -> f:('a -> bool) -> bool

  module Bounded : sig
    type nonrec t = Trait_bound.t * Param.t t [@@deriving compare, equal, hash, sexp]
  end
end

type t = Var_id.t Expr.t [@@deriving compare, hash, equal, sexp]

val fresh_var : unit -> t

module Scheme : sig
  type nonrec t = Param.t Expr.t [@@deriving compare, hash, equal, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val instantiate
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> t
    -> Var_id.t Expr.t

  val instantiate_bounded
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> Expr.Bounded.t
    -> Var_id.t Expr.t

  val infer_param_map : template_type:t -> instance_type:t -> t Param.Map.t
end

module Concrete : sig
  type t = Nothing.t Expr.t [@@deriving compare, equal, hash, sexp]

  val cast : t -> 'a Expr.t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Decl : sig
  type 'var decl =
    | Abstract
    | Alias of 'var Expr.t
    | Variants of (Cnstr_name.t * 'var Expr.t list) list
    (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record *)
    | Record of (Value_name.t * 'var Expr.t) list
  [@@deriving compare, equal, hash, sexp]

  type t = Param.t list * Param.t decl [@@deriving compare, equal, hash, sexp]

  val arity : t -> int
  val map_exprs : t -> f:(Param.t Expr.t -> Param.t Expr.t) -> t
  val fold_exprs : t -> init:'acc -> f:('acc -> Param.t Expr.t -> 'acc) -> 'acc
  val iter_exprs : t -> f:(Param.t Expr.t -> unit) -> unit
  val no_free_params : t -> bool

  (* TODO: Remove this. I don't think I want to use it. *)
  module Monomorphic : sig
    type t = Nothing.t decl [@@deriving compare, equal, hash, sexp]
  end

  val monomorphize : t -> args:Concrete.t list -> Monomorphic.t
end
