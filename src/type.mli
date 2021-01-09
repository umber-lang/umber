open Import
open Names
module Var_id : module type of Unique_id.Int ()

module Primitive : sig
  type t =
    | Int
    | Float
    | Char
    | String
  [@@deriving compare, equal, hash, sexp]
end

module Param : sig
  type t = Type_param_name.t [@@deriving compare, equal, hash, sexp]

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
    | Primitive of Primitive.t
    | Function of 'var t * 'var t
    | Tuple of 'var t list
  [@@deriving compare, equal, hash, sexp, variants]

  val map_vars : 'a t -> f:('a -> 'b) -> 'b t

  val map
    :  'a t
    -> f:('a t -> [< `Halt of 'a t | `Defer of 'a t | `Retry of 'a t ])
    -> 'a t

  val fold_vars : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  val for_all_vars : 'a t -> f:('a -> bool) -> bool

  module Bounded : sig
    type nonrec t = Trait_bound.t * Param.t t [@@deriving compare, equal, hash, sexp]
  end
end

module Decl : sig
  type decl =
    | Abstract
    | Alias of Param.t Expr.t
    | Variants of (Cnstr_name.t * Param.t Expr.t list) list
    | Record of (Value_name.t * Param.t Expr.t) list
  [@@deriving sexp]

  type t = Param.t list * decl [@@deriving sexp]

  val arity : t -> int
  val map_exprs : t -> f:(Param.t Expr.t -> Param.t Expr.t) -> t
  val no_free_params : t -> bool
end

module Val_anot : sig
  type t = Fixity.t option * Expr.Bounded.t [@@deriving compare, equal, hash, sexp]
end

type t = Var_id.t Expr.t [@@deriving compare, hash, equal, sexp]

val fresh_var : unit -> t

module Scheme : sig
  type nonrec t = Param.t Expr.t [@@deriving compare, hash, equal, sexp]

  val instantiate
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> t
    -> Var_id.t Expr.t

  val instantiate_bounded
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> Expr.Bounded.t
    -> Var_id.t Expr.t
end

module Concrete : sig
  type t = Nothing.t Expr.t [@@deriving compare, equal, hash, sexp]

  val cast : t -> 'a Expr.t
end
