open! Import

module Primitive : sig
  type t =
    | Int
    | Float
    | Char
    | String
  [@@deriving equal, sexp]

  val of_literal : Literal.t -> t
end

type t =
  | Primitive of Primitive.t
  | Block of (Cnstr_tag.t * t list) Nonempty.t
  | Function of t Nonempty.t * t
  | Abstract
[@@deriving equal, sexp]

val assert_equal : t -> t -> unit
val expect_block : t -> (Cnstr_tag.t * t list) Nonempty.t
val expect_function : t -> t Nonempty.t * t
val of_type_scheme : names:Name_bindings.t -> Type.Scheme.t -> t
val arity : t -> int
