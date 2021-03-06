open Import

module Level : sig
  (** A number from 0-9 representing the precedence level (higher is tighter) *)
  type t = private int [@@deriving compare, equal, hash, sexp]

  include Comparable.Infix with type t := t

  val min : t
  val max : t
  val of_int_exn : int -> t
  val pred : t -> t option
  val succ : t -> t option
end

module Assoc : sig
  type t =
    | Non_assoc
    | Left
    | Right
  [@@deriving compare, equal, hash, sexp]

  val compatible : t -> t -> bool
end

type t = Assoc.t * Level.t [@@deriving compare, equal, hash, sexp]

val default : t
val of_decl_exn : Assoc.t -> int -> t
