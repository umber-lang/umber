open! Core

type t =
  | Positive
  | Negative
[@@deriving sexp, compare]

include Comparable.S with type t := t

val flip : t -> t
