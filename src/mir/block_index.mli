open! Core
open! Import

type t [@@deriving sexp]

val of_int : int -> t
val to_int : t -> int
