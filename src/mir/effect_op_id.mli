open! Core
open! Import

type t [@@deriving sexp_of]

val create : effect_operation_name:Value_name.Absolute.t -> t
val to_int : t -> int
