open! Core
open! Import

type t =
  | Named of Cnstr_name.t
  | Tuple
[@@deriving compare, equal, sexp, variants]

include Comparable.S with type t := t
