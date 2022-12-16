open Import

type t =
  | Int of int
  | Float of float
  | Char of Uchar.t
  | String of Ustring.t
[@@deriving compare, equal, hash, sexp, variants]

include Comparable.S with type t := t
include Hashable.S with type t := t

val typ : t -> Type.Concrete.t
