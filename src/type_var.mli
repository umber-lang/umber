open Import

type t [@@deriving sexp, compare, equal, hash]

val create : unit -> t

include Stringable.S with type t := t
include Comparable.S with type t := t
include Hashable.S with type t := t
