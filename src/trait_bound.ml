open Import
open Names

type t = (Trait_name.t * Type_param_name.t Nonempty.t) list
[@@deriving compare, equal, hash, sexp, quickcheck]
