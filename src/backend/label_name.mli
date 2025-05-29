open! Core
open! Import

type t [@@deriving compare, equal, sexp_of]

include Stringable.S with type t := t
include Hashable.S with type t := t

val of_mir_name : Mir_name.t -> t
val of_extern_name : Extern_name.t -> t
