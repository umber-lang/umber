open! Core
open! Import

(* FIXME: This isn't needed in the frontend, move it out *)

type t [@@deriving sexp, compare, equal, hash]

val create : unit -> t
val of_int : int -> t

include Stringable.S with type t := t
include Comparable.S with type t := t
include Hashable.S with type t := t

module For_testing : sig
  val reset_ids : unit -> unit
end
