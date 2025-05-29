open! Core
open! Import

module Make () : sig
  type t [@@deriving compare, equal, sexp_of]

  val to_string : t -> string

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  module Counter : sig
    type id := t
    type t [@@deriving sexp_of]

    val create : unit -> t
    val next : t -> id
  end
end
