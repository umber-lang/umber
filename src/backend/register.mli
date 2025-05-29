open! Core
open! Import

module Real : sig
  type t =
    | Rax
    | Rcx
    | Rdx
    | Rbx
    | Rsp
    | Rbp
    | Rsi
    | Rdi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving compare, equal, hash, sexp, enumerate]

  val pp : Format.formatter -> t -> unit

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Virtual : module type of Unique_counter.Make ()

type t =
  | Real of Real.t
  | Virtual of Virtual.t
[@@deriving compare, equal, hash, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
