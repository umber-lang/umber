open! Import

(** Constructor tags are represented as follows:
    - For constant constructors (i.e. constructors with no arguments), the tag is given
      inline as a 64-bit integer where the least significant bit is always set to 1.
      This is identical to the OCaml representation.
    - For non-constant constructors (i.e. those with arguments), the tag is given in a
      block header as the first 16 bits. In that case, as with any block, the pointer to
      the block will have its least signficiant bit set to 0. *)
type t [@@deriving compare, equal, hash, sexp]

(* TODO: what about putting constructor tags in the pointer sometimes? On 64-bit
   platforms we should have 3 free bits. This could be especially helpful for
   implementing unboxed options or similar types. *)

include Comparable.S with type t := t

val of_int : int -> t
val to_int : t -> int
val default : t
val closure : t
val int : t
val char : t
val float : t
val string : t
