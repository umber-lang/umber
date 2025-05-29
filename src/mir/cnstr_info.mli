open! Core
open! Import

type t [@@deriving sexp_of]

(* TODO: Consider exposing a function to zip with the list of args. I think the
   callsites generally do that, which leads to looking up each constructor separately. *)

val tag : t -> Cnstr.t -> Cnstr_tag.t
val cnstrs : t -> Cnstr.t list

val fold
  :  t
  -> init:'acc
  -> f:('acc -> Cnstr.t -> Cnstr_tag.t -> arg_count:int -> 'acc)
  -> 'acc

val of_variants : (Cnstr_name.t * int) list -> t
val of_tuple : int -> t
