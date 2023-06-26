open! Core

type 'a t = private 'a list [@@deriving compare, equal, hash, sexp]

val of_list : 'a list -> compare:('a -> 'a -> int) -> ('a t, 'a) result
val of_list_exn : 'a list -> compare:('a -> 'a -> int) -> 'a t
val empty : _ t
val singleton : 'a -> 'a t
