open! Core
open! Import

type 'a t =
  | []
  | ( :: ) of 'a * 'a Nonempty.t
[@@deriving hash, compare, equal, sexp]

include Container.S1 with type 'a t := 'a t

val to_list : 'a t -> 'a list
val of_list_exn : 'a list -> 'a t
val of_list_convert : 'a list -> make:('a t -> 'b) -> singleton:('a -> 'b) -> 'b
val map : 'a t -> f:('a -> 'b) -> 'b t

val fold_until
  :  'a t
  -> init:'acc
  -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t
