open Import

(* TODO: This should almost certainly use the [Non_empty_list] library. Any extensions can
   still be implemented on top of that, like what we've done with [List] in util.ml *)

type 'a t = ( :: ) of 'a * 'a list [@@deriving compare, equal, hash, sexp]

include Container.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val rev : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val append_list : 'a t -> 'a list -> 'a t
val ( @ ) : 'a t -> 'a t -> 'a t
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
val split_last : 'a t -> 'a list * 'a
val zip_strict : 'a t -> 'b t -> ('a * 'b) t List.Or_unequal_lengths.t
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val fold' : 'a t -> init:('a -> 'acc) -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val foldi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc

val fold_until
  :  'a t
  -> init:'acc
  -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_map_until
  :  'a t
  -> init:'acc
  -> f:('acc -> 'a -> ('acc * 'b, 'final) Fold_action.t)
  -> ('acc * 'b t, 'final * 'b list) Fold_action.t

module Fold2_result : sig
  type nonrec ('a, 'b) t =
    | Left_trailing of 'a t
    | Right_trailing of 'b t
    | Same_length
end

val zip : 'a t -> 'b t -> ('a * 'b) t * ('a, 'b) Fold2_result.t

val fold2
  :  'a t
  -> 'b t
  -> init:'acc
  -> f:('acc -> 'a -> 'b -> 'acc)
  -> 'acc * ('a, 'b) Fold2_result.t

val fold2_exn : 'a t -> 'b t -> init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'acc
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> ('a, 'b) Fold2_result.t
val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
val cartesian_product_all : 'a t t -> 'a t t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
