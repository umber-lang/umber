open Import

type 'a t = ( :: ) of 'a * 'a list [@@deriving compare, equal, hash, sexp]

include Container.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val rev : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val append_list : 'a t -> 'a list -> 'a t
val ( @ ) : 'a t -> 'a t -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val fold_right : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
