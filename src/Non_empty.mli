open Import

type 'a t = ( :: ) of 'a * 'a list [@@deriving sexp]

include Container.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val cons : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val rev : 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val append_list : 'a t -> 'a list -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
val unzip : ('a * 'b) t -> 'a t * 'b t
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a