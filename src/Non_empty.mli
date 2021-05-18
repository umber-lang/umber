type 'a t = ( :: ) of 'a * 'a list [@@deriving sexp]

val cons : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t option
val to_list : 'a t -> 'a list
val rev : 'a t -> 'a t
