type 'a t = private 'a list

val cons : 'a -> 'a list -> 'a t
val of_list : 'a list -> 'a t option
val of_list_exn : 'a list -> 'a t
val to_list : 'a t -> 'a list
