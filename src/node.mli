open! Import

type 'a t [@@deriving compare, sexp, equal, hash]

val create : 'a -> Span.t -> 'a t
val span : _ t -> Span.t
val dummy_span : 'a -> 'a t
val with_value : 'a t -> f:('a -> 'b) -> 'b
val with_value2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c
val map : 'a t -> f:('a -> 'b) -> 'b t
val fold_map : 'acc -> 'a t -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val split : ('a * 'b) t -> 'a t * 'b t
val set : 'a t -> 'b -> 'b t
