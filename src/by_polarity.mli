open! Core

type 'a t =
  { positive : 'a
  ; negative : 'a
  }
[@@deriving sexp]

val init : (Polarity.t -> 'a) -> 'a t
val get : 'a t -> polarity:Polarity.t -> 'a
val update : 'a t -> polarity:Polarity.t -> f:('a -> 'a) -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> polarity:Polarity.t -> unit) -> unit
val flip : 'a t -> 'a t
