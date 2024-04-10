open! Core

type 'a t =
  { positive : 'a
  ; negative : 'a
  }
[@@deriving sexp]

val update : 'a t -> polarity:Polarity.t -> f:('a -> 'a) -> 'a t
val iter : 'a t -> f:('a -> polarity:Polarity.t -> unit) -> unit
