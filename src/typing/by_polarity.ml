open! Core

type 'a t =
  { positive : 'a
  ; negative : 'a
  }
[@@deriving sexp]

let init (f : Polarity.t -> _) = { positive = f Positive; negative = f Negative }

let get t ~(polarity : Polarity.t) =
  match polarity with
  | Positive -> t.positive
  | Negative -> t.negative
;;

let update t ~(polarity : Polarity.t) ~f =
  match polarity with
  | Positive -> { t with positive = f t.positive }
  | Negative -> { t with negative = f t.negative }
;;

let map { positive; negative } ~f = { positive = f positive; negative = f negative }

let iter { positive; negative } ~f =
  f positive ~polarity:Polarity.Positive;
  f negative ~polarity:Polarity.Negative
;;

let flip { positive; negative } = { negative = positive; positive = negative }
