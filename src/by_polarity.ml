open! Core

type 'a t =
  { positive : 'a
  ; negative : 'a
  }
[@@deriving sexp]

let update t ~(polarity : Polarity.t) ~f =
  match polarity with
  | Positive -> { t with positive = f t.positive }
  | Negative -> { t with negative = f t.negative }
;;

let iter { positive; negative } ~f =
  f positive ~polarity:Polarity.Positive;
  f negative ~polarity:Polarity.Negative
;;
