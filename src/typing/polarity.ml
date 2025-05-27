open! Core

module T = struct
  type t =
    | Positive
    | Negative
  [@@deriving sexp, compare]

  let flip = function
    | Positive -> Negative
    | Negative -> Positive
  ;;
end

include T
include Comparable.Make (T)
