open! Core
open! Import

type t =
  { name : Value_name.Absolute.t
  ; hash : int
  }
[@@deriving sexp_of]

let create ~effect_operation_name =
  { name = effect_operation_name; hash = Value_name.Absolute.hash effect_operation_name }
;;

let to_int t = t.hash
