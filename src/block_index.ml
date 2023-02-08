open! Import

type t = int [@@deriving equal, sexp]

let of_int = Fn.id
let to_int = Fn.id
