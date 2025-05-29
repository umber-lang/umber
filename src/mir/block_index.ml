open! Core
open! Import

type t = int [@@deriving sexp]

let of_int = Fn.id
let to_int = Fn.id
