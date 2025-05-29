open! Core
open! Import

type t =
  { name : Mir_name.t
  ; arity : int
  }
[@@deriving sexp_of]
