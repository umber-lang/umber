open! Core
open! Import

type t =
  { name : Mir_name.t
  ; extern_name : Extern_name.t
  ; arity : int
  }
[@@deriving sexp_of]
