open! Core
open! Names

type ('typ, 'name) t =
  { operation : 'name Value_name.Qualified.t
  ; args : ('typ, 'name) Pattern.t Nonempty.t
  }
[@@deriving equal, sexp]
