open Import

type ('pat, 'expr) t =
  { rec_ : bool
  ; bindings : ('pat * 'expr) list
  ; body : 'expr
  }
[@@deriving equal, compare, hash, sexp]
