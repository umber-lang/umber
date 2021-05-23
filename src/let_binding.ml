open Import

type ('pat, 'expr) t =
  { rec_ : bool
  ; bindings : ('pat * 'expr) Nonempty.t
  ; body : 'expr
  }
[@@deriving compare, equal, hash, sexp]
