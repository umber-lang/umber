open Import

type ('pat, 'expr) t =
  { rec_ : bool
  ; bindings : ('pat * 'expr) Node.t Nonempty.t
  ; body : 'expr
  }
[@@deriving compare, equal, hash, sexp]
