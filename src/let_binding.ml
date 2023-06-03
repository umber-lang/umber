open Import

type ('pat, 'expr) t =
  { rec_ : bool
  ; bindings : ('pat Node.t * 'expr Node.t) Nonempty.t
  ; body : 'expr Node.t
  }
[@@deriving compare, equal, hash, sexp]
