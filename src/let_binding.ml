open Import

type ('pat, 'expr) t =
  { rec_ : bool
  ; pat : 'pat
  ; expr : 'expr
  ; body : 'expr
  }
[@@deriving sexp]
