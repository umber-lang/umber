type 'a t =
  { node : 'a
  ; span : Span.t
  }
[@@deriving equal, compare, hash, sexp]

let map t ~f = { t with node = f t.node }

let fold_map t ~f =
  let x, node = f t.node in
  x, { t with node }
;;
