open Import

type 'a t =
  { span : Span.t
  ; node : 'a
  }
[@@deriving equal, compare, hash]

let dummy_span node = { node; span = Span.dummy }
let map t ~f = { t with node = f t.node }

let fold_map acc t ~f =
  let acc, node = f acc t.node in
  acc, { t with node }
;;

let sexp_of_t sexp_of_node { node; _ } = sexp_of_node node
let t_of_sexp node_of_sexp = dummy_span << node_of_sexp
