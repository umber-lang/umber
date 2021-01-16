open Import

type 'a t =
  { span : Span.t
  ; node : 'a
  }
[@@deriving equal, compare, hash]

let map t ~f = { t with node = f t.node }

let fold_map acc t ~f =
  let acc, node = f acc t.node in
  acc, { t with node }
;;

let sexp_of_t sexp_of_node { span; node } =
  Sexp.List [ Span.sexp_of_t span; sexp_of_node node ]
;;

let t_of_sexp node_of_sexp = function
  | Sexp.List [ span; node ] -> { span = Span.t_of_sexp span; node = node_of_sexp node }
  | _ -> raise_s [%message "Node.t_of_sexp: parse failed"]
;;
