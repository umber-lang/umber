open! Import

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving equal, compare, hash]

let create value span = { value; span }
let span t = t.span

let raise_error (error : Compilation_error.t) span =
  raise_notrace
    (Compilation_error.Compilation_error
       { error with
         span =
           (match error.span with
            | Some _ -> error.span
            | None -> Some span)
       ; backtrace = Some (Backtrace.Exn.most_recent ())
       })
;;

let with_value { value; span } ~f =
  try f value with
  | Compilation_error.Compilation_error error -> raise_error error span
;;

let with_value2 t t' ~f =
  try f t.value t'.value with
  | Compilation_error.Compilation_error error ->
    raise_error error (Span.combine t.span t'.span)
;;

let map t ~f = with_value t ~f:(fun value -> { t with value = f value })

let fold_map acc t ~f =
  with_value t ~f:(fun value ->
    let acc, value = f acc value in
    acc, { t with value })
;;

let set t value = { t with value }
let dummy_span value = { value; span = Span.dummy }
let sexp_of_t sexp_of_value { value; _ } = sexp_of_value value
let t_of_sexp node_of_sexp = dummy_span << node_of_sexp
