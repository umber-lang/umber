open! Import

type 'a t =
  { span : Span.t
  ; value : 'a
  }
[@@deriving equal, compare, hash]

let create value span = { value; span }
let span t = t.span

let with_value { value; span } ~f =
  try f value with
  | Compilation_error.Compilation_error error ->
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
let quickcheck_generator generator = Quickcheck.Generator.map generator ~f:dummy_span

let quickcheck_shrinker shrinker =
  Quickcheck.Shrinker.map shrinker ~f:dummy_span ~f_inverse:(fun t -> t.value)
;;

let quickcheck_observer observer =
  Quickcheck.Observer.unmap observer ~f:(fun t -> t.value)
;;
