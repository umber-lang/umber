open! Import

module T = struct
  type 'a t = 'a option * Compilation_error.t list

  let return x = Some x, []

  let bind t ~f =
    match t with
    | (None, _) as t -> t
    | Some state, errors ->
      let state', errors' = f state in
      state', errors @ errors'
  ;;

  let map = `Custom (fun t ~f -> Tuple2.map_fst t ~f:(Option.map ~f))
end

include T
include Monad.Make (T)

let fatal_error error = None, [ error ]
