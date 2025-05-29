open! Core
open! Import

module Make () = struct
  include Int

  module Counter = struct
    type t = int ref [@@deriving sexp_of]

    let create () = ref 0

    let next t =
      let id = !t in
      incr t;
      id
    ;;
  end
end
