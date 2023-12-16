open Import

module U = struct
  module T = struct
    type t = int [@@deriving compare, equal, hash]

    let of_string = String.chop_prefix_exn ~prefix:"$" >> Int.of_string
    let to_string = Int.to_string >> ( ^ ) "$"
  end

  include T
  include Sexpable.Of_stringable (T)
end

include U
include Comparable.Make (U)
include Hashable.Make (U)

let next = ref 0

let create () =
  let current = !next in
  incr next;
  current
;;

let of_int = Fn.id

module For_testing = struct
  let reset_ids () = next := 0
end
