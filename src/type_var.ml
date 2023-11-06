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
module Id = Unique_id.Int ()

let create = Id.create >> Id.to_int_exn
