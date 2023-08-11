open Import

module T = struct
  include Unique_id.Int ()

  let of_string = String.chop_prefix_exn ~prefix:"$" >> of_string
  let to_string = to_string >> ( ^ ) "$"
end

include T
include Sexpable.Of_stringable (T)
