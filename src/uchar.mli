open Core_kernel
include module type of Uchar

exception Malformed

val add_to_buffer : Buffer.t -> t -> unit
val of_gen_exn : char Gen.t -> t option
val to_string : t -> string
val of_string_exn : string -> t
val to_int : t -> int
