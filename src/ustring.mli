open Import

type t [@@deriving compare, equal, hash, sexp]
type elt = Uchar.t

include Container.S0 with type t := t with type elt := elt
include Comparable.S with type t := t
include Hashable.S with type t := t

val empty : t
val ( ^ ) : t -> t -> t
val get : t -> int -> elt
val index : t -> elt -> int option
val index_exn : t -> elt -> int
val sub : (t, t) Blit.sub
val subo : (t, t) Blit.subo
val of_array_unsafe : Uchar.t array -> t
val of_string_exn : string -> t
val of_ustring : t -> t
val to_string : t -> string
val to_ustring : t -> t
val add_to_buffer : Buffer.t -> t -> unit
val add_substring_to_buffer : Buffer.t -> t -> pos:int -> len:int -> unit
val print : ?out:Out_channel.t -> t -> unit
val print_endline : ?out:Out_channel.t -> t -> unit
