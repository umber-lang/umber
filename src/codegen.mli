open Import

type t

val of_mir : source_filename:Filename.t -> Mir.t -> t
val to_string : t -> string
val print : t -> to_:Filename.t -> unit
