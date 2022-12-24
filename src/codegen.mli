open Import

type t

val of_mir : source_filename:Filename.t -> Mir.t -> (t, Compilation_error.t) result
val of_mir_exn : source_filename:Filename.t -> Mir.t -> t
val compile_to_object : t -> output_file:Filename.t -> unit
val to_string : t -> string
val print : t -> to_:Filename.t -> unit
