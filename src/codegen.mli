open Import

type t

val of_mir : source_filename:Filename.t -> Mir.t -> (t, Compilation_error.t) result
val of_mir_exn : source_filename:Filename.t -> Mir.t -> t
val compile_to_object_and_dispose : t -> output_file:Filename.t -> unit

val compile_entry_module
  :  source_filenames:Filename.t list
  -> entry_file:Filename.t
  -> unit

val to_string : t -> string
val print : t -> to_:Filename.t -> unit
