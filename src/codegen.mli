open Import

type t

val create : source_filename:Filename.t -> t
val add_mir : t -> Mir.t -> (unit, Compilation_error.t) result
val add_mir_exn : t -> Mir.t -> unit
val compile_to_object_and_dispose : t -> output_file:Filename.t -> unit

val compile_entry_module
  :  source_filenames:Filename.t list
  -> entry_file:Filename.t
  -> unit

val to_string : t -> string
val print : t -> to_:Filename.t -> unit
