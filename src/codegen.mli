open Import
open Names

type t

val of_mir
  :  module_path:Module_path.Absolute.t
  -> source_filename:Filename.t
  -> Mir.t
  -> (t, Compilation_error.t) result

val compile_to_object_and_dispose : t -> output_file:Filename.t -> unit

val compile_entry_module
  :  module_paths:Module_path.Absolute.t list
  -> entry_file:Filename.t
  -> unit

val to_string : t -> string
val print : t -> to_:Filename.t -> unit
