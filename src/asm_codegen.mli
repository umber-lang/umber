open! Import
open Names

type t

val of_mir : module_path:Module_path.Absolute.t -> Mir.t -> t
val compile_to_object_file : t -> output_file:Filename.t -> unit

val compile_entry_module
  :  module_paths:Module_path.Absolute.t list
  -> entry_file:Filename.t
  -> unit

val pp : Format.formatter -> t -> unit
