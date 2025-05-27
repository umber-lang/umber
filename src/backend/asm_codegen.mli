open! Core
open! Import
open Names

val convert_mir : module_path:Module_path.Absolute.t -> Mir.t -> Asm_program.t
val compile_to_object_file : Asm_program.t -> output_file:Filename.t -> unit

val compile_entry_module
  :  module_paths:Module_path.Absolute.t list
  -> entry_file:Filename.t
  -> unit
