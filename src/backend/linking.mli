open! Core
open! Import

val link_with_std_and_runtime
  :  object_files:Filename.t list
  -> output_exe:Filename.t
  -> (unit, Compilation_error.t) result
