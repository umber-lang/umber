open Import

val link_with_runtime
  :  object_file:Filename.t
  -> output_exe:Filename.t
  -> (unit, Compilation_error.t) result
