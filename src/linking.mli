open Import

val link_with_std_and_runtime
  :  object_file:Filename.t
  -> output_exe:Filename.t
  -> (unit, Compilation_error.t) result
