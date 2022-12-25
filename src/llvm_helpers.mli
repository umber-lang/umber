open Import

val parse_module : string -> Llvm.llmodule
val compile_module_to_object : Llvm.llmodule -> output_file:Filename.t -> unit
