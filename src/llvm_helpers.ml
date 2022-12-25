open Import

let parse_module module_string =
  Llvm_irreader.parse_ir (Llvm.create_context ()) (Llvm.MemoryBuffer.of_string module_string)
;;

let target_machine =
  lazy
    (Llvm_all_backends.initialize ();
     let triple = Llvm_target.Target.default_triple () in
     let target = Llvm_target.Target.by_triple triple in
     Llvm_target.TargetMachine.create ~triple target)
;;

let compile_module_to_object module_ ~output_file =
  Llvm_target.TargetMachine.emit_to_file
    module_
    ObjectFile
    output_file
    (force target_machine)
;;