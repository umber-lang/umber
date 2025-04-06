open! Import

let compile_to_object_file ~input_file ~output_file =
  Shell.run "nasm" [ "-felf64"; input_file; "-o"; output_file ]
;;
