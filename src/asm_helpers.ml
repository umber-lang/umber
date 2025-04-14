open! Import

(* TODO: Maybe turn on -wall -werror to get all the warnings as errors. I don't want it to
   silently do wrong things. *)
let compile_to_object_file ~input_file ~output_file =
  Shell.run "nasm" [ "-felf64"; input_file; "-o"; output_file ]
;;
