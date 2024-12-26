open Import

let prelude_object_file =
  lazy
    (let module_ = Llvm_helpers.parse_module (force Sites.prelude_llvm) in
     let output_file = Filename_unix.temp_file "umber_prelude" "" in
     Llvm_helpers.compile_module_to_object module_ ~output_file;
     at_exit (fun () -> Sys_unix.remove output_file);
     output_file)
;;

let link_with_std_and_runtime ~object_files ~output_exe =
  (* TODO: Look into dead code elimination. I tried passing -Wl,--gc-sections and the size
     reduced a little but it didn't seem to actually remove all the unused functions. *)
  (* Just shell out to clang because why not *)
  Compilation_error.try_with
    Other
    ~msg:[%sexp "Error encountered while linking"]
    (fun () ->
    Shell.run
      "clang"
      (object_files
       @ [ force prelude_object_file; force Sites.runtime_archive_file; "-o"; output_exe ]
      ))
;;
