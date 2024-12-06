open Import

let prelude_object_file =
  lazy
    (let module_ = Llvm_helpers.parse_module (force Sites.prelude_llvm) in
     let output_file = Filename_unix.temp_file "umber_prelude" "" in
     Llvm_helpers.compile_module_to_object module_ ~output_file;
     output_file)
;;

(** The Rust standard library requires these dependencies. Determined by running
    `cargo rustc -- --print native-static-libs` *)
let rust_std_dependencies =
  [ "-lc"
  ; "-lm"
  ; "-lrt"
  ; "-lpthread"
  ; "-lgcc_s"
  ; "-lutil"
  ; "-lrt"
  ; "-lpthread"
  ; "-lm"
  ; "-ldl"
  ; "-lc"
  ]
;;

let link_with_std_and_runtime ~object_files ~output_exe =
  (* Just shell out to clang because why not *)
  Compilation_error.try_with
    Other
    ~msg:[%sexp "Error encountered while linking"]
    (fun () ->
    Shell.run
      "clang"
      (object_files
       @ rust_std_dependencies
       @ [ force prelude_object_file; force Sites.runtime_archive_file; "-o"; output_exe ]
      ))
;;
