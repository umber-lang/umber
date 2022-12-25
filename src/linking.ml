open Import

let look_up_in_site dirs ~file =
  List.find_map_exn dirs ~f:(fun dir ->
    let file = dir ^/ file in
    if Sys_unix.file_exists_exn file then Some file else None)
;;

let runtime_archive = lazy (look_up_in_site Sources.Sites.runtime ~file:"libruntime.a")

let prelude_object_file =
  lazy
    (let module_ = Llvm_helpers.parse_module Umber_std.Prelude.llvm in
     let output_file = Filename_unix.temp_file "umber_prelude" "" in
     Llvm_helpers.compile_module_to_object module_ ~output_file;
     output_file)
;;

let link_with_std_and_runtime ~object_file ~output_exe =
  (* Just shell out to clang because why not *)
  Compilation_error.try_with
    Other
    ~msg:[%sexp "Error encountered while linking"]
    (fun () ->
    Shell.run
      "clang"
      [ object_file; force prelude_object_file; force runtime_archive; "-o"; output_exe ])
;;
