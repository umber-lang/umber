open Import

let look_up_in_site dirs ~file =
  List.find_map_exn dirs ~f:(fun dir ->
    let file = dir ^/ file in
    if Sys_unix.file_exists_exn file then Some file else None)
;;

let runtime_archive = lazy (look_up_in_site Sources.Sites.runtime ~file:"libruntime.a")

let link_with_runtime ~object_file ~output_exe =
  (* Just shell out to clang because why not *)
  Compilation_error.try_with
    Other
    ~msg:[%sexp "Error encountered while linking"]
    (fun () -> Shell.run "clang" [ object_file; force runtime_archive; "-o"; output_exe ])
;;
