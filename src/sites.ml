open! Import

let look_up_in_site dirs ~file =
  match
    List.find_map dirs ~f:(fun dir ->
      let file = dir ^/ file in
      if Sys_unix.file_exists_exn file then Some file else None)
  with
  | Some file -> file
  | None ->
    compiler_bug
      [%message "Failed to find file in site" (file : string) (dirs : string list)]
;;

let runtime_archive_file =
  lazy (look_up_in_site Sources.Sites.runtime ~file:"libruntime.a")
;;

let prelude_names =
  lazy (look_up_in_site Sources.Sites.stdlib ~file:"prelude_names.sexp" |> Sexp.load_sexp)
;;

let prelude_asm_file = lazy (look_up_in_site Sources.Sites.stdlib ~file:"prelude_asm.asm")
