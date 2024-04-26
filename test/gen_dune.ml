open! Core

(* TODO: Consider having each diff rule depend on the rules for earlier phases. 
   This would hopefully mean we don't see red diffs for later phases when earlier phases
   fail. *)

let test_dirs = [ "tokens"; "ast"; "mir"; "llvm"; "output" ]

let gen_rule () =
  let test_names =
    Util.sorted_files_in_local_dir "examples"
    |> Array.map ~f:Filename.chop_extension
    |> Array.to_list
  in
  let action : Sexp.t =
    List
      (Atom "progn"
       :: [%sexp "run", "%{dep:test.exe}"]
       :: List.concat_map test_dirs ~f:(fun dir ->
            List.map test_names ~f:(fun test_name ->
              [%sexp
                "diff?"
                , (dir ^/ test_name ^ ".expected" : string)
                , (dir ^/ test_name ^ ".out" : string)])))
  in
  let deps : Sexp.t =
    List
      (Atom "deps"
       :: [%sexp "package", "umber"]
       :: [%sexp "glob_files", "examples/*"]
       :: List.map test_dirs ~f:(fun dir ->
            [%sexp "glob_files", (dir ^/ "*.expected" : string)]))
  in
  [%sexp "rule", ("alias", "runtest"), (deps : Sexp.t), ("action", (action : Sexp.t))]
;;

let () = print_s (gen_rule ())
