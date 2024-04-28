open! Core

(* TODO: Consider having each diff rule depend on the rules for earlier phases. 
   This would hopefully mean we don't see red diffs for later phases when earlier phases
   fail. *)

let test_dirs = [ "tokens"; "ast"; "mir"; "llvm"; "output" ]

(* FIXME: Will stop at the first error instead of propagating them, bad. Maybe try
   "concurrent" or generate separate rules. *)

let gen_rule ~test_file =
  let test_name = Filename.chop_extension test_file in
  let action : Sexp.t =
    List
      [ Atom "progn"
      ; [%sexp
          "run", "%{dep:test.exe}", ("%{dep:" ^ "examples" ^/ test_file ^ "}" : string)]
      ; List
          (Atom "concurrent"
           :: List.map test_dirs ~f:(fun dir ->
                [%sexp
                  "diff?"
                  , (dir ^/ test_name ^ ".expected" : string)
                  , (dir ^/ test_name ^ ".out" : string)]))
      ]
  in
  [%sexp
    "rule"
    , ("alias", "runtest")
    , ("deps", ("package", "umber"))
    , ("action", (action : Sexp.t))]
;;

let () =
  let test_files = Util.sorted_files_in_local_dir "examples" in
  Array.iter test_files ~f:(fun test_file -> print_s (gen_rule ~test_file))
;;
