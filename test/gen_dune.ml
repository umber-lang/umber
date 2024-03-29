open! Core

(* TODO: Consider having each diff rule depend on the rules for earlier phases. 
   This would hopefully mean we don't see red diffs for later phases when earlier phases
   fail. *)

let make_rule dir name : Sexp.t =
  [%sexp
    "rule"
    , ("alias", "runtest")
    , ("deps", "test.dummy")
    , ( "action"
      , ( "no-infer"
        , ("diff", (dir ^/ name ^ ".expected" : string), (dir ^/ name ^ ".out" : string))
        ) )]
;;

let handle_dir dir bare_filename =
  make_rule dir bare_filename |> print_s;
  Out_channel.newline stdout
;;

let () =
  Array.iter (Util.sorted_files_in_local_dir "examples") ~f:(fun filename ->
    let bare_filename = Filename.chop_extension filename in
    handle_dir "tokens" bare_filename;
    handle_dir "ast" bare_filename;
    handle_dir "mir" bare_filename;
    handle_dir "llvm" bare_filename;
    handle_dir "output" bare_filename)
;;
