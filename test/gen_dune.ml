open Core_kernel

let make_rule dir name : Sexp.t =
  List
    [ Atom "rule"
    ; List [ Atom "alias"; Atom "runtest" ]
    ; List [ Atom "deps"; Atom "test.dummy" ]
    ; List
        [ Atom "action"
        ; List
            [ Atom "no-infer"
            ; List
                [ Atom "diff"
                ; Atom (Filename.concat dir (name ^ ".expected"))
                ; Atom (Filename.concat dir (name ^ ".out"))
                ]
            ]
        ]
    ]
;;

let handle_dir dir bare_filename =
  make_rule dir bare_filename |> print_s;
  Out_channel.newline stdout
;;

let () =
  Array.iter
    (Sys.readdir Filename.(concat current_dir_name "examples"))
    ~f:(fun filename ->
      let bare_filename = Filename.chop_extension filename in
      handle_dir "tokens" bare_filename;
      handle_dir "ast" bare_filename;
      handle_dir "mir" bare_filename
      (* FIXME: re-enable llvm tests once mir tested are unborked *)
      (*handle_dir "llvm" bare_filename*))
;;
