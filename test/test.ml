open Core_kernel
open Umber

let concat_current dir = Filename.(concat (concat current_dir_name dir))

(* These tests are currently for parsing only as their related type system features have
   not yet been implemented. *)
let parse_only_tests = [ "Imports"; "Modules"; "Operators"; "Traits"; "Types" ]
let should_type_check test = not (List.mem ~equal:String.equal parse_only_tests test)

(* These tests are just for type-checking as they are not ready to be converted to MIR. *)
(* TODO: enable these for mir *)
let type_only_tests =
  [ "LetBindingGroups" (* let rec *)
  ; "LetPattern" (* unions in toplevel let bindings *)
  ; "MutualRecursion" (* let rec *)
  ]
;;

let should_make_mir test =
  should_type_check test && not (List.mem ~equal:String.equal type_only_tests test)
;;

(* TODO: expand this to more files *)
let no_llvm_tests = [ "AsPattern" (* closures *) ]

let should_make_llvm test =
  should_make_mir test && not (List.mem ~equal:String.equal no_llvm_tests test)
;;

let print_compilation_error ~out ~filename (error : Compilation_error.t) =
  let exn =
    match error.kind with
    | Other -> error.exn
    | _ -> None
  in
  Parsing.fprint_s
    ~out
    [%sexp
      "Compilation error"
      , ({ error with filename = Some filename; exn } : Compilation_error.t)]
;;

let run_tests () =
  let test filename =
    let bare_filename = Filename.chop_extension filename in
    let out_filename = bare_filename ^ ".out" in
    let print_tokens_to = Out_channel.create (concat_current "tokens" out_filename) in
    let print_ast_to = Out_channel.create (concat_current "ast" out_filename) in
    let print_mir_to = Out_channel.create (concat_current "mir" out_filename) in
    let print_llvm_to = Out_channel.create (concat_current "llvm" out_filename) in
    let in_file = concat_current "examples" filename in
    (match Parsing.parse_file ~print_tokens_to ~full_lex:true in_file with
    | Ok ast ->
      if should_type_check bare_filename
      then (
        match Ast.Typed.Module.of_untyped ast with
        | Ok (names, ast) ->
          Parsing.fprint_s ~out:print_ast_to [%sexp (ast : Ast.Typed.Module.t)];
          if should_make_mir bare_filename
          then (
            match Mir.of_typed_module ~names ast with
            | Ok (templates, mir) ->
              let mir = Mir.renumber_ids mir in
              let templates = Mir.Function_factory.Templates.renumber_ids templates in
              Parsing.fprint_s
                ~out:print_mir_to
                [%sexp (mir : Mir.t), (templates : Mir.Function_factory.Templates.t)];
              if should_make_llvm bare_filename
              then
                Out_channel.output_string
                  print_llvm_to
                  (try
                     Codegen.of_mir ~source_filename:filename mir |> Codegen.to_string
                   with
                  | exn -> Exn.to_string exn)
            | Error error -> print_compilation_error ~out:print_mir_to ~filename error)
        | Error error -> print_compilation_error ~out:print_ast_to ~filename error)
      else Parsing.fprint_s ~out:print_ast_to [%sexp (ast : Ast.Untyped.Module.t)]
    | Error error -> print_compilation_error ~out:print_ast_to ~filename error);
    Out_channel.close print_tokens_to;
    Out_channel.close print_ast_to;
    Out_channel.close print_mir_to;
    Out_channel.close print_llvm_to
  in
  Array.iter (Sys.readdir Filename.(concat current_dir_name "examples")) ~f:test
;;

let () = run_tests ()
