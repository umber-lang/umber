open Core_kernel
open Umber

let concat_current dir = Filename.(concat (concat current_dir_name dir))

(* These tests are currently for parsing only as their related type system features have
   not yet been implemented. *)
let parse_only_tests = [ "Imports"; "Modules"; "Operators"; "Traits"; "Types" ]
let should_type_check test = not (List.mem ~equal:String.equal parse_only_tests test)

let run_tests () =
  let test filename =
    let bare_filename = Filename.chop_extension filename in
    let out_filename = bare_filename ^ ".out" in
    let print_tokens_to = Out_channel.create (concat_current "tokens" out_filename) in
    let print_ast_to = Out_channel.create (concat_current "ast" out_filename) in
    let in_file = concat_current "examples" filename in
    (match Parsing.parse_file ~print_tokens_to ~full_lex:true in_file with
    | Ok ast ->
      if should_type_check bare_filename
      then (
        match Ast.Typed.Module.of_untyped ~backtrace:false ast with
        | Ok (_names, ast) ->
          Ast.Typed.Module.sexp_of_t ast |> Parsing.fprint_s ~out:print_ast_to
        | Error msg -> Ustring.print_endline ~out:print_ast_to msg)
      else Ast.Untyped.Module.sexp_of_t ast |> Parsing.fprint_s ~out:print_ast_to
    | Error msg -> Ustring.print_endline ~out:print_ast_to msg);
    Out_channel.close print_tokens_to;
    Out_channel.close print_ast_to
  in
  Array.iter (Sys.readdir Filename.(concat current_dir_name "examples")) ~f:test
;;

let () = run_tests ()
