open Core_kernel
open Umber

let concat_current dir = Filename.(concat (concat current_dir_name dir))

(* These tests are currently for parsing only as their related type system features have
   not yet been implemented. *)
let parse_only_tests = [ "Imports"; "Modules"; "Operators"; "Traits"; "Types" ]
let should_type_check test = not (List.mem ~equal:String.equal parse_only_tests test)

(* These tests are just for type-checking as they are not ready to be converted to MIR.  *)
let type_only_tests = parse_only_tests @ []

(*let should_make_mir test = not (List.mem ~equal:String.equal type_only_tests test)*)
let should_make_mir _ = false (* TODO: fix *)

exception Compilation_error of Filename.t * Sexp.t [@@deriving sexp]

let run_tests () =
  let test filename =
    let bare_filename = Filename.chop_extension filename in
    let out_filename = bare_filename ^ ".out" in
    let print_tokens_to = Out_channel.create (concat_current "tokens" out_filename) in
    let print_ast_to = Out_channel.create (concat_current "ast" out_filename) in
    let print_mir_to = Out_channel.create (concat_current "mir" out_filename) in
    let in_file = concat_current "examples" filename in
    (try
       match Parsing.parse_file ~print_tokens_to ~full_lex:true in_file with
       | Ok ast ->
         if should_type_check bare_filename
         then (
           match Ast.Typed.Module.of_untyped ~backtrace:false ast with
           | Ok (names, ast) ->
             Ast.Typed.Module.sexp_of_t ast |> Parsing.fprint_s ~out:print_ast_to;
             if should_make_mir bare_filename
             then
               Mir.of_typed_module ~names ast
               |> [%sexp_of: Mir.Stmt.t list]
               |> Parsing.fprint_s ~out:print_mir_to
           | Error msg -> Ustring.print_endline ~out:print_ast_to msg)
         else Ast.Untyped.Module.sexp_of_t ast |> Parsing.fprint_s ~out:print_ast_to
       | Error msg -> Ustring.print_endline ~out:print_ast_to msg
     with
    | exn -> raise (Compilation_error (filename, Exn.sexp_of_t exn)));
    Out_channel.close print_tokens_to;
    Out_channel.close print_ast_to;
    Out_channel.close print_mir_to
  in
  Array.iter (Sys.readdir Filename.(concat current_dir_name "examples")) ~f:test
;;

let () = run_tests ()
