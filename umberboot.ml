open Core
open Umber

let command =
  Command.basic
    ~summary:"Umberboot is a compiler for Umber written in OCaml."
    (let%map_open.Command () = return ()
     and filename = anon ("filename" %: Filename.arg_type)
     and lex = flag "lex" no_arg ~doc:"Print lexer output (tokens)"
     and parse = flag "parse" no_arg ~doc:"Print parser output (untyped AST)"
     and type_ = flag "type" no_arg ~doc:"Print type-checker output (typed AST)"
     and name = flag "name" no_arg ~doc:"Print name-resolver output (name bindings)"
     and mir = flag "mir" no_arg ~doc:"Print mid-level IR (MIR)"
     and llvm = flag "llvm" no_arg ~doc:"Print LLVM IR"
     and no_std = flag "no-std" no_arg ~doc:"Don't include the standard library"
     and parent =
       flag
         "parent"
         (optional Ast.Module_name.arg_type_lenient)
         ~doc:"MODULE_NAME The name of the parent module"
     in
     fun () ->
       let or_raise = function
         | Ok x -> x
         | Error err -> raise_s [%sexp (err : Compilation_error.t)]
       in
       if parse || type_ || name || mir || llvm
       then (
         let print_tokens_to = Option.some_if lex stdout in
         let ast = or_raise (Parsing.parse_file ?print_tokens_to filename) in
         if parse then print_s [%sexp (ast : Ast.Untyped.Module.t)];
         if type_ || name || mir || llvm
         then (
           let names =
             if no_std then Name_bindings.core else Lazy.force Name_bindings.std_prelude
           in
           let names =
             match parent with
             | Some module_name -> Name_bindings.into_module names module_name ~place:`Def
             | None -> names
           in
           let names, ast = or_raise (Ast.Typed.Module.of_untyped ~names ast) in
           if type_ then print_s [%sexp (ast : Ast.Typed.Module.t)];
           if name then print_s [%sexp (names : Name_bindings.t)];
           if mir || llvm
           then (
             let mir_stmts = or_raise (Mir.of_typed_module ~names ast) in
             if mir then print_s [%sexp (mir_stmts : Mir.t)];
             if llvm
             then
               Codegen.of_mir ~source_filename:filename mir_stmts
               |> Codegen.to_string
               |> print_endline)))
       else if lex
       then
         Parsing.lex_file filename ~print_tokens_to:stdout
         |> Result.iter_error ~f:(fun error ->
              print_s [%sexp (error : Compilation_error.t)]))
;;

let () = Command.run command
