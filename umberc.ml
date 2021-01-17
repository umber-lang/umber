open Core
open Umber

let run file debug lex_only print_types print_names no_std parent backtrace () =
  let open Result.Let_syntax in
  if lex_only
  then
    Parsing.lex_file file ~print_tokens_to:stdout
    |> Result.iter_error ~f:Ustring.print_endline
  else (
    let result =
      let print_tokens_to = Option.some_if debug stdout in
      let%bind ast = Parsing.parse_file ?print_tokens_to file in
      if print_types || print_names
      then (
        let names =
          if no_std then Name_bindings.core else Lazy.force Name_bindings.std_prelude
        in
        let names =
          match parent with
          | Some str ->
            Name_bindings.into_module names (Ast.Module_name.of_string_lenient_exn str)
          | None -> names
        in
        let%map names, ast = Ast.Typed.Module.of_untyped ?backtrace ~names ast in
        if print_types && print_names
        then [%sexp_of: Name_bindings.t * Ast.Typed.Module.t] (names, ast)
        else if print_types
        then Ast.Typed.Module.sexp_of_t ast
        else Name_bindings.sexp_of_t names)
      else Ok (Ast.Untyped.Module.sexp_of_t ast)
    in
    match result with
    | Ok sexp -> print_s sexp
    | Error msg -> Ustring.print_endline msg)
;;

let () =
  Command.basic_spec
    ~summary:"Parse this language"
    Command.Spec.(
      empty
      +> anon ("filename" %: string)
      +> flag "debug" Command.Flag.no_arg ~doc:"Print lexer debug output"
      +> flag "lex-only" Command.Flag.no_arg ~doc:"Skip parsing and just lex the input"
      +> flag "type" Command.Flag.no_arg ~doc:"Print the typed AST"
      +> flag "names" Command.Flag.no_arg ~doc:"Print the name bindings"
      +> flag "no-std" Command.Flag.no_arg ~doc:"Don't include the standard library"
      +> flag "parent" (Command.Flag.optional string) ~doc:"The name of the parent module"
      +> flag
           "backtrace"
           (Command.Flag.optional bool)
           ~doc:"Whether to include backtraces for errors")
    run
  |> Command.run
;;
