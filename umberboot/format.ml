open Core
open Umber

let command =
  Command.basic
    ~summary:"Format an Umber file, outputting to stdout"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
     and debug =
       flag
         "debug"
         no_arg
         ~doc:
           "For debugging, output the `Auto_format.Document.t` instead of the formatted \
            code"
     in
     fun () ->
       match Parsing.parse_file filename with
       | Ok ast ->
         if debug
         then
           print_s
             [%sexp
               (Pretty_ast.format_to_document ast : Pretty_ast.Auto_format.Document.t)]
         else (
           Pretty_ast.format ast |> Sequence.iter ~f:print_string;
           Out_channel.newline stdout)
       | Error error -> raise (Compilation_error.Compilation_error error))
;;
