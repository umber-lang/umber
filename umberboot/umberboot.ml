open Core
include Compile

let command =
  Command.group
    ~summary:"Umberboot is a compiler for Umber written in OCaml."
    [ "compile", Compile.command; "format", Format.command ]
;;
