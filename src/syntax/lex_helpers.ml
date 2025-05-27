open! Core
open! Import

let lexeme lexbuf = Sedlexing.lexeme lexbuf |> Ustring.of_array_unchecked
let digit = [%sedlex.regexp? '0' .. '9']

let lex_lower_name lexbuf =
  match%sedlex lexbuf with
  | lowercase, Star (digit | alphabetic | '\'' | '_')
  | '_', Plus (digit | alphabetic | '\'' | '_') -> Ok (lexeme lexbuf)
  | _ -> error_s [%message "Invalid lower name" ~_:(lexeme lexbuf : Ustring.t)]
;;

let lex_upper_name lexbuf =
  match%sedlex lexbuf with
  | uppercase, Star (digit | alphabetic | '\'' | '_') -> Ok (lexeme lexbuf)
  | _ -> error_s [%message "Invalid upper name" ~_:(lexeme lexbuf : Ustring.t)]
;;

let lex_place lexbuf =
  match%sedlex lexbuf with
  | "(s)" -> Some `Sig
  | "(d)" -> Some `Def
  | _ -> None
;;
