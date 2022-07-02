let lexeme lexbuf = Sedlexing.lexeme lexbuf |> Ustring.of_array_unchecked
let digit = [%sedlex.regexp? '0' .. '9']

let lex_lower_name lexbuf =
  match%sedlex lexbuf with
  | lowercase, Star (digit | alphabetic | '\'' | '_')
  | '_', Plus (digit | alphabetic | '\'' | '_') -> Some (lexeme lexbuf)
  | _ -> None
;;

let lex_upper_name lexbuf =
  match%sedlex lexbuf with
  | uppercase, Star (digit | alphabetic | '\'' | '_') -> Some (lexeme lexbuf)
  | _ -> None
;;

let lex_eof lexbuf =
  match%sedlex lexbuf with
  | eof -> true
  | _ -> false
;;

let lex_place lexbuf =
  match%sedlex lexbuf with
  | "(s)" -> Some `Sig
  | "(d)" -> Some `Def
  | _ -> None
;;
