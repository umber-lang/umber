open Import
open Sedlexing
open Parser

let lexeme = lexeme >> Ustring.of_array_unchecked
let lexeme_str = lexeme >> Ustring.to_string

exception Syntax_error of Span.Pos.t * Ustring.t [@@deriving sexp]

let span = Span.of_loc << lexing_positions

let syntax_error ?msg lexbuf =
  let msg =
    match msg with
    | None -> lexeme lexbuf
    | Some str -> Ustring.of_string_exn (sprintf "%s after `%s`" str (lexeme_str lexbuf))
  in
  raise (Syntax_error ((span lexbuf).start, msg))
;;

let digit = [%sedlex.regexp? '0' .. '9']
let frac = [%sedlex.regexp? '.', Star digit]
let exp = [%sedlex.regexp? ('e' | 'E'), Opt ('-' | '+'), Plus digit]

let operator_symbol =
  [%sedlex.regexp?
    Sub ((pc | pd | pe | pf | pi | po | ps | sc | sk | sm | so), Chars "'\"()[]{},#")]
;;

let line_comment = [%sedlex.regexp? '#', Star (Compl (Chars "\r\n"))]
let unescape _ = failwith "Escapes not implemented yet"

let read_string lexbuf =
  let read_escape lexbuf =
    match%sedlex lexbuf with
    | '\\' | '"' | '\'' -> lexeme_char lexbuf 0
    | 'n' -> Uchar.of_char '\n'
    | 'r' -> Uchar.of_char '\r'
    | 't' -> Uchar.of_char '\t'
    | 'b' -> Uchar.of_char '\b'
    | Rep (digit, 3) ->
      (* Decimal ASCII escape *)
      lexeme lexbuf
      |> Ustring.to_string
      |> Int.of_string
      |> Char.of_int_exn
      |> Uchar.of_char
    (* TODO: hex escapes, octal, unicode! *)
    | _ -> syntax_error lexbuf
  in
  let rec loop lexbuf buf =
    let add c =
      Queue.enqueue buf c;
      loop lexbuf buf
    in
    match%sedlex lexbuf with
    (* TODO: add escaping + string interpolation *)
    | '"' -> Queue.to_array buf |> Ustring.of_array_unchecked
    | "\\" -> add (read_escape lexbuf)
    | any -> add (lexeme_char lexbuf 0)
    | _ -> syntax_error ~msg:"String literal ended unexpectedly" lexbuf
  in
  loop lexbuf (Queue.create ())
;;

let rec read lexbuf =
  match%sedlex lexbuf with
  (* Ignore comments and whitespace. *)
  | line_comment -> read lexbuf
  | Plus white_space -> read lexbuf
  (* TODO: Do int/float conversions manually and support underscores
     Even further: abstract the functionality behind a trait e.g. Num?
     Look into how Haskell does it (though might not want to copy, Num is
     notoriously overloaded with functionaltiy) *)
  (* Int *)
  | Opt '-', Plus digit -> INT (Int.of_string (Ustring.to_string (lexeme lexbuf)))
  (* Float *)
  | Opt '-', Plus digit, (exp | frac) ->
    FLOAT (Float.of_string (Ustring.to_string (lexeme lexbuf)))
  (* Char *)
  | '\'', Compl (Chars "'\n\r"), '\'' -> CHAR (lexeme_char lexbuf 1)
  | "'\\''" -> CHAR (Uchar.of_char '\'')
  | "'\\", Plus (Compl (Chars "'\n\r")), '\'' ->
    CHAR (unescape (sub_lexeme lexbuf 2 (lexeme_length lexbuf)))
  (* String literals *)
  | '"' -> STRING (read_string lexbuf)
  (* Keywords *)
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "let'" -> LET_NONREC
  | "let" -> LET
  | "and" -> AND
  | "in" -> IN
  | "match" -> MATCH
  | "with" -> WITH
  | "without" -> WITHOUT
  | "as" -> AS
  | "type" -> TYPE
  | "val" -> VAL
  | "extern" -> EXTERN
  | "infix" -> INFIX
  | "infixl" -> INFIXL
  | "infixr" -> INFIXR
  | "module", Star white_space, ":" -> FILE_MODULE
  | "module" -> MODULE
  | "trait" -> TRAIT
  | "impl" -> IMPL
  | "import" -> IMPORT
  | "effect" -> EFFECT
  (* Special symbols *)
  | '=' -> EQUALS
  | '|' -> PIPE
  | ':', Plus white_space -> COLON_SPACED
  | ':' -> COLON
  | ',' -> COMMA
  | '\\' -> BACKSLASH
  | '*' -> ASTERISK
  (* Need to support: `f . g`, `(. f)`, `(f .)`, and `(.)` as an operator *)
  | '.', Plus white_space -> OPERATOR (Ustring.of_string_exn ".")
  | ".)" ->
    rollback lexbuf;
    ignore (next lexbuf : Uchar.t option);
    OPERATOR (Ustring.of_string_exn ".")
  | '.' -> PERIOD
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  (* Brackets *)
  | '(' -> L_PAREN
  | ')' -> R_PAREN
  | '[' -> L_BRACKET
  | ']' -> R_BRACKET
  | '{' -> L_BRACE
  | '}' -> R_BRACE
  (* Names *)
  | lowercase, Star (digit | alphabetic | '\'' | '_') -> LOWER_NAME (lexeme lexbuf)
  | '_', Plus (digit | alphabetic | '\'' | '_') -> LOWER_NAME (lexeme lexbuf)
  | '_' -> UNDERSCORE
  | uppercase, Star (digit | alphabetic | '\'' | '_') -> UPPER_NAME (lexeme lexbuf)
  (* Symbolic Operators (any unicode punctuation/symbol) *)
  | Plus operator_symbol -> OPERATOR (lexeme lexbuf)
  (* Catch-all *)
  | eof -> EOF
  | _ -> syntax_error ~msg:"Invalid character(s)" lexbuf
;;
