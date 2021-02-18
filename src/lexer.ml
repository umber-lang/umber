open Import
open Sedlexing
open Parser

(* TODO: consider making this lexer deal in a layout-insensitive variant of the language
   and adding another component to deal with converting the input whitespace into 
   a syntax, possibly using braces {} and semicolons ; (although they may get overloaded
   since both are already part of my syntax)

   Can use :{ and }: if braces don't work. Semicolons should work fine as just line separators
   (just allow expressions to be sequences of operations,
    and add a special case for (;) - `val (;) (infixl 1) : () -> a -> a`)

   Problem: I think I might just end up re-encoding the grammer in this component -- 
   since to get anything interesting done it would have to simplify expressions like
   `
   { 
     a : Int,
     b : Float 
    }
   `
   to `{ a : Int, b : Float }` and not `{ :{ a : Int, b : Float }: }`
   
   Another option: treat certain tokens e.g. = and : as special block-starters and ... (???)
   - ^ maybe this could somehow make the "no identation in brackets" idea work (?)
   - Wait! What if (= or : or ->) followed by an indent switches the indentation back on?
     Want to support both let ... = match and let ... = (shouldn't have to be the end of the line)
     ^ then COMMA switches it off again?

   Want to start not ignoring the exact pattern of -> ... INDENT
   
   e.g.
   `
   let _ = (
     1, 2)
   `
   should lex as (LET (LOWER_NAME _) EQUALS L_PAREN ... ???) (where am I going with this?)
 *)

let lexeme = lexeme >> Ustring.of_array_unsafe
let lexeme_str = lexeme >> Ustring.to_string

type indent_type =
  | Tabs
  | Spaces
  | Unknown

module Bracket_type = struct
  type t =
    | Paren
    | Bracket
    | Brace

  let of_char_exn = function
    | '(' | ')' -> Paren
    | '[' | ']' -> Bracket
    | '{' | '}' -> Brace
    | c -> raise_s [%message "Can't convert to bracket type" (c : char)]
  ;;

  let to_char_right = function
    | Paren -> ')'
    | Bracket -> ']'
    | Brace -> '}'
  ;;

  let to_token_left = function
    | Paren -> L_PAREN
    | Bracket -> L_BRACKET
    | Brace -> L_BRACE
  ;;

  let to_token_right = function
    | Paren -> R_PAREN
    | Bracket -> R_BRACKET
    | Brace -> R_BRACE
  ;;
end

type lex_state =
  | Start_of_file
  | Before_indent
  | After_indent
  | Inline
  | End_of_file
[@@deriving sexp]

type t =
  { mutable lex_state : lex_state
  ; indent_sizes : int Stack.t
  ; mutable indent_type : indent_type
  ; mutable buffered_dedents : int
  ; mutable last_token : token
  ; brackets : Bracket_type.t Stack.t
  ; mutable ignoring_indents : bool
      (* TODO: Maybe store the block_starter's indentation level? the bracket level? *)
  }

let create () =
  { lex_state = Start_of_file
  ; indent_sizes = Stack.singleton 0
  ; indent_type = Unknown
  ; buffered_dedents = 0
  ; last_token = EOF
  ; brackets = Stack.create ()
  ; ignoring_indents = false
  }
;;

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

let line_sep = [%sedlex.regexp? '\n' | "\r\n"]
let inline_space = [%sedlex.regexp? Sub (white_space, Chars "\r\n")]
let digit = [%sedlex.regexp? '0' .. '9']
let frac = [%sedlex.regexp? '.', Star digit]
let exp = [%sedlex.regexp? ('e' | 'E'), Opt ('-' | '+'), Plus digit]

let operator_symbol =
  [%sedlex.regexp?
    Sub ((pc | pd | pe | pf | pi | po | ps | sc | sk | sm | so), Chars "'\"()[]{},#")]
;;

(* TODO: stray '\r's may cause issues with indentation parsing
   '\r' should just always be ignored, I think *)
let line_comment = [%sedlex.regexp? '#', Star (Compl (Chars "\r\n")), Plus line_sep]
let unescape _ = failwith "Escapes not implemented yet"

(*let prefix_matches lexbuf s =
  mark lexbuf 0;
  let rec loop lexbuf s i =
    if i >= Ustring.length s then true
    else match next lexbuf with
      | Some c ->
        if Uchar.equal c (Ustring.get s i) then loop lexbuf s (i + 1)
        else (
          ignore (backtrack lexbuf : int);
          false)
      | None -> syntax_error ~msg:"Unexpected end of file" lexbuf
  in
  loop lexbuf s 0

let balance_pairs lexbuf ~opening ~closing =
  let opening = Ustring.of_string_exn opening in
  let closing = Ustring.of_string_exn closing in
  let buf = Queue.create () in
  let rec loop last_lexeme depth =
    if depth = 0 then Queue.to_array buf |> Ustring.of_array_unsafe
    else (
      Ustring.iter last_lexeme ~f:(Queue.enqueue buf);
      if prefix_matches lexbuf opening then loop opening (depth + 1)
      else if prefix_matches lexbuf closing then loop closing (depth - 1)
      else match%sedlex lexbuf with
        | any -> loop (lexeme lexbuf) depth
        | _ -> syntax_error ~msg:"Unexpected end of file" lexbuf)
  in
  loop Ustring.empty 1*)

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
    | '"' -> Queue.to_array buf |> Ustring.of_array_unsafe
    | "\\" -> add (read_escape lexbuf)
    | any -> add (lexeme_char lexbuf 0)
    | _ -> syntax_error ~msg:"String literal ended unexpectedly" lexbuf
  in
  loop lexbuf (Queue.create ())
;;

let get_indent t lexbuf =
  let rec loop lexbuf =
    match%sedlex lexbuf with
    | Star inline_space, Plus line_sep -> loop lexbuf
    | Plus ' ' -> lexeme_length lexbuf, Spaces
    | Plus '\t' -> lexeme_length lexbuf, Tabs
    | _ -> 0, Unknown
  in
  let size, typ = loop lexbuf in
  (match t.indent_type, typ with
  | Spaces, Tabs | Tabs, Spaces -> syntax_error ~msg:"Mixing spaces and tabs" lexbuf
  | Unknown, _ -> t.indent_type <- typ
  | _ -> ());
  size
;;

let handle_eof t =
  (* [handle_eof] handles any remaining dedents needed at end of file *)
  t.lex_state <- End_of_file;
  let indents = Stack.length t.indent_sizes - 1 in
  if indents > 0
  then (
    while Stack.top_exn t.indent_sizes > 0 do
      ignore (Stack.pop_exn t.indent_sizes : int)
    done;
    t.buffered_dedents <- indents - 1;
    DEDENT)
  else (
    t.buffered_dedents <- 0;
    EOF)
;;

let emit_dedents t lexbuf new_indent =
  let prev_len = Stack.length t.indent_sizes in
  while new_indent < Stack.top_exn t.indent_sizes do
    ignore (Stack.pop_exn t.indent_sizes : int)
  done;
  if new_indent <> Stack.top_exn t.indent_sizes
  then
    syntax_error ~msg:"Indentation does not match any previous indentation level" lexbuf;
  t.buffered_dedents <- prev_len - Stack.length t.indent_sizes - 1;
  (* TODO: re-figure out and document why this needs to be set *)
  t.lex_state <- After_indent;
  DEDENT
;;

let read =
  let rec read_indent t lexbuf ~on_indent =
    let new_indent = get_indent t lexbuf in
    let prev_indent = Stack.top_exn t.indent_sizes in
    if new_indent > prev_indent
    then on_indent t lexbuf ~new_indent ~prev_indent
    else if new_indent < prev_indent
    then emit_dedents t lexbuf new_indent
    else (
      match t.last_token with
      | INDENT | DEDENT ->
        (* Shouldn't ever produce LINE_SEP following INDENT or DEDENT *)
        read_after_indent t lexbuf
      | _ ->
        (match t.lex_state with
        | Start_of_file -> read_after_indent t lexbuf
        | _ ->
          (match%sedlex lexbuf with
          | '#' ->
            rollback lexbuf;
            read_after_indent t lexbuf
          | '=', Star inline_space, Plus line_sep -> EQUALS_ONLY_LINE
          | _ ->
            t.lex_state <- After_indent;
            LINE_SEP)))
  and read_after_indent t lexbuf =
    (* [read_after_indent] handles block comments (which can be only preceeded
       by whitespace on the same line) *)
    match%sedlex lexbuf with
    | line_comment ->
      let ignore_block t lexbuf ~new_indent:_ ~prev_indent =
        let rec loop t lexbuf base_indent =
          match%sedlex lexbuf with
          | Star (Compl '\n'), Plus line_sep ->
            let new_indent = get_indent t lexbuf in
            if new_indent = base_indent
            then read_after_indent t lexbuf
            else if new_indent < base_indent
            then emit_dedents t lexbuf new_indent
            else loop t lexbuf base_indent
          | Star (Compl '\n'), eof -> handle_eof t
          | _ -> assert false
        in
        loop t lexbuf prev_indent
      in
      read_indent t lexbuf ~on_indent:ignore_block
    | '#', Star (Compl '\n'), eof -> handle_eof t
    | '=', Star inline_space, Plus line_sep -> EQUALS_ONLY_LINE
    | _ -> read_inline t lexbuf
  and read_before_indent t lexbuf =
    (* [read_before_indent] handles indentation at the beginning of a line *)
    (* TODO: consider allowing block comments with leading spaces *)
    read_indent t lexbuf ~on_indent:(fun t _ ~new_indent ~prev_indent:_ ->
      Stack.push t.indent_sizes new_indent;
      INDENT)
  and read_inline t lexbuf =
    (* [read_inline] lexes lines from the point after the indentation *)
    match%sedlex lexbuf with
    (* Comments, indentation, and other whitespace *)
    | line_comment | Plus line_sep -> read_before_indent t lexbuf
    | '#', Star (Compl '\n'), eof -> handle_eof t
    | Plus inline_space -> read_inline t lexbuf
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
    | "match" -> MATCH
    | "with" -> WITH
    | "without" -> WITHOUT
    | "as" -> AS
    | "type" -> TYPE
    | "alias" -> ALIAS
    | "val" -> VAL
    | "extern" -> EXTERN
    | "infix" -> INFIX
    | "infixl" -> INFIXL
    | "infixr" -> INFIXR
    | "module", Star inline_space, ":" -> FILE_MODULE
    | "module" -> MODULE
    | "trait" -> TRAIT
    | "impl" -> IMPL
    | "import" -> IMPORT
    (* Special symbols *)
    | '=' -> EQUALS
    | '|' -> PIPE
    | ':', Plus inline_space -> COLON_SPACED
    | ':' -> COLON
    | ',' -> COMMA
    | '\\' -> BACKSLASH
    | '*' -> ASTERISK
    (* Need to support: `f . g`, `(. f)`, `(f .)`, and `(.)` as an operator *)
    | '.', Plus inline_space -> OPERATOR (Ustring.of_string_exn ".")
    | ".)" ->
      rollback lexbuf;
      ignore (next lexbuf : Uchar.t option);
      OPERATOR (Ustring.of_string_exn ".")
    | '.' -> PERIOD
    | "->" -> ARROW
    | "=>" -> FAT_ARROW
    (* Brackets *)
    | Chars "([{" ->
      let bracket_type =
        Bracket_type.of_char_exn (lexeme_char lexbuf 0 |> Uchar.to_char_exn)
      in
      Stack.push t.brackets bracket_type;
      t.ignoring_indents <- true;
      Bracket_type.to_token_left bracket_type
    | Chars ")]}" ->
      let actual = lexeme_str lexbuf in
      (match Stack.pop t.brackets with
      | Some bracket_type ->
        let expected = Bracket_type.to_char_right bracket_type in
        if String.(of_char expected = actual)
        then (
          if Stack.is_empty t.brackets then t.ignoring_indents <- false;
          Bracket_type.to_token_right bracket_type)
        else (
          let msg =
            sprintf "Mismatched brackets: expected `%c` but got `%s`" expected actual
          in
          syntax_error lexbuf ~msg)
      | None -> syntax_error ~msg:(sprintf "Unexpected `%s`" actual) lexbuf)
    (* Names *)
    | lowercase, Star (digit | alphabetic | '\'' | '_') -> LOWER_NAME (lexeme lexbuf)
    | '_', Plus (digit | alphabetic | '\'' | '_') -> LOWER_NAME (lexeme lexbuf)
    | '_' -> UNDERSCORE
    | uppercase, Star (digit | alphabetic | '\'' | '_') -> UPPER_NAME (lexeme lexbuf)
    (* Symbolic Operators (any unicode punctuation/symbol) *)
    | Plus operator_symbol -> OPERATOR (lexeme lexbuf)
    (* Misc. *)
    | eof -> handle_eof t
    | _ -> syntax_error ~msg:"Invalid character(s)" lexbuf
  in
  let rec read t lexbuf =
    let token =
      if t.buffered_dedents > 0
      then (
        t.buffered_dedents <- t.buffered_dedents - 1;
        DEDENT)
      else (
        match t.lex_state with
        | Start_of_file | Before_indent ->
          let token = read_before_indent t lexbuf in
          (match t.lex_state with
          | Start_of_file | Before_indent -> t.lex_state <- Inline
          | _ -> ());
          token
        | After_indent ->
          t.lex_state <- Inline;
          read_after_indent t lexbuf
        | Inline -> read_inline t lexbuf
        | End_of_file -> EOF)
    in
    let handle token =
      t.last_token <- token;
      token
    in
    (*if t.saw_block_starter then
      (* Saw a block starter: not ignoring indents *)
      match token with
      | INDENT ->
        (* Possibly entering a block; keep considering indents *)
        handle token
      | DEDENT (* want to ignore DEDENTs right after the block starter if in brackets*)*)
    (*match token with
    | INDENT when t.saw_block_starter ->
      (* Keep ignoring indents *)
      read t lexbuf
    | (DEDENT | LINE_SEP) when t.saw_block_starter ->
      (* Stop ignoring indents *)
      (* may or may not want to emit a dedent here 
         `
         module A :
           val a : Int
         val b : Int
         ` should have a dedent
         but
         `
         type A = {
           a : Int
         }
         ` should not
         Maybe we want to actually disable the indentation handling rather than just discard the outputs?

         Commas ending indentation sensitivity will also break some things:
         `
         let _ = {
           a =
             let () = ()
             1,           # This emitted an INDENT but no DEDENT (maybe ok?)
           b = 2
         }
         `



         HEY YOU: Try just doing a lookahead (yeah put ignoring_indents back)
         and check for Plus white_space, Chars ")]}" (could actually work)
      *)
      t.saw_block_starter <- false;
      handle token*)
    match token with
    | (INDENT | DEDENT | LINE_SEP) when t.ignoring_indents -> read t lexbuf
    | token ->
      (match token with
      | EQUALS | COLON | ARROW -> t.ignoring_indents <- false
      | COMMA -> t.ignoring_indents <- true
      | _ -> ());
      handle token
  in
  read
;;
