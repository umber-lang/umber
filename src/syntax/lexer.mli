open! Core
open! Import

val read : Sedlexing.lexbuf -> Parser.token
val span : Sedlexing.lexbuf -> Span.t
val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a
val escape_string_literal : Ustring.t -> string
val escape_char_literal : Uchar.t -> string
