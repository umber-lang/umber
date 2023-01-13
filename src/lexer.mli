open! Import

val read : Sedlexing.lexbuf -> Parser.token
val span : Sedlexing.lexbuf -> Span.t

exception Syntax_error of Span.Pos.t * Ustring.t [@@deriving sexp]

val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a
