open! Import

val read : Sedlexing.lexbuf -> Parser.token
val span : Sedlexing.lexbuf -> Span.t
val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a
