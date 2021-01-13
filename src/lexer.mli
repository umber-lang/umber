type t

val create : unit -> t
val read : t -> Sedlexing.lexbuf -> Parser.token
val span : Sedlexing.lexbuf -> Span.t

exception Syntax_error of Span.Pos.t * Ustring.t [@@deriving sexp]

val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a
