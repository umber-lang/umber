type t

val create : unit -> t
val read : t -> Sedlexing.lexbuf -> Parser.token

exception Syntax_error of (int * int) * Ustring.t [@@deriving sexp]

val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a
