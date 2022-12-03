open! Import

val lex_lower_name : Sedlexing.lexbuf -> Ustring.t Or_error.t
val lex_upper_name : Sedlexing.lexbuf -> Ustring.t Or_error.t
val lex_eof : Sedlexing.lexbuf -> bool
val lex_place : Sedlexing.lexbuf -> [ `Sig | `Def ] option
