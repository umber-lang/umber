type t

val create : unit -> t
val read : t -> Sedlexing.lexbuf -> Parser.token

exception Syntax_error of (int * int) * Ustring.t [@@deriving sexp]

val syntax_error : ?msg:string -> Sedlexing.lexbuf -> 'a

(* TODO: functions valid_upper_name and valid_lower_name to expose
   for name validation in of_string_exn and of_ustring (_exn?) *)
