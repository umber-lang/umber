module Document : sig
  type t =
    | Empty
    | Text of string
    | Break of string
    | Concat of t * t
    | Indent of int * t
    | Group of t

  val space : t
  val line_break : t

  (** [Concat] two documents. *)
  val ( ^^ ) : t -> t -> t

  (** Concatenate two documents with a space separator in between. *)
  val ( ^| ) : t -> t -> t

  val separated : ?sep:t -> t list -> t
end

val format : Document.t -> max_line_length:int -> string Base.Sequence.t
val print : Document.t -> max_line_length:int -> unit
