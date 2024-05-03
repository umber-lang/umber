module Document : sig
  type t =
    | Empty
    | Text of string
    | Break
    | Force_break
    | Concat of t * t
    | Indent of int * t
    | Group of t
  [@@deriving sexp, variants]

  (** [Concat] two documents. *)
  val ( ^^ ) : t -> t -> t

  (** Concatenate two documents with a space separator in between. *)
  val ( ^| ) : t -> t -> t

  val concat_all : t list -> t
  val separated : ?sep:t -> t list -> t
end

val format : Document.t -> max_line_length:int -> string Base.Sequence.t
val print : Document.t -> max_line_length:int -> unit
