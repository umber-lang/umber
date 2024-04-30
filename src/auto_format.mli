module Document : sig
  type t =
    | Empty
    | Text of string
    | Break
    | Concat of t * t
    | Indent of int * t
    | Group of t
end

val format : Document.t -> max_line_length:int -> string Base.Sequence.t
val format_and_print : Document.t -> max_line_length:int -> unit
