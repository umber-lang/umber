open! Import

module Config : sig
  type t =
    { max_line_length : int
    ; indent_size : int
    }

  val default : t
end

val format : ?config:Config.t -> Untyped.Module.t -> string Sequence.t
val format_to_document : ?config:Config.t -> Untyped.Module.t -> Auto_format.Document.t
val typed_ast_to_untyped_annotated_module : Typed.Module.t -> Untyped.Module.t
