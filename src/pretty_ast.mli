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

module Typed_to_untyped : sig
  val convert_module : names:Name_bindings.t -> Typed.Module.t -> Untyped.Module.t
end
