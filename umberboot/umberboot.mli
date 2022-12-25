open! Core
open! Umber

module Stage : sig
  (** Stages of compilation. The order is significant; later stages depend on earlier
      stages. *)
  type t =
    | Lexing
    | Parsing
    | Type_checking
    | Generating_mir
    | Generating_llvm
    | Linking
  [@@deriving compare, variants, sexp]
end

module Target : sig
  (** Targets for the compiler to output. Each require up to a certain compilation stage
      to be completed. *)
  type t =
    | Tokens
    | Untyped_ast
    | Typed_ast
    | Names
    | Mir
    | Llvm
    | Exe
  [@@deriving compare, variants, sexp]
end

module File_or_stdout : sig
  type t =
    | File of Filename.t
    | Stdout

  val with_out_channel : t -> f:(Out_channel.t -> 'a) -> 'a
end

val compile
  :  ?no_std:bool
  -> ?parent:Ast.Module_name.t
  -> ?renumber_mir_ids:bool
  -> ?on_error:(Stage.t -> Compilation_error.t -> unit)
  -> filename:string
  -> (Target.t * File_or_stdout.t) list
  -> unit

val command : Command.t