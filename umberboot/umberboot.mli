open! Core
open! Umber

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

val compile_and_print
  :  ?no_std:bool
  -> ?parent:Ast.Module_name.t
  -> filename:string
  -> Target.t list
  -> unit

val command : Command.t