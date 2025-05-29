open! Core
open! Import
open Asm_program

type t [@@deriving sexp_of]

val create : Label_name.t -> arity:int -> t
val add_code : t -> Register.t Instr.Nonterminal.t -> unit
val add_terminal : t -> Register.t Instr.Terminal.t -> unit
val add_local : t -> Mir_name.t -> Register.t Value.t -> unit
val find_local : t -> Mir_name.t -> Register.t Value.t option
val current_label : t -> Label_name.t
val position_at_label : t -> Label_name.t -> unit
val create_label : ?global:bool -> t -> ('a, unit, string, Label_name.t) format4 -> 'a
val pick_register : t -> Register.t Value.t
val pick_register' : t -> Register.Virtual.t

(* TODO: Most uses of [pick_register] should probably use [move_to_new_register] instead. *)
val move_to_new_register : t -> Register.t Value.t -> Register.t Value.t
val move_to_new_register' : t -> Register.t Value.t -> Register.Virtual.t
val name : t -> Label_name.t
val arity : t -> int

(* TODO: This could be Nonempty because we always at least have an entry block. *)
val basic_blocks : t -> Register.Real.t Basic_block.t list
