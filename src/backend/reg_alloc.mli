open! Core
open! Import

val allocate
  :  basic_blocks:Register.t Asm_program.Basic_block.t list
  -> register_counter:Register.Virtual.Counter.t
  -> Register.Real.t Asm_program.Basic_block.t list
