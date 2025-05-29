open! Core
open! Import

module Size : sig
  type t =
    | I8
    | I16
    | I32
    | I64
  [@@deriving equal, sexp_of]

  val n_bytes : t -> int
end

module Asm_literal : sig
  type t =
    | Int of int
    | Float of float
    | String of Ustring.t
  [@@deriving sexp_of, equal]
end

module Call_conv : sig
  type t =
    | C
    | Umber

  val arg_registers : t -> Register.Real.t Nonempty.t
  val non_arg_caller_save_registers : t -> Register.Real.t list
  val return_value_register : t -> Register.Real.t
  val register_is_reserved : t -> Register.Real.t -> bool
  val all_available_registers : t -> Register.Real.t list

  module Umber : sig
    val fiber_register : Register.Real.t
  end
end

module Global_kind : sig
  type t =
    | Extern_proc
    | Other
  [@@deriving sexp_of]
end

module Simple_value : sig
  type 'reg t =
    | Register of 'reg
    | Global of Label_name.t * Global_kind.t
    | Constant of Asm_literal.t
  [@@deriving sexp_of]

  val map_registers : 'r1 t -> f:('r1 -> 'r2) -> 'r2 t
  val fold_registers : 'reg t -> init:'acc -> f:('acc -> 'reg -> 'acc) -> 'acc

  val fold_map_registers
    :  'r1 t
    -> init:'acc
    -> f:('acc -> 'r1 -> 'acc * 'r2)
    -> 'acc * 'r2 t
end

module Memory : sig
  type 'reg expr =
    | Value of 'reg Simple_value.t
    | Add of 'reg expr * 'reg expr
  [@@deriving sexp_of]

  type 'reg t = Size.t * 'reg expr [@@deriving sexp_of]

  (** Dereference memory with an offset, with similar semantics to C dereferencing, so if 
      calling [offset v size n], the actual byte offset is [Size.n_bytes size * n]. *)
  val offset : 'reg Simple_value.t -> Size.t -> int -> 'reg t

  val fold_simple_values
    :  'reg t
    -> init:'acc
    -> f:('acc -> 'reg Simple_value.t -> 'acc)
    -> 'acc
end

module Value : sig
  type 'reg t =
    | Simple_value of 'reg Simple_value.t
    | Memory of 'reg Memory.t
  [@@deriving sexp_of]
end

module Register_op : sig
  type t =
    | Use
    | Assignment
    | Use_and_assignment
end

module Instr : sig
  module Nonterminal : sig
    type 'reg t =
      | Add of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | And of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Call of
          { fun_ : 'reg Value.t
          ; call_conv : Call_conv.t
          ; arity : int
          }
      | Cmp of 'reg Value.t * 'reg Value.t
      | Lea of
          { dst : 'reg
          ; src : 'reg Memory.t
          }
      | Mov of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Pop of 'reg Value.t
      | Push of 'reg Value.t
      | Sub of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Test of 'reg Value.t * 'reg Value.t
    [@@deriving sexp_of, variants]

    val map_args : 'r1 t -> f:('r1 Simple_value.t -> 'r2 Simple_value.t) -> 'r2 t

    val fold_map_args
      :  'r1 t
      -> init:'acc
      -> f:('acc -> 'r1 Simple_value.t -> op:Register_op.t -> 'acc * 'r2 Simple_value.t)
      -> 'acc * 'r2 t
  end

  module Terminal : sig
    type 'reg t =
      | Ret
      | Jump of 'reg Value.t
      | Jump_if of
          (* FIXME: The [else_] label is basically a meme, the correctness is not actually
             enforced. We just have to be careful to only use "else" to mean the label
             following this one...
             
             We can either:
             - Add a check to assert that the else case is the following label
             - Serialize this as jnz then_ followed by jmp else_, with an extra check to
               skip the second jump if the next label is the same as else_

             Both options seem pretty messy. 

             Another idea is to remove the else case and have code using it check the next
             label in the list. That's annoying too though.
          *)
          
          { cond : [ `Zero | `Nonzero ]
          ; then_ : Label_name.t
          ; else_ : Label_name.t
          }
    [@@deriving sexp_of]

    val map_registers : 'r1 t -> f:('r1 -> 'r2) -> 'r2 t
  end
end

module Basic_block : sig
  type 'reg t =
    { label : Label_name.t
    ; code : 'reg Instr.Nonterminal.t list
    ; terminal : 'reg Instr.Terminal.t
    }
  [@@deriving sexp_of]

  val map_registers : 'r1 t -> f:('r1 -> 'r2) -> 'r2 t
  val fold_registers : 'reg t -> init:'acc -> f:('acc -> 'reg -> 'acc) -> 'acc
end

module Data_decl : sig
  module Payload : sig
    type t =
      | Literal of Asm_literal.t
      | Label of Label_name.t
  end

  type t =
    { label : Label_name.t
    ; payloads : (Size.t * Payload.t) list
    }
end

module Bss_decl : sig
  type t =
    { label : Label_name.t
    ; kind : [ `Words ]
    ; size : int
    }

  val pp : Format.formatter -> t -> unit
end

module Global_decl : sig
  type t =
    { name : Label_name.t
    ; strength : [ `Strong | `Weak ]
    }
end

type t =
  { globals : Global_decl.t list
  ; externs : Label_name.t list
  ; text_section : Register.Real.t Basic_block.t list
  ; rodata_section : Data_decl.t list
  ; bss_section : Bss_decl.t list
  }

val empty : t
val pp : Format.formatter -> t -> unit
