open Import
open Names

module Label_name : sig
  type t [@@deriving sexp_of]

  include Stringable.S with type t := t
  include Hashable.S with type t := t

  val of_mir_name : Mir_name.t -> t
  val of_extern_name : Extern_name.t -> t
  val to_mir_name : t -> Mir_name.t
end

module Size : sig
  type t =
    | I8
    | I16
    | I32
    | I64
  [@@deriving sexp_of]
end

module Asm_literal : sig
  type t =
    | Int of int
    | Float of float
    | String of Ustring.t
  [@@deriving sexp_of]
end

module Register : sig
  type t =
    | Rax
    | Rcx
    | Rdx
    | Rbx
    | Rsp
    | Rbp
    | Rsi
    | Rdi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving compare, equal, hash, sexp, enumerate]

  include Hashable.S with type t := t
end

module Global_kind : sig
  type t =
    | Extern_proc
    | Other
  [@@deriving sexp_of]
end

module Value : sig
  type 'reg t =
    | Register of 'reg
    | Memory of Size.t * 'reg memory_expr
    | Global of Label_name.t * Global_kind.t
    | Constant of Asm_literal.t

  and 'reg memory_expr =
    | Value of 'reg t
    | Add of 'reg memory_expr * 'reg memory_expr
  [@@deriving sexp_of]

  val mem_offset : 'reg t -> Size.t -> int -> 'reg t

  val fold_map_registers
    :  'r1 t
    -> init:'acc
    -> f:('acc -> 'r1 -> 'acc * 'r2)
    -> 'acc * 'r2 t
end

module Instr : sig
  type 'reg t =
    | And of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Call of 'reg Value.t
    | Cmp of 'reg Value.t * 'reg Value.t
    | Jmp of Label_name.t
    | Jnz of Label_name.t
    | Jz of Label_name.t
    | Lea of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Mov of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Ret
    | Setz of 'reg Value.t
    | Test of 'reg Value.t * 'reg Value.t
  [@@deriving sexp_of]

  (** Returns [true] iff the instruction is valid to terminator an instruction group. *)
  val is_terminator : _ t -> bool

  val fold_map_args
    :  'r1 t
    -> init:'acc
    -> f:('acc -> 'r1 Value.t -> 'acc * 'r2 Value.t)
    -> 'acc * 'r2 t
end

module Instr_group : sig
  type t =
    { label : Label_name.t
    ; instrs : Register.t Instr.t list
    }
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
  ; text_section : Instr_group.t list
  ; rodata_section : Data_decl.t list
  ; bss_section : Bss_decl.t list
  }

val pp : Format.formatter -> t -> unit
