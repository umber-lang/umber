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
  type t =
    | Register of Register.t
    | Memory of Size.t * memory_expr
    | Global of Label_name.t * Global_kind.t
    | Constant of Asm_literal.t

  and memory_expr =
    | Value of t
    | Add of memory_expr * memory_expr
  [@@deriving sexp_of]

  val mem_offset : t -> Size.t -> int -> t
end

module Instr : sig
  type t =
    | And of
        { dst : Value.t
        ; src : Value.t
        }
    | Call of Value.t
    | Cmp of Value.t * Value.t
    | Jmp of Label_name.t
    | Jnz of Label_name.t
    | Jz of Label_name.t
    | Lea of
        { dst : Value.t
        ; src : Value.t
        }
    | Mov of
        { dst : Value.t
        ; src : Value.t
        }
    | Ret
    | Setz of Value.t
    | Test of Value.t * Value.t
end

module Instr_group : sig
  type t =
    { label : Label_name.t
    ; instrs : Instr.t list
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
