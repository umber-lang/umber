open Import
open Names

module Label_name : sig
  type t [@@deriving compare, equal, sexp_of]

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

  val n_bytes : t -> int
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

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Call_conv : sig
  type t =
    | C
    | Umber

  val arg_registers : t -> Register.t Nonempty.t
  val return_value_register : t -> Register.t
  val register_is_reserved : t -> Register.t -> bool
  val all_available_registers : t -> Register.t list
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

  (** Dereference memory with an offset, with similar semantics to C dereferencing, so if 
      calling [mem_offset t size n], the actual byte offset is [Size.n_bytes size * n]. *)
  val mem_offset : 'reg t -> Size.t -> int -> 'reg t

  val map_registers : 'r1 t -> f:('r1 -> 'r2) -> 'r2 t
  val fold_registers : 'reg t -> init:'acc -> f:('acc -> 'reg -> 'acc) -> 'acc

  val fold_map_registers
    :  'r1 t
    -> init:'acc
    -> f:('acc -> 'r1 -> 'acc * 'r2)
    -> 'acc * 'r2 t
end

module Instr : sig
  module Nonterminal : sig
    type 'reg t =
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
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Mov of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Setz of 'reg Value.t
      | Test of 'reg Value.t * 'reg Value.t
    [@@deriving sexp_of]

    val map_args : 'r1 t -> f:('r1 Value.t -> 'r2 Value.t) -> 'r2 t

    val fold_map_args
      :  'r1 t
      -> init:'acc
      -> f:('acc -> 'r1 Value.t -> 'acc * 'r2 Value.t)
      -> 'acc * 'r2 t
  end

  module Terminal : sig
    type t =
      | Ret
      | Jump of Label_name.t
      | Jump_if of
          { cond : [ `Zero | `Nonzero ]
          ; then_ : Label_name.t
          ; else_ : Label_name.t
          }
    [@@deriving sexp_of]
  end

  type 'reg t =
    | Terminal of Terminal.t
    | Nonterminal of 'reg Nonterminal.t
  [@@deriving sexp_of]

  (* FIXME: Cleanup. Also, do we even need the [Instr] type? *)
  (* val fold_map_args
    :  'r1 t
    -> init:'acc
    -> f:('acc -> 'r1 Value.t -> 'acc * 'r2 Value.t)
    -> 'acc * 'r2 t *)
end

module Basic_block : sig
  type 'reg t =
    { label : Label_name.t
    ; code : 'reg Instr.Nonterminal.t list
    ; terminal : Instr.Terminal.t
    }
  [@@deriving sexp_of]
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
  ; text_section : Register.t Basic_block.t list
  ; rodata_section : Data_decl.t list
  ; bss_section : Bss_decl.t list
  }

val pp : Format.formatter -> t -> unit
