open! Import

type llvalue = Llvm.llvalue [@@deriving sexp_of]
type llmodule = Llvm.llmodule [@@deriving sexp_of]

module Opcode : sig
  type t = [%import: Llvm.Opcode.t] [@@deriving sexp]
end

module Value_kind : sig
  type t = [%import: Llvm.ValueKind.t] [@@deriving sexp]
end