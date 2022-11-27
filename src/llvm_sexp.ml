open! Import

type llvalue = Llvm.llvalue

let sexp_of_llvalue value = Sexp.Atom (Llvm.string_of_llvalue value)

type llmodule = Llvm.llmodule

let sexp_of_llmodule module_ = Sexp.Atom (Llvm.string_of_llmodule module_)

module Opcode = struct
  type t = [%import: Llvm.Opcode.t] [@@deriving sexp]
end

module Value_kind = struct
  type t = [%import: Llvm.ValueKind.t] [@@deriving sexp]
end