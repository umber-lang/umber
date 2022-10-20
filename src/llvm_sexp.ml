open! Import

type llvalue = Llvm.llvalue

let sexp_of_llvalue value = Sexp.Atom (Llvm.string_of_llvalue value)

type llmodule = Llvm.llmodule

let sexp_of_llmodule module_ = Sexp.Atom (Llvm.string_of_llmodule module_)