open! Import

type llvalue = Llvm.llvalue [@@deriving sexp_of]
type llmodule = Llvm.llmodule [@@deriving sexp_of]
