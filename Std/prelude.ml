open! Core

let lazy_read filename ~f = lazy (In_channel.read_all filename |> f)
let names = lazy_read "prelude_names.sexp" ~f:Sexp.of_string
let llvm = lazy_read "prelude_llvm.ll" ~f:Fn.id