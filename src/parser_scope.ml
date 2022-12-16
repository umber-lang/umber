include Names
module Pattern = Pattern
module Expr = Untyped.Expr
module Module = Module

let single_or_list wrapper = function
  | [ x ] -> x
  | xs -> wrapper xs
;;
