include Names
module Pattern = Untyped.Pattern
module Effect_pattern = Untyped.Effect_pattern
module Expr = Untyped.Expr
module Module = Module

let single_or_list wrapper = function
  | [ x ] -> x
  | xs -> wrapper xs
;;
