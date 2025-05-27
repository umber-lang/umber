include Names
module Pattern = Untyped_ast.Pattern
module Effect_pattern = Untyped_ast.Effect_pattern
module Expr = Untyped_ast.Expr
module Module = Module

let single_or_list wrapper = function
  | [ x ] -> x
  | xs -> wrapper xs
;;
