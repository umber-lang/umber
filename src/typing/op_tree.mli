open! Core
open! Import
open Names

type t = (Value_name.Relative.t Node.t, Untyped_ast.Expr.t Node.t) Btree.t

(** Re-associate the operator tree through tree rotations.
    These constraints hold on the finished tree:
    1. Every node has precedence less than or equal to both its non-leaf children's
       precedence.
    2. Left-associative nodes do not have left-associative right-children with the same
       precedence.
    3. Same as (2) but with "left" and "right" swapped. *)
val to_untyped_expr : names:Name_bindings.t -> t -> Untyped_ast.Expr.t Node.t

(** Convert the operator tree to an expression without fixing precedence first. *)
val to_untyped_expr_as_is : t -> Untyped_ast.Expr.t Node.t
