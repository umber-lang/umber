open Names

type t = (Value_name.Relative.t Node.t, Untyped.Expr.t Node.t) Btree.t

(* FIXME: Check associativity conditions, not sure about them.
   
   2. Left-associative nodes do not have left-associative right-children with the same
      precedence.
   3. Same as (2) but with "left" and "right" swapped.
*)

(** Re-associate the operator tree through tree rotations.
    These constraints hold on the finished tree:
    1. Every node has precedence less than or equal to both its non-leaf children's
       precedence.
    2. One of the following holds:
       a. If the node is left-associative, its precedence is strictly less than its right
          child's precedence (or its right child is a leaf node), and its precedence is
          strictly less than its left child's precedence, or its left child is a leaf
          node, or its precedence is equal to its left child's precedence and its left
          child is also left-associative.
       b. Same as (a), but with right and left swapped.
       c. If the node is non-associative, its precedence is strictly less than both its
          non-leaf children's precedence. *)
val to_untyped_expr : names:Name_bindings.t -> t -> Untyped.Expr.t Node.t
