open Names

type t = (Value_name.Relative.t, Untyped.Expr.t) Btree.t

(* TODO: add more tests for this *)

(** Re-associate the operator tree through tree rotations.
    These constraints hold on the finished tree:
    1. Every node has precedence less than or equal to both its childrens' precedence.
    2. When a node has precedence equal to that some of its children,
       it and those children all share the same associativity:
       a. In the case of left associativity, rotating anticlockwise results in a tree
          which violates constraint 1.
       b. In the case of right associativity, rotating clockwise results in a tree
          which violates constraint 1.
       c. The case of no associativity is not allowed. *)
val to_untyped_expr : names:Name_bindings.t -> t -> Untyped.Expr.t
