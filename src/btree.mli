open Import

type ('node, 'leaf) t =
  | Node of 'node * ('node, 'leaf) t * ('node, 'leaf) t
  | Leaf of 'leaf
[@@deriving equal, compare, hash, sexp, quickcheck]

val length : ('a, 'b) t -> int
val inorder_nodes : ('node, _) t -> 'node Sequence.t
