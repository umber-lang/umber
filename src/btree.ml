open Import

type ('node, 'leaf) t =
  | Node of 'node * ('node, 'leaf) t * ('node, 'leaf) t
  | Leaf of 'leaf
[@@deriving equal, compare, hash]

let rec sexp_of_t sexp_of_node sexp_of_leaf = function
  | Leaf data -> Sexp.List [ sexp_of_leaf data ]
  | Node (data, left_child, right_child) ->
    let sexp_of = sexp_of_t sexp_of_node sexp_of_leaf in
    List [ sexp_of_node data; List [ sexp_of left_child; sexp_of right_child ] ]
;;

let rec t_of_sexp node_of_sexp leaf_of_sexp = function
  | Sexp.List [ node; List [ left_child; right_child ] ] ->
    let t_of = t_of_sexp node_of_sexp leaf_of_sexp in
    Node (node_of_sexp node, t_of left_child, t_of right_child)
  | List [ leaf ] -> Leaf (leaf_of_sexp leaf)
  | _ -> raise_s [%message "Btree.t_of_sexp: parse failed"]
;;

let rotate_anticlockwise_exn = function
  | Leaf _ as leaf -> leaf
  | Node (root_data, left_child, right_child) ->
    (match right_child with
    | Leaf _ -> raise_s [%message "Cannot rotate leaf anticlockwise into a node"]
    | Node (right_data, left_grandchild, right_grandchild) ->
      Node (right_data, Node (root_data, left_child, left_grandchild), right_grandchild))
;;

let rotate_clockwise_exn = function
  | Leaf _ as leaf -> leaf
  | Node (root_data, left_child, right_child) ->
    (match left_child with
    | Leaf _ -> raise_s [%message "Cannot rotate leaf clockwise into a node"]
    | Node (left_data, left_grandchild, right_grandchild) ->
      Node (left_data, left_grandchild, Node (root_data, right_grandchild, right_child)))
;;

module Rotation = struct
  type t =
    | Clockwise
    | Anticlockwise
end
