open! Core
open! Import

type ('node, 'leaf) t =
  | Node of 'node * ('node, 'leaf) t * ('node, 'leaf) t
  | Leaf of 'leaf
[@@deriving equal, compare, hash, quickcheck]

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

let rec length = function
  | Leaf _ -> 1
  | Node (_, left, right) -> 1 + length left + length right
;;

let rec inorder_nodes_gen =
  let open Sequence.Generator in
  function
  | Node (value, left, right) ->
    inorder_nodes_gen left >>= fun () -> yield value >>= fun () -> inorder_nodes_gen right
  | Leaf _ -> return ()
;;

let inorder_nodes t = Sequence.Generator.run (inorder_nodes_gen t)
