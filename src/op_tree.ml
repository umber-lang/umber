open! Import
open Names

type t = (Value_name.Relative.t Node.t, Untyped.Expr.t Node.t) Btree.t
[@@deriving sexp_of]

(* FIXME: cleanup *)
let rec fix_precedence ~names (t : t) : t =
  (* print_s [%message "start fix_precedence" ~input:(t : t)]; *)
  let result =
    match t with
    | Leaf _ as leaf -> leaf
    | Node (op_name, left_child, right_child) ->
      let left_child = fix_precedence ~names left_child in
      let right_child = fix_precedence ~names right_child in
      (* print_s
        [%message "fix_precedence node" ~input:(t : t) (left_child : t) (right_child : t)]; *)
      let rotation =
        match left_child, right_child with
        | Leaf _, Leaf _ -> None
        | Node (left_name, ll_child, lr_child), Leaf _ ->
          let _, left_level =
            Node.with_value left_name ~f:(Name_bindings.find_fixity names)
          in
          Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
        | Leaf _, Node (right_name, rl_child, rr_child) ->
          let _, right_level =
            Node.with_value right_name ~f:(Name_bindings.find_fixity names)
          in
          Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
        | Node (left_name, ll_child, lr_child), Node (right_name, rl_child, rr_child) ->
          let _, left_level =
            Node.with_value left_name ~f:(Name_bindings.find_fixity names)
          in
          let _, right_level =
            Node.with_value right_name ~f:(Name_bindings.find_fixity names)
          in
          if Fixity.Level.(left_level <= right_level)
          then Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
          else Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
      in
      (match rotation with
       | None -> Node (op_name, left_child, right_child)
       | Some (level, rotation_info) ->
         let _, op_level = Node.with_value op_name ~f:(Name_bindings.find_fixity names) in
         if Fixity.Level.(level < op_level)
         then (
           (* FIXME: Do I really have to call fix_precedence again here? It traverses the
             whole subtree again, which seems kinda crazy. (Although it shouldn't be
             incorrect unless there's another bug somewhere.) *)
           match rotation_info with
           | `Clockwise, left_name, ll_child, lr_child ->
             Node
               ( left_name
               , ll_child
               , fix_precedence ~names (Node (op_name, lr_child, right_child)) )
           | `Anticlockwise, right_name, rl_child, rr_child ->
             Node
               ( right_name
               , fix_precedence ~names (Node (op_name, left_child, rl_child))
               , rr_child ))
         else Node (op_name, left_child, right_child))
  in
  (* print_s [%message "end fix_precedence" ~input:(t : t) (result : t)]; *)
  result
;;

let can_rotate op_level op_assoc child_level child_assoc =
  if Fixity.Level.(child_level = op_level)
  then
    if Fixity.Assoc.compatible child_assoc op_assoc
    then true
    else Type_bindings.type_error_msg "Associativity error"
  else false
;;

let rec fix_associativity ~names (t : t) : t =
  match t with
  | Node (op_name, left_child, right_child) ->
    let op_assoc, op_level =
      Node.with_value op_name ~f:(Name_bindings.find_fixity names)
    in
    handle_node ~names op_name op_assoc op_level left_child right_child
  | Leaf _ as leaf -> leaf

and handle_node ~names op_name op_assoc op_level left_child right_child =
  match op_assoc with
  | Left -> turn_anticlockwise ~names op_name op_assoc op_level left_child right_child
  | Right -> turn_clockwise ~names op_name op_assoc op_level left_child right_child
  | Non_assoc ->
    (* Forbid children to have the same level *)
    Node
      ( op_name
      , handle_non_assoc ~names op_level left_child
      , handle_non_assoc ~names op_level right_child )

and turn_anticlockwise ~names op_name op_assoc op_level left_child right_child =
  match right_child with
  | Node (right_name, rl_child, rr_child) ->
    let right_assoc, right_level =
      Node.with_value right_name ~f:(Name_bindings.find_fixity names)
    in
    if can_rotate op_level op_assoc right_level right_assoc
    then (
      (* Keep trying to rotate anticlockwise *)
      let new_left = Btree.Node (op_name, left_child, rl_child) in
      turn_anticlockwise ~names right_name right_assoc right_level new_left rr_child)
    else Node (op_name, fix_associativity ~names left_child, right_child)
  | Leaf _ -> Node (op_name, fix_associativity ~names left_child, right_child)

and turn_clockwise ~names op_name op_assoc op_level left_child right_child =
  match left_child with
  | Btree.Node (left_name, ll_child, lr_child) ->
    let left_assoc, left_level =
      Node.with_value left_name ~f:(Name_bindings.find_fixity names)
    in
    if can_rotate op_level op_assoc left_level left_assoc
    then (
      (* Keep trying to rotate clockwise *)
      let new_right = Btree.Node (op_name, lr_child, right_child) in
      turn_clockwise ~names left_name left_assoc left_level ll_child new_right)
    else Node (op_name, left_child, fix_associativity ~names right_child)
  | Leaf _ -> Node (op_name, left_child, fix_associativity ~names right_child)

and handle_non_assoc ~names parent_level = function
  | Btree.Node (op_name, left_child, right_child) ->
    let op_assoc, op_level =
      Node.with_value op_name ~f:(Name_bindings.find_fixity names)
    in
    if Fixity.Level.(op_level = parent_level)
    then Type_bindings.type_error_msg "Associativity error"
    else handle_node ~names op_name op_assoc op_level left_child right_child
  | Leaf _ as leaf -> leaf
;;

let rec to_untyped_expr : t -> Untyped.Expr.t Node.t = function
  | Leaf expr -> expr
  | Node (op_name, left_child, right_child) ->
    let left_arg, right_arg = to_untyped_expr left_child, to_untyped_expr right_child in
    let span =
      Span.combine
        (Node.span op_name)
        (Span.combine (Node.span left_arg) (Node.span right_arg))
    in
    Node.create
      (Untyped.Expr.Fun_call
         (Node.map op_name ~f:Untyped.Expr.name, [ left_arg; right_arg ]))
      span
;;

(* FIXME: cleanup comment and implement checking *)
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
let rec check_invariants (t : t) ~names =
  match t with
  | Leaf _ -> true
  | Node (op_name, left_child, right_child) ->
    let op_assoc, op_level =
      Node.with_value op_name ~f:(Name_bindings.find_fixity names)
    in
    let ( < ) = Fixity.Level.( < ) in
    let ( = ) = Fixity.Level.( = ) in
    let check_associativity
      (left_op_assoc : Fixity.Assoc.t option)
      (right_op_assoc : Fixity.Assoc.t option)
      =
      match op_assoc with
      | Non_assoc -> false
      | Left ->
        (* FIXME: Also need to check that "rotating anticlockwise would violate constraint 1"
           Rotating anticlockwise means:
           
              A          C
             / \  -->   / \
            B   C      A   E
               / \    / \
              D   E  B   D
          So if the second tree breaks constraint 1 (but the first doesn't), that means
          A.level > D.level. Not sure if this is actually the correct constraint.
          Wait, but this doesn't make sense. We need to have A <= C <= D.
        *)
        (match left_op_assoc, right_op_assoc with
         | (None | Some Left), (None | Some Left) -> true
         | _ -> false)
      | Right ->
        (* FIXME: Also need to check that "rotating anticlockwise would violate constraint 1" *)
        (match left_op_assoc, right_op_assoc with
         | (None | Some Right), (None | Some Right) -> true
         | _ -> false)
    in
    (* FIXME: code duplication *)
    check_invariants left_child ~names
    && check_invariants right_child ~names
    &&
    (match left_child, right_child with
     | Leaf _, Leaf _ -> true
     | Node (left_op_name, _, _), Leaf _ ->
       let left_op_assoc, left_op_level =
         Node.with_value left_op_name ~f:(Name_bindings.find_fixity names)
       in
       op_level < left_op_level
       || (op_level = left_op_level && check_associativity (Some left_op_assoc) None)
     | Leaf _, Node (right_op_name, _, _) ->
       let right_op_assoc, right_op_level =
         Node.with_value right_op_name ~f:(Name_bindings.find_fixity names)
       in
       op_level < right_op_level
       || (op_level = right_op_level && check_associativity None (Some right_op_assoc))
     | Node (left_op_name, _, _), Node (right_op_name, _, _) ->
       let left_op_assoc, left_op_level =
         Node.with_value left_op_name ~f:(Name_bindings.find_fixity names)
       in
       let right_op_assoc, right_op_level =
         Node.with_value right_op_name ~f:(Name_bindings.find_fixity names)
       in
       (op_level < left_op_level
       || (op_level = left_op_level && check_associativity (Some left_op_assoc) None))
       && (op_level < right_op_level
          || (op_level = right_op_level && check_associativity None (Some right_op_assoc))
          ))
;;

let to_untyped_expr ~names t =
  let t = fix_precedence t ~names |> fix_associativity ~names in
  assert_or_compiler_bug (check_invariants t ~names) ~here:[%here];
  to_untyped_expr t
;;
