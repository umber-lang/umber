open Import
open Names

type t = (Value_name.Relative.t, Untyped.Expr.t) Btree.t

let rec fix_precedence ~names = function
  | Btree.Node (op_name, left_child, right_child) as root ->
    let left_child = fix_precedence ~names left_child in
    let right_child = fix_precedence ~names right_child in
    let rotation =
      match left_child, right_child with
      | Btree.Leaf _, Leaf _ -> None
      | Node (left_name, ll_child, lr_child), Leaf _ ->
        let _, left_level = Name_bindings.find_fixity names left_name in
        Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
      | Leaf _, Node (right_name, rl_child, rr_child) ->
        let _, right_level = Name_bindings.find_fixity names right_name in
        Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
      | Node (left_name, ll_child, lr_child), Node (right_name, rl_child, rr_child) ->
        let _, left_level = Name_bindings.find_fixity names left_name in
        let _, right_level = Name_bindings.find_fixity names right_name in
        if Fixity.Level.(left_level <= right_level)
        then Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
        else Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
    in
    (match rotation with
     | None -> root
     | Some (level, rotation_info) ->
       let _, op_level = Name_bindings.find_fixity names op_name in
       if Fixity.Level.(level < op_level)
       then (
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
       else root)
  | Leaf _ as leaf -> leaf
;;

let can_rotate op_level op_assoc child_level child_assoc =
  if Fixity.Level.(child_level = op_level)
  then
    if Fixity.Assoc.compatible child_assoc op_assoc
    then true
    else Type_bindings.type_error_msg "Associativity error"
  else false
;;

let rec fix_associativity ~names = function
  | Btree.Node (op_name, left_child, right_child) ->
    let op_assoc, op_level = Name_bindings.find_fixity names op_name in
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
  | Btree.Node (right_name, rl_child, rr_child) ->
    let right_assoc, right_level = Name_bindings.find_fixity names right_name in
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
    let left_assoc, left_level = Name_bindings.find_fixity names left_name in
    if can_rotate op_level op_assoc left_level left_assoc
    then (
      (* Keep trying to rotate clockwise *)
      let new_right = Btree.Node (op_name, lr_child, right_child) in
      turn_clockwise ~names left_name left_assoc left_level ll_child new_right)
    else Node (op_name, left_child, fix_associativity ~names right_child)
  | Leaf _ -> Node (op_name, left_child, fix_associativity ~names right_child)

and handle_non_assoc ~names parent_level = function
  | Btree.Node (op_name, left_child, right_child) ->
    let op_assoc, op_level = Name_bindings.find_fixity names op_name in
    if Fixity.Level.(op_level = parent_level)
    then Type_bindings.type_error_msg "Associativity error"
    else handle_node ~names op_name op_assoc op_level left_child right_child
  | Leaf _ as leaf -> leaf
;;

let rec to_untyped_expr = function
  | Btree.Leaf expr -> expr
  | Node (op_name, left_child, right_child) ->
    let left_arg, right_arg = to_untyped_expr left_child, to_untyped_expr right_child in
    Untyped.Expr.Fun_call (Name op_name, [ left_arg; right_arg ])
;;

let to_untyped_expr ~names =
  fix_precedence ~names >> fix_associativity ~names >> to_untyped_expr
;;
