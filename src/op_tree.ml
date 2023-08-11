open! Import
open Names
open Fixity.Level.O

type t = (Value_name.Relative.t Node.t, Untyped.Expr.t Node.t) Btree.t
[@@deriving equal, sexp_of]

let rec fix_precedence ~names (t : t) : t =
  match t with
  | Leaf _ as leaf -> leaf
  | Node (op_name, left_child, right_child) ->
    let left_child = fix_precedence ~names left_child in
    let right_child = fix_precedence ~names right_child in
    let _, op_level = Node.with_value op_name ~f:(Name_bindings.find_fixity names) in
    let rotation =
      match left_child, right_child with
      | Leaf _, Leaf _ -> None
      | Node (left_name, ll_child, lr_child), Leaf _ ->
        let _, left_level =
          Node.with_value left_name ~f:(Name_bindings.find_fixity names)
        in
        if left_level < op_level
        then Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
        else None
      | Leaf _, Node (right_name, rl_child, rr_child) ->
        let _, right_level =
          Node.with_value right_name ~f:(Name_bindings.find_fixity names)
        in
        if right_level < op_level
        then Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
        else None
      | Node (left_name, ll_child, lr_child), Node (right_name, rl_child, rr_child) ->
        let _, left_level =
          Node.with_value left_name ~f:(Name_bindings.find_fixity names)
        in
        let _, right_level =
          Node.with_value right_name ~f:(Name_bindings.find_fixity names)
        in
        if left_level <= right_level && left_level < op_level
        then Some (left_level, (`Clockwise, left_name, ll_child, lr_child))
        else if right_level < op_level
        then Some (right_level, (`Anticlockwise, right_name, rl_child, rr_child))
        else None
    in
    (match rotation with
     | None -> Node (op_name, left_child, right_child)
     | Some (level, rotation_info) ->
       let _, op_level = Node.with_value op_name ~f:(Name_bindings.find_fixity names) in
       if level < op_level
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
       else Node (op_name, left_child, right_child))
;;

let can_rotate op_level op_assoc child_level child_assoc =
  child_level = op_level && Fixity.Assoc.compatible child_assoc op_assoc
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
    Node
      (op_name, fix_associativity ~names left_child, fix_associativity ~names right_child)

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
    else
      Node
        ( op_name
        , fix_associativity ~names left_child
        , fix_associativity ~names right_child )
  | Leaf _ ->
    Node
      (op_name, fix_associativity ~names left_child, fix_associativity ~names right_child)

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
    else
      Node
        ( op_name
        , fix_associativity ~names left_child
        , fix_associativity ~names right_child )
  | Leaf _ ->
    Node
      (op_name, fix_associativity ~names left_child, fix_associativity ~names right_child)
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

(* The associativity of inorder-adjacent operators with the same precedence must be
   compatible (meaning either both left or both right). *)
let check_for_associativity_errors t ~names =
  ignore
    (Sequence.fold ~init:None (Btree.inorder_nodes t) ~f:(fun last_fixity op_name ->
       let op_assoc, op_level =
         Node.with_value op_name ~f:(Name_bindings.find_fixity names)
       in
       let span = Node.span op_name in
       (match last_fixity with
        | None -> ()
        | Some (last_span, last_assoc, last_level) ->
          if Fixity.Level.( = ) op_level last_level
             && not (Fixity.Assoc.compatible op_assoc last_assoc)
          then
            Compilation_error.raise
              Type_error
              ~span:(Span.combine last_span span)
              ~msg:[%message "Associativity error"]);
       Some (span, op_assoc, op_level))
      : (Span.t * Fixity.Assoc.t * Fixity.Level.t) option)
;;

let rec check_invariants (t : t) ~names =
  match t with
  | Leaf _ -> ()
  | Node (op_name, left_child, right_child) ->
    let op_assoc, op_level =
      Node.with_value op_name ~f:(Name_bindings.find_fixity names)
    in
    let upholds_invariant =
      let check_left_or_right ~opposite_side_child =
        match opposite_side_child with
        | None -> true
        | Some (child_op_assoc, child_op_level) ->
          op_level < child_op_level
          || (op_level = child_op_level
              && not (Fixity.Assoc.equal op_assoc child_op_assoc))
      in
      let convert_child (child : t) =
        match child with
        | Leaf _ -> None
        | Node (child_op_name, _, _) ->
          Some (Node.with_value child_op_name ~f:(Name_bindings.find_fixity names))
      in
      let left_child = convert_child left_child in
      let right_child = convert_child right_child in
      match op_assoc with
      | Non_assoc -> true
      | Left -> check_left_or_right ~opposite_side_child:right_child
      | Right -> check_left_or_right ~opposite_side_child:left_child
    in
    if not upholds_invariant
    then compiler_bug [%message "Operator precedence tree violates invariants" (t : t)];
    check_invariants left_child ~names;
    check_invariants right_child ~names
;;

let fix_precedence_and_associativity t ~names =
  check_for_associativity_errors t ~names;
  let t' = fix_precedence t ~names |> fix_associativity ~names in
  check_invariants t' ~names;
  t'
;;

let to_untyped_expr ~names t = to_untyped_expr (fix_precedence_and_associativity t ~names)

let%test_module _ =
  (module struct
    let operators =
      let dummy_type = Internal_type.fresh_var () in
      List.map Fixity.all ~f:(fun fixity ->
        let op_name =
          Value_name.of_string_unchecked (Sexp.to_string [%sexp (fixity : Fixity.t)])
        in
        ( op_name
        , Name_bindings.Name_entry.create ~fixity ~type_source:Let_inferred dummy_type ))
      |> Value_name.Map.of_alist_exn
    ;;

    let names = Name_bindings.For_testing.create ~names:operators

    let op_name_generator =
      Quickcheck.Generator.of_list
        (Map.keys operators
         |> List.map ~f:(fun name -> Node.dummy_span (Module_path.Relative.empty, name)))
    ;;

    let dummy_expr_generator =
      Quickcheck.Generator.return (Node.dummy_span (Untyped.Expr.Literal (Int 0)))
    ;;

    let generator : t Quickcheck.Generator.t =
      [%quickcheck.generator:
        ([%custom op_name_generator], [%custom dummy_expr_generator]) Btree.t]
    ;;

    let shrinker : t Quickcheck.Shrinker.t =
      [%quickcheck.shrinker:
        ( [%custom Quickcheck.Shrinker.empty ()]
        , [%custom Quickcheck.Shrinker.empty ()] )
        Btree.t]
    ;;

    let%expect_test _ =
      Quickcheck.test generator ~sexp_of:sexp_of_t ~shrinker ~f:(fun t ->
        match fix_precedence_and_associativity t ~names with
        | t' ->
          if not (Int.equal (Btree.length t) (Btree.length t'))
          then
            compiler_bug
              [%message
                "Operator tree has a different number of nodes"
                  ~input_length:(Btree.length t : int)
                  ~output_length:(Btree.length t' : int)
                  ~input:(t : t)
                  ~output:(t' : t)];
          let t'' = fix_precedence_and_associativity t' ~names in
          if not (equal t' t'')
          then
            compiler_bug
              [%message
                "Fixing the operator tree is not idempotent"
                  ~input:(t : t)
                  ~first_output:(t' : t)
                  ~second_output:(t'' : t)]
        | exception Compilation_error.Compilation_error { kind = Type_error; _ } -> ())
    ;;
  end)
;;
