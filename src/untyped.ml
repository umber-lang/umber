open Import
open Names

(* TODO: Make this a CST, with it precisely representing the syntax written.
   This should involving adding spans (Node-ifying the tree). *)

module Pattern = struct
  include Pattern

  type nonrec t = (Module_path.relative Type.Scheme.Bounded.t, Module_path.relative) t
  [@@deriving equal, sexp]
end

module Expr = struct
  type t =
    | Literal of Literal.t
    | Name of Value_name.Relative.t
    | Qualified of Module_path.Relative.t * t Node.t
    | Fun_call of t Node.t * t Node.t Nonempty.t
    | Op_tree of (Value_name.Relative.t Node.t, t Node.t) Btree.t
    | Lambda of Pattern.t Node.t Nonempty.t * t Node.t
    | If of t Node.t * t Node.t * t Node.t
    | Match of t Node.t * (Pattern.t Node.t * t Node.t) Nonempty.t
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t Node.t list
    | Seq_literal of t Node.t list
    | Record_literal of (Value_name.t * t Node.t option) Nonempty.t
    | Record_update of t Node.t * (Value_name.t * t Node.t option) Nonempty.t
    | Record_field_access of t Node.t * Value_name.t Node.t
    | Type_annotation of t Node.t * Module_path.relative Type.Scheme.Bounded.t Node.t
  [@@deriving equal, sexp, variants]

  (** Get all the external names referenced by an expression. Names local to the
      expression (e.g. those bound by match expressions or lambdas) are not included. *)
  let names_used ~names =
    let add_locals init = Pattern.Names.fold ~init ~f:Set.add in
    let rec loop ~names used locals = function
      | Literal _ -> used
      | Name ((path, name) as name') ->
        if Module_path.is_empty path && Set.mem locals name
        then used
        else Set.add used (Name_bindings.absolutify_value_name names name')
      | Qualified (path, expr) ->
        let names = Name_bindings.import_all names path in
        Node.with_value expr ~f:(loop ~names used locals)
      | Fun_call (fun_, args) ->
        let used = Node.with_value fun_ ~f:(fun fun_ -> loop ~names used locals fun_) in
        Nonempty.fold args ~init:used ~f:(fun used expr ->
          Node.with_value expr ~f:(loop ~names used locals))
      | Op_tree tree ->
        let rec tree_loop acc = function
          | Btree.Node (op_name, left_child, right_child) ->
            let op_name =
              Node.with_value op_name ~f:(Name_bindings.absolutify_value_name names)
            in
            tree_loop (tree_loop (Set.add used op_name) left_child) right_child
          | Leaf expr -> Node.with_value expr ~f:(loop ~names acc locals)
        in
        tree_loop used tree
      | Lambda (args, body) ->
        let locals =
          Nonempty.fold args ~init:locals ~f:(fun locals ->
            Node.with_value ~f:(add_locals locals))
        in
        Node.with_value body ~f:(loop ~names used locals)
      | If (cond, then_, else_) ->
        let used = Node.with_value cond ~f:(loop ~names used locals) in
        let used = Node.with_value then_ ~f:(loop ~names used locals) in
        Node.with_value else_ ~f:(loop ~names used locals)
      | Match (expr, branches) ->
        let used = Node.with_value expr ~f:(loop ~names used locals) in
        Nonempty.fold branches ~init:used ~f:(fun used (pat, branch) ->
          let locals = Node.with_value pat ~f:(add_locals locals) in
          Node.with_value branch ~f:(loop ~names used locals))
      | Let { rec_; bindings; body } ->
        let new_locals =
          Nonempty.fold bindings ~init:locals ~f:(fun locals (pat, _) ->
            Node.with_value pat ~f:(add_locals locals))
        in
        let binding_locals = if rec_ then new_locals else locals in
        let used =
          Nonempty.fold bindings ~init:used ~f:(fun used (_, expr) ->
            Node.with_value expr ~f:(loop ~names used binding_locals))
        in
        Node.with_value body ~f:(loop ~names used new_locals)
      | Tuple items | Seq_literal items ->
        List.fold items ~init:used ~f:(fun used ->
          Node.with_value ~f:(loop ~names used locals))
      | Record_literal fields -> loop_record_fields ~names used locals fields
      | Record_update (expr, fields) ->
        let used = Node.with_value expr ~f:(loop ~names used locals) in
        loop_record_fields ~names used locals fields
      | Record_field_access (expr, _) | Type_annotation (expr, _) ->
        Node.with_value expr ~f:(loop ~names used locals)
    and loop_record_fields ~names used locals =
      Nonempty.fold ~init:used ~f:(fun used ((_field_name : Value_name.t), expr) ->
        match expr with
        | Some expr -> Node.with_value expr ~f:(loop ~names used locals)
        | None -> used)
    in
    loop ~names Value_name.Absolute.Set.empty Value_name.Set.empty
  ;;

  let match_function
    ~match_keyword_span
    ~branches_span
    (branches : (Pattern.t Node.t * t Node.t) Nonempty.t)
    : t
    =
    let name = Value_name.empty in
    Lambda
      ( [ Node.create (Catch_all (Some name) : Pattern.t) match_keyword_span ]
      , Node.create
          (Match
             ( Node.create (Name (Module_path.Relative.empty, name)) match_keyword_span
             , branches ))
          branches_span )
  ;;

  let qualified path expr =
    match path with
    | [] -> Node.with_value expr ~f:Fn.id
    | _ :: _ -> Qualified (Module_path.Relative.of_ustrings_unchecked path, expr)
  ;;

  (* FIXME: This doesn't have the expected evaluation semantics. It needs to evaluate the
     argument, then construct the lambda. *)
  let op_section_right op expr =
    let op_span = Node.span op in
    let left_var = Value_name.empty in
    let left_var_qualified =
      Value_name.Relative.with_path Module_path.Relative.empty left_var
    in
    Lambda
      ( [ Node.create (Catch_all (Some left_var) : Pattern.t) op_span ]
      , Node.create
          (Fun_call
             ( Node.map op ~f:(name << Value_name.Relative.of_ustrings_unchecked)
             , [ Node.create (Name left_var_qualified) op_span; expr ] ))
          (Node.span expr) )
  ;;

  (* FIXME: This will interact strangely with functions of unknown arity. The type
     inference will assume they have arity 1. It should probably work similarly to
     `op_section_right` and explicitly construct a lambda, rather than relying on 
     the somewhat anemic partial application we currently have. *)
  let op_section_left expr op =
    Fun_call (Node.map op ~f:(name << Value_name.Relative.of_ustrings_unchecked), [ expr ])
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.t, Module_path.relative) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.t, Module_path.relative) def [@@deriving sexp_of]
end
