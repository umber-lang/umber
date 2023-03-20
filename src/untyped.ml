open Import
open Names

(* TODO: Make this a CST, with it precisely representing the syntax written.
   This should involving adding spans (Node-ifying the tree). *)

module Pattern = struct
  include Pattern

  type nonrec t = Type.Scheme.Bounded.t t [@@deriving sexp]
end

module Expr = struct
  type t =
    | Literal of Literal.t
    | Name of Value_name.Relative.t
    | Qualified of Module_path.t * t
    | Fun_call of t * t Nonempty.t
    | Op_tree of (Value_name.Relative.t, t) Btree.t
    | Lambda of Pattern.t Nonempty.t * t
    | If of t * t * t
    | Match of t * (Pattern.t * t) Nonempty.t
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t list
    | Seq_literal of t list
    | Record_literal of (Value_name.t * t option) Nonempty.t
    | Record_update of t * (Value_name.t * t option) Nonempty.t
    | Record_field_access of t * Value_name.t
    | Type_annotation of t * Type.Scheme.Bounded.t
  [@@deriving sexp, variants]

  (** Get all the external names referenced by an expression. Names local to the
      expression (e.g. those bound by match expressions or lambdas) are not included. *)
  let names_used ~names =
    let add_locals init = Pattern.Names.fold ~init ~f:Set.add in
    let rec loop ~names used locals = function
      | Literal _ -> used
      | Name ([], name) when Set.mem locals name -> used
      | Name name -> Set.add used (Name_bindings.absolutify_value_name names name)
      | Qualified (path, expr) ->
        loop ~names:(Name_bindings.import_all names path) used locals expr
      | Fun_call (fun_, args) ->
        let used = loop ~names used locals fun_ in
        Nonempty.fold args ~init:used ~f:(fun used expr -> loop ~names used locals expr)
      | Op_tree tree ->
        let rec tree_loop acc = function
          | Btree.Node (op_name, left_child, right_child) ->
            tree_loop (tree_loop (Set.add used op_name) left_child) right_child
          | Leaf expr -> loop ~names acc locals expr
        in
        tree_loop used tree
      | Lambda (args, body) ->
        loop ~names used (Nonempty.fold args ~init:locals ~f:add_locals) body
      | If (cond, then_, else_) ->
        loop ~names (loop ~names (loop ~names used locals cond) locals then_) locals else_
      | Match (expr, branches) ->
        let used = loop ~names used locals expr in
        Nonempty.fold branches ~init:used ~f:(fun used (pat, branch) ->
          loop ~names used (add_locals locals pat) branch)
      | Let { rec_; bindings; body } ->
        let new_locals =
          Nonempty.fold bindings ~init:locals ~f:(fun locals binding ->
            Node.with_value binding ~f:(fun (pat, _) -> add_locals locals pat))
        in
        let binding_locals = if rec_ then new_locals else locals in
        let used =
          Nonempty.fold bindings ~init:used ~f:(fun used binding ->
            Node.with_value binding ~f:(fun (_, expr) ->
              loop ~names used binding_locals expr))
        in
        loop ~names used new_locals body
      | Tuple items | Seq_literal items ->
        List.fold items ~init:used ~f:(fun used -> loop ~names used locals)
      | Record_literal fields -> loop_record_fields ~names used locals fields
      | Record_update (expr, fields) ->
        loop_record_fields ~names (loop ~names used locals expr) locals fields
      | Record_field_access (expr, _) | Type_annotation (expr, _) ->
        loop ~names used locals expr
    and loop_record_fields ~names used locals =
      Nonempty.fold ~init:used ~f:(fun used -> function
        | _, Some expr -> loop ~names used locals expr
        | _, None -> used)
    in
    loop ~names Value_name.Relative.Set.empty Value_name.Set.empty
  ;;

  let match_function branches =
    let name = Value_name.empty in
    Lambda ([ Catch_all (Some name) ], Match (Name ([], name), branches))
  ;;

  let qualified (path, expr) =
    match path with
    | [] -> expr
    | _ -> Qualified (Module_path.of_ustrings_unchecked path, expr)
  ;;

  let op_section_right op expr =
    let op = Value_name.Relative.of_ustrings_unchecked op in
    let left_var = Value_name.empty in
    let left_var_qualified = Value_name.Relative.with_path [] left_var in
    Lambda
      ( [ Catch_all (Some left_var) ]
      , Fun_call (Name op, [ Name left_var_qualified; expr ]) )
  ;;

  let op_section_left expr op =
    Fun_call (Name (Value_name.Relative.of_ustrings_unchecked op), [ expr ])
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.t) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.t) def [@@deriving sexp_of]
end
