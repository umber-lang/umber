open Import
open Names

(* TODO: Make this a CST, with it precisely representing the syntax written (including
   comments). This will be helpful for writing an auto-formatter. *)

module Pattern = struct
  include Pattern

  type nonrec t = (Module_path.relative Type_scheme.t, Module_path.relative) t
  [@@deriving equal, sexp]
end

module Effect_pattern = struct
  include Effect_pattern

  type nonrec t = (Module_path.relative Type_scheme.t, Module_path.relative) t
  [@@deriving equal, sexp]
end

module Expr = struct
  type t =
    | Literal of Literal.t
    | Name of Value_name.Relative.t
    | Qualified of Module_path.Relative.t * t Node.t
    | Fun_call of t Node.t * t Node.t Nonempty.t
    | Op_tree of (Value_name.Relative.t Node.t, t Node.t) Btree.t
    | Op_section of
        { op_side : [ `Left | `Right ]
        ; op : Value_name.Relative.t Node.t
        ; expr : t Node.t
        }
    | Lambda of Pattern.t Node.t Nonempty.t * t Node.t
    | If of t Node.t * t Node.t * t Node.t
    | Match of t Node.t * (Pattern.t Node.t * t Node.t) Nonempty.t
    | Match_function of (Pattern.t Node.t * t Node.t) Nonempty.t
    | Handle of
        t Node.t
        * ([ `Effect of Effect_pattern.t | `Value of Pattern.t ] Node.t * t Node.t)
          Nonempty.t
    (* TODO: Support fixity declarations on local let bindings *)
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t Node.t list
    | Seq_literal of t Node.t list
    | Record_literal of (Value_name.t * t Node.t option) Nonempty.t
    | Record_update of t Node.t * (Value_name.t * t Node.t option) Nonempty.t
    | Record_field_access of t Node.t * Value_name.t Node.t
    | Type_annotation of t Node.t * Module_path.relative Type_scheme.t Node.t
  [@@deriving equal, sexp, variants]

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
      | Op_section { op_side = _; op; expr } ->
        let used = Node.with_value op ~f:(fun op -> loop ~names used locals (Name op)) in
        Node.with_value expr ~f:(loop ~names used locals)
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
      | Match_function branches ->
        Nonempty.fold branches ~init:used ~f:(fun used (pat, branch) ->
          let locals = Node.with_value pat ~f:(add_locals locals) in
          Node.with_value branch ~f:(loop ~names used locals))
      | Handle (expr, branches) ->
        let used = Node.with_value expr ~f:(loop ~names used locals) in
        Nonempty.fold branches ~init:used ~f:(fun used (effect_pattern, branch) ->
          let locals =
            Node.with_value effect_pattern ~f:(function
              | `Effect { operation = _; args } ->
                let locals = Nonempty.fold args ~init:locals ~f:add_locals in
                Set.add locals Value_name.resume_keyword
              | `Value pattern -> add_locals locals pattern)
          in
          Node.with_value branch ~f:(loop ~names used locals))
      | Let { rec_; bindings; body } ->
        let new_locals =
          Nonempty.fold
            bindings
            ~init:locals
            ~f:(fun locals (pat, (_ : Fixity.t option), _expr) ->
            Node.with_value pat ~f:(add_locals locals))
        in
        let binding_locals = if rec_ then new_locals else locals in
        let used =
          Nonempty.fold bindings ~init:used ~f:(fun used (_pat, _fixity, expr) ->
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

  let qualified path expr =
    match path with
    | [] -> Node.with_value expr ~f:Fn.id
    | _ :: _ -> Qualified (Module_path.Relative.of_ustrings_unchecked path, expr)
  ;;
end

let create_effect_operation sig_ : _ Effect.Operation.t =
  match (sig_ : _ Module.sig_) with
  | Common_sig (Val (_, Some _, _)) ->
    Compilation_error.raise
      Other
      ~msg:[%message "Fixity declarations are not supported on effect operations"]
  | Common_sig (Val (name, None, ((scheme, constraints) as type_))) ->
    if not (List.is_empty constraints)
    then failwith "TODO: constraints on effect operations";
    (match scheme with
     | Function (args, Effect_union [], result) -> { name; args; result }
     | Function (_, _, _) ->
       Compilation_error.raise
         Other
         ~msg:
           [%message "Effect operations can't perform effects" (type_ : _ Type_scheme.t)]
     | Var _ | Type_app _ | Tuple _ | Union _ | Intersection _ ->
       Compilation_error.raise
         Other
         ~msg:[%message "Effect operations must be functions" (type_ : _ Type_scheme.t)])
  | Module_sig _ | Common_sig (Extern _ | Type_decl _ | Effect _ | Trait_sig _ | Import _)
    ->
    Compilation_error.raise
      Other
      ~msg:
        [%message
          [%string "This kind of statement is not allowed inside an effect declaration"]
            (sig_ : _ Module.sig_)]
;;

let create_effect params sigs : _ Effect.t =
  let operations =
    Option.map sigs ~f:(List.map ~f:(Node.with_value ~f:create_effect_operation))
  in
  { params; operations }
;;

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.t, Module_path.relative) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.t, Module_path.relative) def [@@deriving sexp_of]
end
