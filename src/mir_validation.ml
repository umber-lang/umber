open! Import
open Names

module State : sig
  type t

  val add : t -> Mir_name.t -> Mir_type.t -> t
  val find : t -> Mir_name.t -> Mir_type.t
  val update : t -> Mir_name.t -> Mir_type.t -> t
end = struct
  type t = Mir_type.t Mir_name.Map.t

  (* TODO: Add proper error messages instead of relying on [Map.add_exn], etc. *)
  let add t name type_ = Map.add_exn t ~key:name ~data:type_
  let find = Map.find_exn

  let update t name type_ =
    if Map.mem t name
    then Map.set t ~key:name ~data:type_
    else
      compiler_bug
        [%message
          "Could not find name to update type of"
            (name : Mir_name.t)
            ~refined_type:(type_ : Mir_type.t)]
  ;;
end

let rec refine_expr_type state expr ~refined_type =
  (* TODO: Make this smarter, to follow more names through expressions.
     Might want to consider a unification style instead. *)
  match (expr : Mir.Expr.t) with
  | Name name -> State.update state name refined_type
  | Primitive _ | Fun_call _ | Make_block _ | Get_block_field _ -> state
  | Let (_name, _expr, body) -> refine_expr_type state body ~refined_type
  | Cond_assign { vars = _; conds = _; body; if_none_matched } ->
    let state = refine_expr_type state body ~refined_type in
    (match if_none_matched with
     | Use_bindings _ -> state
     | Otherwise otherwise_expr -> refine_expr_type state otherwise_expr ~refined_type)
;;

let rec type_of_expr state expr : Mir_type.t =
  match (expr : Mir.Expr.t) with
  | Name name -> State.find state name
  | Primitive literal -> Primitive (Mir_type.Primitive.of_literal literal)
  | Let (name, expr, body) ->
    let state = State.add state name (type_of_expr state expr) in
    type_of_expr state body
  | Fun_call (fun_name, args) ->
    (* FIXME: We should be removing all partial applications before this point, which I'm
       almost certain is not happening. *)
    let arg_types, return_type = Mir_type.expect_function (State.find state fun_name) in
    Nonempty.iter2_exn args arg_types ~f:(fun (arg, arg_type) inferred_arg_type ->
      Mir_type.assert_equal arg_type inferred_arg_type;
      Mir_type.assert_equal arg_type (type_of_expr state arg));
    return_type
  | Make_block { tag; fields } -> Block [ tag, List.map fields ~f:(type_of_expr state) ]
  | Get_block_field (index, expr) ->
    (match Mir_type.expect_block (type_of_expr state expr) with
     | [ ((_ : Cnstr_tag.t), fields) ] ->
       (match List.nth fields (Block_index.to_int index) with
        | Some type_ -> type_
        | None ->
          compiler_bug
            [%message
              "Out of bounds block index"
                (index : Block_index.t)
                (fields : Mir_type.t list)
                (expr : Mir.Expr.t)])
     | _ :: _ :: _ as variants ->
       compiler_bug
         [%message
           "Multiple possible variants for block in Get_block_field"
             (index : Block_index.t)
             (variants : (Cnstr_tag.t * Mir_type.t list) Nonempty.t)
             (expr : Mir.Expr.t)])
  | Cond_assign { vars; conds; body; if_none_matched } ->
    let state_after_conds, var_types_per_cond =
      Nonempty.fold_map conds ~init:state ~f:(fun state (cond, var_exprs) ->
        (* FIXME: update state (e.g. blocks) with cond *)
        let state = update_types_with_cond state cond in
        state, List.map var_exprs ~f:(type_of_expr state))
    in
    let check_var_types_per_cond ~var_types_per_cond:(first :: rest : _ Nonempty.t) =
      assert_or_compiler_bug ~here:[%here] (List.length first = List.length vars);
      if not (List.for_all rest ~f:([%equal: Mir_type.t list] first))
      then
        compiler_bug
          [%message
            "Unequal variable types in Cond_assign"
              (var_types_per_cond : Mir_type.t list Nonempty.t)
              (expr : Mir.Expr.t)]
    in
    (match if_none_matched with
     | Use_bindings var_exprs ->
       let var_types_per_cond =
         Nonempty.cons (List.map var_exprs ~f:(type_of_expr state)) var_types_per_cond
       in
       (* FIXME: update state with cond here too? negation? *)
       check_var_types_per_cond ~var_types_per_cond;
       type_of_expr state_after_conds body
     | Otherwise otherwise_expr ->
       (* FIXME: maybe you have the negated state here? Or maybe you don't need it? *)
       type_of_expr state otherwise_expr)

and update_types_with_cond state cond =
  let check_variants ~expr ~tag ~is_constant =
    let variants = Mir_type.expect_block (type_of_expr state expr) in
    let condition = if is_constant then List.is_empty else Fn.non List.is_empty in
    let valid_variants =
      Nonempty.filter variants ~f:(fun (tag', fields) ->
        Cnstr_tag.equal tag tag' && condition fields)
    in
    match Nonempty.of_list valid_variants with
    | None ->
      compiler_bug
        [%message
          "No valid variants from Constant_tag_equals"
            (tag : Cnstr_tag.t)
            (variants : (Cnstr_tag.t * Mir_type.t list) Nonempty.t)
            (cond : Mir.Expr.cond)]
    | Some valid_variants ->
      refine_expr_type state expr ~refined_type:(Block valid_variants)
  in
  match (cond : Mir.Expr.cond) with
  | Equals (expr, literal) ->
    Mir_type.assert_equal
      (type_of_expr state expr)
      (Primitive (Mir_type.Primitive.of_literal literal));
    state
  | Constant_tag_equals (expr, tag) -> check_variants ~expr ~tag ~is_constant:true
  | Non_constant_tag_equals (expr, tag) -> check_variants ~expr ~tag ~is_constant:false
  | And _ -> failwith "TODO: And in conditions"
;;

let validate_stmt state stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) -> State.add state name (type_of_expr state expr)
  | Fun_def { fun_name; closed_over; args; body } ->
    if not (Set.is_empty closed_over) then failwith "TODO: closures";
    let body_type =
      let state =
        Nonempty.fold args ~init:state ~f:(fun state (arg_name, arg_type) ->
          State.add state arg_name arg_type)
      in
      type_of_expr state body
    in
    State.add state fun_name body_type
  | Extern_decl _ ->
    (* FIXME: implement this *)
    state
;;
