open! Core
open! Import

type t =
  | Primitive of Literal.t
  | Name of Mir_name.t
  (* TODO: recursive lets? Mutual recursion? (will surely need a rec flag at least)
     Can maybe handle that in toplevel function definitions. *)
  | Let of Mir_name.t * t * t
  | Fun_call of Mir_name.t * t Nonempty.t
  | Make_block of
      { tag : Cnstr_tag.t
      ; fields : t list [@sexp.omit_nil]
      }
  | Get_block_field of Block_index.t * t
  | Cond_assign of
      { vars : Mir_name.t list [@sexp.omit_nil]
      ; conds : (cond * (t list[@sexp.omit_nil])) Nonempty.t
      ; body : t
      ; if_none_matched : cond_if_none_matched
      }
  | Handle_effects of
      { vars : (Mir_name.t * Mir_name.t) list
      ; value_handler : (Mir_name.t * t) option
      ; effect_handlers : effect_handler Nonempty.t
      ; expr : t
      }
  | Perform_effect of
      { effect_op : Effect_op_id.t
      ; args : t Nonempty.t
      }

and effect_handler =
  { effect_op : Effect_op_id.t
  ; args : Mir_name.t Nonempty.t
  ; resume : Mir_name.t
  ; handler : t
  }

and cond =
  (* TODO: In practice, the expressions in conditions have to be simple names. Maybe we
     should enforce that in the type here. *)
  | Equals of t * Literal.t
  | Constant_tag_equals of t * Cnstr_tag.t
  | Non_constant_tag_equals of t * Cnstr_tag.t
  | And of cond * cond

and cond_if_none_matched =
  | Otherwise of t
  | Use_bindings of t list
[@@deriving sexp_of]

let rec map t ~f =
  match (f t : _ Map_action.t) with
  | Halt t -> t
  | Retry t -> map t ~f
  | Defer t ->
    (match t with
     | Name _ | Primitive _ -> t
     | Let (name, t, t') -> Let (name, map t ~f, map t' ~f)
     | Fun_call (fun_name, args) -> Fun_call (fun_name, Nonempty.map args ~f:(map ~f))
     | Make_block { tag; fields } ->
       Make_block { tag; fields = List.map fields ~f:(map ~f) }
     | Get_block_field (index, t) -> Get_block_field (index, map t ~f)
     | Cond_assign { vars; conds; body; if_none_matched } ->
       Cond_assign
         { vars
         ; conds = Nonempty.map conds ~f:(Tuple2.map_snd ~f:(List.map ~f:(map ~f)))
         ; body = map body ~f
         ; if_none_matched =
             (match if_none_matched with
              | Otherwise t -> Otherwise (map t ~f)
              | Use_bindings bindings -> Use_bindings (List.map bindings ~f:(map ~f)))
         }
     | Handle_effects { vars; value_handler; effect_handlers; expr } ->
       Handle_effects
         { vars
         ; value_handler =
             Option.map value_handler ~f:(fun (arg, expr) -> arg, map expr ~f)
         ; effect_handlers =
             Nonempty.map effect_handlers ~f:(fun { effect_op; args; resume; handler } ->
               { effect_op; args; resume; handler = map handler ~f })
         ; expr = map ~f expr
         }
     | Perform_effect { effect_op; args } ->
       Perform_effect { effect_op; args = Nonempty.map args ~f:(map ~f) })
;;

module Fun_def = struct
  type nonrec t =
    { fun_name : Mir_name.t
    ; args : Mir_name.t Nonempty.t
    ; body : t
    }
  [@@deriving sexp_of]
end

(** Atomic expressions are those that are syntactically guaranteed to have no
    side-effects, not even allocating memory. This allows it to be safely duplicated
    when processing the AST. *)
let is_atomic : t -> bool = function
  | Primitive _ | Name _ | Make_block { tag = _; fields = [] } | Get_block_field _ -> true
  | Fun_call _ | Let _
  | Make_block { tag = _; fields = _ :: _ }
  | Cond_assign _ | Handle_effects _ | Perform_effect _ -> false
;;

(* TODO: consider merging making bindings with making conditions. If we extended
   [Coverage.t] to include some notion of what conditions have been tested as well, it
   could help us avoid unnecessary checks. *)

(** [fold_pattern_bindings] folds over the variable bindings formed by a pattern
    associated with an expression. The expression passed in should be atomic as it will
    be duplicated when generating bindings. *)
let fold_pattern_bindings =
  let rec loop ~ctx ~add_let ~add_name acc pat mir_expr type_ =
    match (pat : Simple_pattern.t) with
    | Catch_all None -> ctx, acc
    | Catch_all (Some name) ->
      let ctx, name = add_name ctx name in
      ctx, add_let acc name mir_expr
    | As (pattern, name) ->
      let ctx, name = add_name ctx name in
      let acc = add_let acc name mir_expr in
      loop ~ctx ~add_let ~add_name acc pattern (Name name) type_
    | Cnstr_appl (_cnstr, args) ->
      List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) (arg, arg_type) ->
        let arg_index = Block_index.of_int i in
        let arg_expr = Get_block_field (arg_index, mir_expr) in
        loop ~ctx ~add_let ~add_name acc arg arg_expr arg_type)
    | Constant _ -> ctx, acc
  in
  fun ~ctx ~init:acc ~add_name ~add_let pat mir_expr type_ ->
    loop ~ctx ~add_let ~add_name acc pat mir_expr type_
;;

let make_atomic ~ctx ~default ~add_name ~add_let ~binding_name mir_expr =
  if is_atomic mir_expr
  then ctx, default, mir_expr
  else (
    let ctx_for_body, expr_name = add_name ctx binding_name in
    let acc = add_let expr_name mir_expr in
    ctx_for_body, acc, Name expr_name)
;;

let rec condition_of_pattern ~ctx ~input_expr ~input_type pattern =
  match (pattern : Simple_pattern.t) with
  | Catch_all _ -> None
  | As (pattern, _) -> condition_of_pattern ~ctx ~input_expr ~input_type pattern
  | Constant lit -> Some (Equals (input_expr, lit))
  | Cnstr_appl (cnstr, args) ->
    let cnstr_info = Context.find_cnstr_info ctx input_type in
    let tag = Cnstr_info.tag cnstr_info cnstr in
    if List.is_empty args
    then Some (Constant_tag_equals (input_expr, tag))
    else (
      (* TODO: We don't need to generate tag checks when there's only one possible tag
         e.g. for tuples or records. *)
      let tag_cond = Non_constant_tag_equals (input_expr, tag) in
      let conds =
        List.filter_mapi args ~f:(fun i (arg, arg_type) ->
          let arg_index = Block_index.of_int i in
          let arg_expr = Get_block_field (arg_index, input_expr) in
          condition_of_pattern ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
      in
      Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond'))))
;;

(* TODO: switch statement optimization
   See:
   - https://github.com/ocaml/ocaml/blob/trunk/lambda/matching.ml
   - https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching  *)

module Just_bound = struct
  type t =
    | Rec of
        { this_name : Mir_name.t
        ; other_names : Mir_name.Set.t
        }
    | Nonrec of { this_pattern_names : Mir_name.Set.t }
  [@@deriving sexp_of]
end

let add_let_bindings ~bindings ~body =
  List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
    match mir_expr with
    | Name name' when Mir_name.equal name name' ->
      (* TODO: Consider getting rid of this name eliding logic. We do it to give
         functions in MIR more readable names, and to avoid extra indirection from
         defining a function with a name like `Foo.*fun.1` and then having to define
         a value `Foo.foo.1` and a function `Foo.foo` to call it. There's got to be a
         better way than passing down the names we bind and them sometimes returning one
         of them back to elide the let binding though. The current setup is quite
         fragile and hard to maintain. *)
      (* Avoid genereating code that looks like let x.0 = x.0 *)
      body
    | _ -> Let (name, mir_expr, body))
;;

let rec check_rec_binding_expr expr =
  match (expr : _ Typed_ast.Expr.t) with
  | Lambda _ -> ()
  | Let { body; bindings = _; rec_ = _ } -> Node.with_value body ~f:check_rec_binding_expr
  | Match (_, _, arms) ->
    Nonempty.iter arms ~f:(fun (_, expr) ->
      Node.with_value expr ~f:check_rec_binding_expr)
  | Handle _
  | Name _
  | Tuple _
  | Record_literal _
  | Fun_call _
  | Record_update _
  | Record_field_access _ ->
    (* TODO: Consider relaxing this to allow more kinds of expressions e.g. function
       calls which don't mention the recursive names.
       See: https://v2.ocaml.org/manual/letrecvalues.html *)
    Compilation_error.raise
      Mir_error
      ~msg:
        [%message
          "Recursive let bindings can only be used to define functions"
            (expr : _ Typed_ast.Expr.t)]
  | Literal _ ->
    compiler_bug
      [%message "Impossible expr in recursive binding" (expr : _ Typed_ast.Expr.t)]
;;

let check_rec_binding_pattern pat =
  match (pat : Typed_ast.Pattern.generalized) with
  | Catch_all _ -> ()
  | _ ->
    Compilation_error.raise
      Mir_error
      ~msg:
        [%message
          "Only variables are allowed as the left-hand side of recursive let bindings"
            (pat : Typed_ast.Pattern.generalized)]
;;

let generate_let_bindings
  ~ctx
  ~ctx_for_body
  ~rec_
  ~init
  ~add_let
  ~extract_binding
  ~process_expr
  bindings
  =
  let all_names_bound =
    lazy
      (Nonempty.fold
         bindings
         ~init:Mir_name.Set.empty
         ~f:(fun all_names_bound (_, names_bound) ->
         Set.union all_names_bound names_bound))
  in
  let ctx = if rec_ then ctx_for_body else ctx in
  (* TODO: Warn about let bindings which bind no names and are pure. *)
  Nonempty.fold
    bindings
    ~init:(ctx_for_body, init)
    ~f:(fun (ctx_for_body, acc) (binding, names_bound) ->
    let pat, expr, ((typ, _constraints) : _ Type_scheme.t) = extract_binding binding in
    let just_bound : Just_bound.t =
      if rec_
      then (
        Node.with_value expr ~f:check_rec_binding_expr;
        Node.with_value pat ~f:check_rec_binding_pattern;
        assert_or_compiler_bug ~here:[%here] (Set.length names_bound = 1);
        let this_name = Set.choose_exn names_bound in
        Rec { this_name; other_names = Set.remove (force all_names_bound) this_name })
      else Nonrec { this_pattern_names = names_bound }
    in
    (* TODO: support unions in let bindings. For the non-rec case we should
       just be able to convert to a match *)
    let pat' =
      Node.with_value pat ~f:(fun pat ->
        let pat' =
          Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"let bindings"
        in
        let missing_cases =
          Simple_pattern.Coverage.(of_pattern pat' |> missing_cases ~ctx ~input_type:typ)
        in
        if not (List.is_empty missing_cases)
        then
          Compilation_error.raise
            Mir_error
            ~msg:
              [%message
                "The pattern in this let binding is not exhaustive"
                  ~pattern:(pat : Typed_ast.Pattern.generalized)
                  (missing_cases : Simple_pattern.t list)];
        pat')
    in
    let acc, mir_expr = process_expr acc ~just_bound ~ctx expr typ in
    let add_name ctx name = ctx, Context.find_value_name_assert_local ctx name in
    let binding_name, add_binding_name =
      match pat' with
      | Catch_all (Some name) | As (_, name) -> name, add_name
      | Catch_all None -> Constant_names.underscore, Context.add_value_name
      | Constant _ | Cnstr_appl _ -> Constant_names.binding, Context.add_value_name
    in
    Node.with_value mir_expr ~f:(fun mir_expr ->
      let ctx_for_body, acc, mir_expr =
        make_atomic
          ~ctx:ctx_for_body
          ~default:acc
          ~add_let:(fun name mir_expr -> add_let acc name mir_expr typ)
          ~binding_name
          ~add_name:add_binding_name
          mir_expr
      in
      fold_pattern_bindings
        ~ctx:ctx_for_body
        pat'
        mir_expr
        typ
        ~init:acc
        ~add_let:(fun acc name mir_expr -> add_let acc name mir_expr typ)
        ~add_name))
;;

let try_rewriting_partial_application
  ~fun_
  ~(fun_type : _ Type_scheme.t)
  ~args_and_types
  ~current_path
  =
  match fst fun_type with
  | Type_app _ | Tuple _ | Var _ ->
    compiler_bug [%message "Non-funtion type in function call"]
  | Union _ | Intersection _ ->
    failwith "TODO: handle rewriting partial application for union and intersection types"
  | Function
      ( fun_arg_types
      , (_effects : _ Type_scheme.effects)
      , (_return_type : _ Type_scheme.type_) ) ->
    (match snd (Nonempty.zip fun_arg_types args_and_types) with
     | Same_length -> `Already_fully_applied
     | Right_trailing _ ->
       compiler_bug [%message "Over-application of arguments to function"]
     | Left_trailing unapplied_arg_types ->
       let fun_name = Constant_names.fun_ in
       let applied_args, bindings =
         Nonempty.mapi args_and_types ~f:(fun i (arg_expr, arg_type) ->
           let name = Constant_names.synthetic_arg i in
           let arg_pat_and_type =
             Node.dummy_span (Typed_ast.Pattern.Catch_all (Some name), arg_type)
           in
           let arg_name = Node.dummy_span (Typed_ast.Expr.Name (current_path, name)) in
           (arg_name, arg_type), (arg_pat_and_type, None, arg_expr))
         |> Nonempty.unzip
       in
       let bindings =
         Nonempty.cons
           ( Node.dummy_span (Typed_ast.Pattern.Catch_all (Some fun_name), fun_type)
           , None
           , fun_ )
           bindings
       in
       let n_applied_args = Nonempty.length applied_args in
       let unapplied_args, lambda_args =
         Nonempty.mapi unapplied_arg_types ~f:(fun i arg_type ->
           let i = i + n_applied_args in
           let name = Constant_names.synthetic_arg i in
           let arg_pattern = Node.dummy_span (Typed_ast.Pattern.Catch_all (Some name)) in
           let arg_name = Node.dummy_span (Typed_ast.Expr.Name (current_path, name)) in
           (* Setting zero constraints on [arg_type] is a bit dodgy, but we don't use
              the constraints so it's fine. *)
           (arg_name, (arg_type, [])), arg_pattern)
         |> Nonempty.unzip
       in
       let args = Nonempty.append applied_args unapplied_args in
       let expr_as_lambda : _ Typed_ast.Expr.t =
         Let
           { rec_ = false
           ; bindings
           ; body =
               Node.dummy_span
                 (Typed_ast.Expr.Lambda
                    ( lambda_args
                    , Node.dummy_span
                        (Typed_ast.Expr.Fun_call
                           ( Node.dummy_span (Typed_ast.Expr.Name (current_path, fun_name))
                           , fun_type
                           , args )) ))
           }
       in
       `Rewritten_partial_application expr_as_lambda)
;;

let process_argument ~ctx ~bindings ~arg ~arg_type =
  Node.with_value arg ~f:(fun arg ->
    let arg =
      Simple_pattern.flatten_typed_pattern_no_unions arg ~label:"argument patterns"
    in
    match arg with
    | Catch_all (Some arg_name) ->
      (* Special-case named catch-all patterns (the dominant case) to skip the
         [lambda_arg] step and use the name directly. *)
      let ctx, arg_name = Context.add_value_name ctx arg_name in
      (ctx, bindings), arg_name
    | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
      let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
      let add_let acc name mir_expr = (name, mir_expr) :: acc in
      let ctx, bindings =
        fold_pattern_bindings
          ~ctx
          arg
          (Name arg_name)
          arg_type
          ~init:bindings
          ~add_let
          ~add_name:Context.add_value_name
      in
      (ctx, bindings), arg_name)
;;

let process_arguments ~ctx ~args ~arg_types =
  Nonempty.zip_exn args arg_types
  |> Nonempty.fold_map ~init:(ctx, []) ~f:(fun (ctx, bindings) (arg, arg_type) ->
       process_argument ~ctx ~bindings ~arg ~arg_type)
;;

let of_typed_expr
  ~just_bound:outer_just_bound
  ~ctx:outer_ctx
  ~(add_fun_def : Fun_def.t -> unit)
  ~(add_fun_decl : Fun_decl.t -> unit)
  outer_expr
  outer_type
  =
  let add_fun_def, is_local_fun_def =
    let local_fun_defs = Mir_name.Hash_set.create () in
    ( (fun (fun_def : Fun_def.t) ->
        Hash_set.add local_fun_defs fun_def.fun_name;
        add_fun_def fun_def)
    , Hash_set.mem local_fun_defs )
  in
  let rec of_typed_expr ?just_bound ~ctx expr expr_type =
    match
      ( (expr : Module_path.absolute Type_scheme.t Typed_ast.Expr.t)
      , (expr_type : _ Type_scheme.type_) )
    with
    | Literal lit, _ -> Primitive lit
    | Name name, _ ->
      let name, name_kind = Context.find_value_name ctx name in
      (match name_kind with
       | Local | Effect_op _ -> Name name
       | External { arity } ->
         add_fun_decl { name; arity };
         Name name
       | Bool_intrinsic { tag } -> Make_block { tag; fields = [] })
    | Fun_call (fun_, fun_type, args_and_types), body_type ->
      (match
         try_rewriting_partial_application
           ~fun_
           ~fun_type
           ~args_and_types
           ~current_path:(Context.current_path ctx)
       with
       | `Rewritten_partial_application expr_as_lambda ->
         of_typed_expr ?just_bound ~ctx expr_as_lambda expr_type
       | `Already_fully_applied ->
         let fun_call fun_ =
           let arg_types = Nonempty.map ~f:snd args_and_types in
           let fun_type : _ Type_scheme.type_ =
             (* Effect information is not preserved here. This is ~fine because we don't
                  actually use it. *)
             Function (Nonempty.map arg_types ~f:fst, Effect_union [], body_type)
           in
           let fun_ = of_typed_expr ~ctx fun_ fun_type in
           let args =
             Nonempty.map args_and_types ~f:(fun (arg, arg_type) ->
               Node.with_value arg ~f:(fun arg -> of_typed_expr ~ctx arg (fst arg_type)))
           in
           match fun_ with
           | Name fun_name -> Fun_call (fun_name, args)
           | Let _
           | Fun_call _
           | Get_block_field _
           | Cond_assign _
           | Handle_effects _
           | Perform_effect _ ->
             let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
             Let (fun_name, fun_, Fun_call (fun_name, args))
           | Primitive _ | Make_block _ ->
             compiler_bug [%message "Invalid function expression" (fun_ : t)]
         in
         Node.with_value fun_ ~f:(fun fun_ ->
           match fun_ with
           | Name (_, name) ->
             (* TODO: I think there's no need for this special-casing actually: we can
                  just use the constructor functions directly. *)
             (* Special-case constructor applications to make use of [Make_block]. *)
             (match Value_name.to_cnstr_name name with
              | Ok cnstr_name ->
                let cnstr_info = Context.find_cnstr_info ctx expr_type in
                let tag = Cnstr_info.tag cnstr_info (Named cnstr_name) in
                let fields, field_types = List.unzip (Nonempty.to_list args_and_types) in
                let field_types = List.map field_types ~f:fst in
                make_block ~ctx ~tag ~fields ~field_types
              | Error _ -> fun_call fun_)
           | _ -> fun_call fun_))
    | Lambda (args, body), Function (arg_types, _effect_row, body_type) ->
      add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound
    | Match (expr, input_type, arms), output_type ->
      let input_expr =
        Node.with_value expr ~f:(fun expr -> of_typed_expr ~ctx expr (fst input_type))
      in
      if is_atomic input_expr
      then
        (* Skip binding [match_expr_name] when matching on an atomic expression. *)
        handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
      else (
        let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
        let body =
          let input_expr = Name match_expr_name in
          handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
        in
        Let (match_expr_name, input_expr, body))
    | ( Handle { expr = input_expr; expr_type = input_type; value_branch; effect_branches }
      , output_type ) ->
      (match Nonempty.of_list effect_branches with
       | None ->
         (* If there are no effect branches, we can just do a simple translation. *)
         let input_expr =
           match value_branch with
           | None ->
             (* This shouldn't actually be reachable - it implies a handle expression
                  with no branches at all. *)
             input_expr
           | Some (pat, value_branch_expr) ->
             (* Translate the value branch to a simple let binding. *)
             let span = Span.combine (Node.span pat) (Node.span value_branch_expr) in
             Node.create
               (Typed_ast.Expr.Let
                  { rec_ = false
                  ; bindings = [ pat, None, input_expr ]
                  ; body = value_branch_expr
                  })
               span
         in
         Node.with_value input_expr ~f:(fun expr ->
           of_typed_expr ~ctx expr (fst input_type))
       | Some effect_branches ->
         (* Track the names used by the inner expression. *)
         let ( ctx_for_input_expr
             , closed_over
             , (_close_over_name : Mir_name.t -> Mir_name.t) )
           =
           find_closed_over_names
             ~ctx
             ~parent_ctx:ctx
             ~recursively_bound_names:Mir_name.Set.empty
         in
         let input_expr =
           Node.with_value input_expr ~f:(fun expr ->
             of_typed_expr ~ctx:ctx_for_input_expr expr (fst input_type))
         in
         let closed_over = !closed_over in
         let value_handler =
           Option.map value_branch ~f:(fun (pat, expr) ->
             let pat, pat_type = Node.split pat in
             let (ctx, bindings), arg =
               Node.with_value pat_type ~f:(fun (pat_type, constraints) ->
                 if not (List.is_empty constraints)
                 then
                   compiler_bug
                     [%message "Type constraints in handler value branch pattern"];
                 process_argument ~ctx ~bindings:[] ~arg:pat ~arg_type:pat_type)
             in
             let expr =
               Node.with_value expr ~f:(fun expr ->
                 add_let_bindings ~bindings ~body:(of_typed_expr ~ctx expr output_type))
             in
             arg, expr)
         in
         let effect_handlers =
           Nonempty.map effect_branches ~f:(fun (effect_branch, handler_expr) ->
             Node.with_value2
               effect_branch
               handler_expr
               ~f:(fun
                    { effect_pattern = { operation; args }; arg_types; resume_type = _ }
                    handler_expr
                  ->
               (* TODO: If we don't need [resume_type] here, it might just be unused. *)
               let effect_op = Effect_op_id.create ~effect_operation_name:operation in
               let (ctx, bindings), args =
                 process_arguments
                   ~ctx
                   ~args
                   ~arg_types:
                     (Nonempty.map arg_types ~f:(fun (type_, constraints) ->
                        if not (List.is_empty constraints)
                        then
                          compiler_bug
                            [%message "Type constraints in effect operation args"];
                        type_))
               in
               let ctx, resume = Context.add_value_name ctx Value_name.resume_keyword in
               let handler =
                 add_let_bindings
                   ~bindings
                   ~body:(of_typed_expr ~ctx handler_expr output_type)
               in
               { effect_op; args; resume; handler }))
         in
         Handle_effects
           { vars = Map.to_alist closed_over
           ; value_handler
           ; effect_handlers
           ; expr = input_expr
           })
    | Let { rec_; bindings; body }, body_type ->
      let ctx_for_body, bindings =
        Nonempty.fold_map
          bindings
          ~init:ctx
          ~f:(fun ctx_for_body ((pat_and_type, _fixity, _expr) as binding) ->
          let ctx_for_body, names_bound =
            Node.with_value pat_and_type ~f:(fun (pattern, _) ->
              Typed_ast.Pattern.fold_names
                pattern
                ~init:(ctx_for_body, Mir_name.Set.empty)
                ~f:(fun (ctx_for_body, names_bound) name ->
                let ctx, name = Context.add_value_name ctx_for_body name in
                ctx, Set.add names_bound name))
          in
          ctx_for_body, (binding, names_bound))
      in
      let ctx, bindings =
        generate_let_bindings
          ~ctx
          ~ctx_for_body
          ~rec_
          ~init:[]
          ~add_let:(fun bindings
                        name
                        mir_expr
                        (_ : Module_path.absolute Type_scheme.type_) ->
            (name, mir_expr) :: bindings)
          ~extract_binding:(fun (pat_and_type, (_ : Fixity.t option), expr) ->
            Node.map pat_and_type ~f:fst, expr, Node.with_value pat_and_type ~f:snd)
          ~process_expr:(fun bindings ~just_bound ~ctx expr typ ->
            ( bindings
            , Node.map expr ~f:(fun expr -> of_typed_expr ~just_bound ~ctx expr typ) ))
          bindings
      in
      let body =
        (* Pass through [just_bound], but only if we generated no bindings. It 
             wouldn't be correct to use it otherwise, as we might not be able to elide
             the resulting binding if it is inside some nested `let` bindings. *)
        let just_bound =
          match just_bound with
          | None -> None
          | Some _ ->
            if List.for_all bindings ~f:(fun (name, mir_expr) ->
                 match mir_expr with
                 | Name mir_name -> Mir_name.equal name mir_name
                 | _ -> false)
            then just_bound
            else None
        in
        Node.with_value body ~f:(fun body ->
          of_typed_expr ?just_bound ~ctx body body_type)
      in
      add_let_bindings ~bindings ~body
    | Tuple fields, Tuple field_types ->
      make_block ~ctx ~tag:Cnstr_tag.default ~fields ~field_types
    | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
      failwith "TODO: records in MIR exprs"
    | ( Lambda _, (Var _ | Type_app _ | Tuple _)
      | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
      compiler_bug
        [%message
          "Incompatible expr and type"
            (expr : _ Type_scheme.t Typed_ast.Expr.t * _ Type_scheme.type_)]
    | _, (Union _ | Intersection _) ->
      failwith "TODO: handle mir conversion for union and intersection types"
  and add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound =
    (* Keep track of the parent context before binding any variables. This lets us
         check which variables are captured by closures later on. *)
    let parent_ctx = ctx in
    let (ctx, bindings), args = process_arguments ~ctx ~args ~arg_types in
    let recursively_bound_names =
      match just_bound with
      | Some (Rec { this_name; other_names }) -> Set.add other_names this_name
      | Some (Nonrec _) | None -> Mir_name.Set.empty
    in
    let ctx, closed_over, close_over_name =
      find_closed_over_names ~ctx ~parent_ctx ~recursively_bound_names
    in
    let body = Node.with_value body ~f:(fun body -> of_typed_expr ~ctx body body_type) in
    let body = add_let_bindings ~bindings ~body in
    (* TODO: Consider having closures share an environment instead of closing over other
         mutually recursive closures. *)
    let fun_name =
      (* If we are a closure, [fun_name] can't be the same as the name we bind, since we
           aren't returning [Name fun_name] and relying on the assignment getting elided. *)
      match just_bound with
      | Some (Rec { this_name; _ }) when Map.is_empty !closed_over -> this_name
      | Some (Nonrec { this_pattern_names }) when Set.length this_pattern_names = 1 ->
        let name = Set.choose_exn this_pattern_names in
        if Map.is_empty !closed_over then name else Context.copy_name ctx name
      | Some (Rec _ | Nonrec _) | None ->
        snd (Context.add_value_name ctx Constant_names.fun_)
    in
    let (fun_def : Fun_def.t), fun_or_closure =
      if Map.is_empty !closed_over
      then { fun_name; args; body }, Name fun_name
      else (
        let (_ : Context.t), closure_env_name =
          Context.add_value_name ctx Constant_names.closure_env
        in
        let closure_env = Name closure_env_name in
        let body =
          match just_bound with
          | Some (Nonrec _) | None -> body
          | Some (Rec { this_name; other_names }) ->
            (* Fix up the body of closures so they close over other recursively bound
                 closures, and refer to themselves with a direct function call passing in
                 the closure environment. *)
            map body ~f:(function
              | Fun_call (name, args) when Mir_name.equal name this_name ->
                Halt (Fun_call (fun_name, Nonempty.cons closure_env args))
              | Name name when Set.mem other_names name ->
                Halt (Name (close_over_name name))
              | expr -> Defer expr)
        in
        let closed_over = !closed_over in
        let args = Nonempty.cons closure_env_name args in
        let (_ : int), body =
          Map.fold closed_over ~init:(1, body) ~f:(fun ~key:_ ~data:name (i, body) ->
            i + 1, Let (name, Get_block_field (Block_index.of_int i, closure_env), body))
        in
        let closure =
          Make_block
            { tag = Cnstr_tag.closure
            ; fields =
                Name fun_name
                :: List.map (Map.keys closed_over) ~f:(fun name -> Name name)
            }
        in
        { fun_name; args; body }, closure)
    in
    add_fun_def fun_def;
    fun_or_closure
  and find_closed_over_names ~ctx ~parent_ctx ~recursively_bound_names =
    let closed_over = ref Mir_name.Map.empty in
    let close_over_name mir_name =
      match Map.find !closed_over mir_name with
      | Some name -> name
      | None ->
        let new_name = Context.copy_name ctx mir_name in
        closed_over := Map.set !closed_over ~key:mir_name ~data:new_name;
        new_name
    in
    let from_which_context name mir_name =
      let in_context ctx =
        Context.peek_value_name ctx name
        |> Option.value_map ~default:false ~f:(Mir_name.equal mir_name)
      in
      if in_context outer_ctx || is_local_fun_def mir_name
      then `From_toplevel
      else if Set.mem recursively_bound_names mir_name
      then `Recursively_bound
      else if in_context parent_ctx
      then `Closed_over_from_parent
      else `Newly_bound
    in
    (* Determine if names looked up were closed over from the parent context. *)
    let ctx =
      Context.with_find_override ctx ~f:(fun name mir_name ->
        match from_which_context name mir_name with
        | `Newly_bound | `Recursively_bound | `From_toplevel -> None
        | `Closed_over_from_parent -> Some (close_over_name mir_name))
    in
    ctx, closed_over, close_over_name
  and make_block ~ctx ~tag ~fields ~field_types =
    let fields =
      List.map2_exn fields field_types ~f:(fun field_expr field_type ->
        Node.with_value field_expr ~f:(fun field_expr ->
          of_typed_expr ~ctx field_expr field_type))
    in
    Make_block { tag; fields }
  and handle_match_arms
    ~ctx
    ~input_expr
    ~input_type:((input_type, _constraints) : _ Type_scheme.t)
    ~output_type
    arms
    =
    let rec loop_one_arm ~pattern ~output_expr ~coverage arms =
      let patterns = Node.with_value pattern ~f:Simple_pattern.flatten_typed_pattern in
      let coverage' = Simple_pattern.Coverage.of_patterns patterns in
      let coverage =
        match coverage with
        | None -> coverage'
        | Some coverage -> Simple_pattern.Coverage.combine coverage coverage'
      in
      let fallback =
        if List.is_empty arms then None else Some (fun () -> loop ~coverage arms)
      in
      handle_single_arm
        ~ctx
        ~input_expr
        ~input_type
        ~output_expr
        ~output_type
        ~fallback
        patterns
    and loop ~coverage arms =
      match arms with
      | [] ->
        (match Simple_pattern.Coverage.missing_cases ~ctx ~input_type coverage with
         | [] ->
           compiler_bug [%message "Pattern coverage/condition checking is out of sync"]
         | missing_cases ->
           Compilation_error.raise
             Mir_error
             ~msg:
               [%message
                 "This pattern match is not exhaustive"
                   (missing_cases : Simple_pattern.t list)])
      | (pattern, output_expr) :: arms ->
        loop_one_arm ~pattern ~output_expr ~coverage:(Some coverage) arms
    in
    let ((pattern, output_expr) :: arms) = arms in
    loop_one_arm ~pattern ~output_expr ~coverage:None arms
  and handle_single_arm
    ~ctx
    ~input_expr
    ~input_type
    ~output_expr
    ~output_type
    ~fallback
    patterns
    =
    let ctx, vars =
      List.fold_map
        (Simple_pattern.names (Nonempty.hd patterns) |> Set.to_list)
        ~init:ctx
        ~f:Context.add_value_name
    in
    let ctx, wrapping_binding, input_expr =
      make_atomic
        ~ctx
        ~default:None
        ~add_let:(fun name expr -> Some (name, expr))
        ~add_name:Context.add_value_name
        ~binding_name:Constant_names.binding
        input_expr
    in
    let body =
      Node.with_value output_expr ~f:(fun output_expr ->
        of_typed_expr ~ctx output_expr output_type)
    in
    let fold_result =
      (* TODO: Consider writing this is a regular recursive function. It might actually
           be easier to understand that way, and this version duplicates work by always
           generating the condition for the last pattern and throwing it away. *)
      Nonempty.fold_map_until ~init:() patterns ~f:(fun () pattern ->
        (* TODO: this should skip underscore bindings (bindings for no actual variables) *)
        let cond = condition_of_pattern ~ctx ~input_expr ~input_type pattern in
        let (_ : Context.t), bindings =
          (* Names are pre-added to the context above so they get the same unique names
               in different patterns. *)
          let add_name ctx name = ctx, Context.find_value_name_assert_local ctx name in
          let add_let bindings name expr = (name, expr) :: bindings in
          fold_pattern_bindings
            ~ctx
            pattern
            input_expr
            input_type
            ~init:[]
            ~add_let
            ~add_name
        in
        let bindings =
          (* Bindings must be sorted by their names to match up with [vars] above, which
               are also in sorted order. *)
          List.sort bindings ~compare:[%compare: Mir_name.t * _] |> List.map ~f:snd
        in
        match cond with
        | None ->
          (* TODO: warn about unused patterns (the rest of the patterns and following
               arms) *)
          Stop bindings
        | Some cond -> Continue ((), (cond, bindings)))
    in
    let add_last_unconditional_bindings ~conds ~last_bindings =
      match Nonempty.of_list conds with
      | Some conds ->
        Cond_assign { vars; conds; body; if_none_matched = Use_bindings last_bindings }
      | None ->
        (* If there are no other conditions, the result doesn't need [Cond_assign] and
             can be a regular unconditional expression. *)
        List.fold2_exn vars last_bindings ~init:body ~f:(fun acc name expr ->
          Let (name, expr, acc))
    in
    let result_expr =
      match fold_result, fallback with
      | Continue ((), conds), Some fallback ->
        (* Didn't hit an unconditional pattern; fallback to later match arms. *)
        Cond_assign { vars; conds; body; if_none_matched = Otherwise (fallback ()) }
      | Continue ((), conds), None ->
        (* Didn't hit an unconditional pattern, but this is the last match arm. Due to
             separate checks in [Simple_pattern.Coverage] we know the pattern is
             exhaustive, so we can elide the last condition. *)
        let conds, ((_ : cond), last_bindings) = Nonempty.split_last conds in
        add_last_unconditional_bindings ~conds ~last_bindings
      | Stop (last_bindings, conds), _ ->
        (* Found an unconditional pattern. *)
        (* TODO: Warn if [fallback] is [Some], since we're ignoring following match arms. *)
        add_last_unconditional_bindings ~conds ~last_bindings
    in
    match wrapping_binding with
    | None -> result_expr
    | Some (name, expr) -> Let (name, expr, result_expr)
  in
  of_typed_expr ~just_bound:outer_just_bound ~ctx:outer_ctx outer_expr outer_type
;;
