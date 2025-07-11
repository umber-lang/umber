open Import
open Names

let type_error_msg msg = Compilation_error.raise Type_error ~msg:[%message msg]
let eprint_s = eprint_s [%here]

module Pattern = struct
  type 'typ t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of 'typ t * Value_name.t
    | Cnstr_appl of Cnstr_name.Absolute.t * ('typ t * 'typ) list
    | Tuple of ('typ t * 'typ) list
    | Record of (Value_name.t * 'typ t option) Nonempty.t
    | Union of 'typ t * 'typ t
  [@@deriving equal, sexp, variants]

  type generalized = Module_path.absolute Type_scheme.t t [@@deriving sexp_of]

  let fold_names pat ~init ~f =
    let rec loop acc ~f = function
      | Catch_all (Some name) -> f acc name
      | As (pat, name) -> loop (f acc name) ~f pat
      | Cnstr_appl (_, items) | Tuple items ->
        List.fold items ~init:acc ~f:(fun acc (item, _type) -> loop acc item ~f)
      | Record fields ->
        Nonempty.fold fields ~init:acc ~f:(fun acc -> function
          | name, None -> f acc name
          | _, Some pat -> loop acc ~f pat)
      | Union (pat, _) ->
        (* Both branches bind the same names, so only one need be considered *)
        loop acc ~f pat
      | Constant _ | Catch_all None -> acc
    in
    loop init ~f pat
  ;;

  let rec map :
            'a 'b.
            'a t -> f:('a t -> ('a t, 'b t) Map_action.t) -> f_type:('a -> 'b) -> 'b t
    =
   fun pat ~f ~f_type ->
    match (f pat : _ Map_action.t) with
    | Halt pat -> pat
    | Retry pat -> map pat ~f ~f_type
    | Defer pat ->
      (match pat with
       | (Constant _ | Catch_all _) as pat -> pat
       | As (pat, name) -> As (map pat ~f ~f_type, name)
       | Cnstr_appl (cnstr, args) ->
         Cnstr_appl
           ( cnstr
           , List.map args ~f:(fun (pat, type_) ->
               let pat = map pat ~f ~f_type in
               let type_ = f_type type_ in
               pat, type_) )
       | Tuple fields ->
         Tuple
           (List.map fields ~f:(fun (pat, type_) ->
              let pat = map pat ~f ~f_type in
              let type_ = f_type type_ in
              pat, type_))
       | Record fields ->
         Record
           (Nonempty.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f ~f_type))))
       | Union (pat1, pat2) ->
         let pat1 = map pat1 ~f ~f_type in
         let pat2 = map pat2 ~f ~f_type in
         Union (pat1, pat2))
 ;;

  let map_types pat ~f = map pat ~f_type:f ~f:Map_action.defer

  let check_cnstr_application ~names ~types ~(cnstr_type : Internal_type.t) ~arg_types =
    (* TODO: inferring unqualified name given type information *)
    let expected_arg_types, effects, result_type =
      match cnstr_type with
      | Function (arg_types, effects, result_type) ->
        Nonempty.to_list arg_types, Some effects, result_type
      | result_type -> [], None, result_type
    in
    match
      List.iter2 expected_arg_types arg_types ~f:(fun expected_arg_type actual_arg_type ->
        (* The actual type of the argument must be a supertype of the expected type
           since we are deconstructing rather than applying the constructor. *)
        Type_bindings.constrain
          ~names
          ~types
          ~subtype:expected_arg_type
          ~supertype:actual_arg_type)
    with
    | Ok () -> effects, result_type
    | Unequal_lengths -> type_error_msg "Wrong number of arguments in application"
  ;;

  (* TODO: Putting pattern names inside here is a hack to make the types work - they are
     unused. We might be able to come up with a better way to smuggle that info through. *)
  let rec of_untyped_with_names ~names ~types ~fixity pat_names
    :  Untyped.Pattern.t
    -> Pattern_names.t * ((Internal_type.t * Pattern_names.t) t * Internal_type.t)
    = function
    | Constant lit ->
      ( pat_names
      , ( Constant lit
        , Type_bindings.instantiate_type_scheme ~names ~types (Literal.typ lit) ) )
    | Catch_all name ->
      let pat_names, typ =
        match name with
        | Some name ->
          Pattern_names.add_fresh_name pat_names name ~type_source:Placeholder ~fixity
        | None -> pat_names, Internal_type.fresh_var ()
      in
      eprint_s
        [%lazy_message
          "pattern bound variable" (name : Value_name.t option) (typ : Internal_type.t)];
      pat_names, (Catch_all name, typ)
    | Cnstr_appl (cnstr, args) ->
      let cnstr_type =
        Name_bindings.find_cnstr_type names cnstr
        |> Type_bindings.instantiate_type_or_scheme ~names ~types
      in
      let cnstr =
        Name_bindings.absolutify_value_name
          names
          (Value_name.Qualified.of_cnstr_name cnstr)
        |> Value_name.Qualified.to_cnstr_name
        |> ok_or_compiler_bug ~here:[%here]
      in
      let pat_names, args =
        List.fold_map args ~init:pat_names ~f:(fun pat_names arg ->
          of_untyped_with_names ~names ~types pat_names arg ~fixity)
      in
      let effects, result_type =
        let arg_types = List.map ~f:snd args in
        check_cnstr_application ~names ~types ~cnstr_type ~arg_types
      in
      assert_or_compiler_bug
        ~here:[%here]
        (match effects with
         | None -> true
         | Some { effects; effect_var } ->
           Map.is_empty effects && Option.is_none effect_var);
      let args =
        List.map args ~f:(fun (arg, arg_type) -> arg, (arg_type, Pattern_names.empty))
      in
      pat_names, (Cnstr_appl (cnstr, args), result_type)
    | Tuple fields ->
      let pat_names, fields, field_types =
        List.fold_right
          fields
          ~init:(pat_names, [], [])
          ~f:(fun field (pat_names, fields, field_types) ->
          let pat_names, (field, field_type) =
            of_untyped_with_names ~names ~types pat_names field ~fixity
          in
          ( pat_names
          , (field, (field_type, Pattern_names.empty)) :: fields
          , field_type :: field_types ))
      in
      pat_names, (Tuple fields, Tuple field_types)
    | Record _fields ->
      (*let _names, _fields =
          List.fold_right
            fields
            ~init:(names, [])
            ~f:(fun (field_name, field_pat) (names, fields) ->
            let names, field_pat =
              match field_pat with
              | Some field_pat ->
                let new_names, field_pat =
                  of_untyped ~handle_name ~names ~types field_pat
                in
                merge_no_shadow names new_names, Some field_pat
              | None -> names, None
            in
            names, (field_name, field_pat) :: fields)
        in*)
      (*names, (Record fields, Record (List.map ~f:(fun (name, pat) ->
              match pat with
              | Some (_, typ) -> typ
              | None -> Type_bindings.fresh_var ())))*)
      failwith "TODO: record types in patterns"
    | Union (pat1, pat2) ->
      let pat_names1, (pat1, typ1) =
        of_untyped_with_names ~names ~types pat_names pat1 ~fixity
      in
      let pat_names2, (pat2, typ2) =
        of_untyped_with_names ~names ~types pat_names pat2 ~fixity
      in
      let result_type = Internal_type.fresh_var () in
      Type_bindings.constrain ~names ~types ~subtype:typ1 ~supertype:result_type;
      Type_bindings.constrain ~names ~types ~subtype:typ2 ~supertype:result_type;
      (* Unions must define the same names with compatible types. *)
      if not
           (Map.equal
              (fun entry1 entry2 ->
                let entry_type = Internal_type.fresh_var () in
                Type_bindings.constrain'
                  ~names
                  ~types
                  ~subtype:(Name_bindings.Name_entry.type_ entry1)
                  ~supertype:(Type entry_type);
                Type_bindings.constrain'
                  ~names
                  ~types
                  ~subtype:(Name_bindings.Name_entry.type_ entry2)
                  ~supertype:(Type entry_type);
                true)
              pat_names1
              pat_names2)
      then type_error_msg "Pattern unions must define the same names";
      pat_names1, (Union (pat1, pat2), result_type)
    | As (pat, name) ->
      let pat_names, (pat, typ) =
        of_untyped_with_names ~names ~types pat_names pat ~fixity
      in
      let pat_names =
        Pattern_names.add_name pat_names name typ ~type_source:Placeholder ~fixity
      in
      pat_names, (As (pat, name), typ)
    | Type_annotation (pat, annotated_type) ->
      (* TODO: Handle trait bounds for type annotations, once traits are implemented. *)
      let annotated_type =
        Type_bindings.instantiate_type_scheme
          ~names
          ~types
          (Type_scheme.map'
             annotated_type
             ~type_name:(Name_bindings.absolutify_type_name names)
             ~effect_name:(Name_bindings.absolutify_effect_name names))
      in
      let pat_names, (pat, inferred_type) =
        of_untyped_with_names ~names ~types pat_names pat ~fixity
      in
      Type_bindings.constrain
        ~names
        ~types
        ~subtype:inferred_type
        ~supertype:annotated_type;
      pat_names, (pat, annotated_type)
  ;;

  let of_untyped_into ~names ~types ~fixity pattern =
    let ((pat_names, _) as pat) =
      of_untyped_with_names ~names ~types Pattern_names.empty pattern ~fixity
    in
    let names =
      Name_bindings.merge_names names pat_names ~combine:(fun _ _ new_entry -> new_entry)
    in
    Type_bindings.record_context_vars types pat_names;
    names, pat
  ;;

  let generalize pat pat_names typ ~names ~types ~toplevel =
    eprint_s
      [%lazy_message
        "Pattern.generalize"
          (pat_names : Pattern_names.t)
          (typ : Internal_type.t)
          (types : Type_bindings.t)];
    let shadowing_allowed = not toplevel in
    let generalize_memoized =
      Memo.general ~hashable:Internal_type.hashable (fun type_ ->
        let scheme = Type_bindings.generalize types type_ in
        (* TODO: This is a weird hack. There's surely a better way of doing this. *)
        (* Do an extra type simplification pass on toplevel patterns only. This lets us
           further simplify types with the knowledge that there are no context variables,
           allowing some useless variables to be removed e.g. effect variables in return
           position. *)
        if toplevel
        then
          Type_simplification.simplify_type
            scheme
            ~context_vars:(By_polarity.init (const Type_param.Map.empty))
        else scheme)
    in
    let names =
      Map.fold pat_names ~init:names ~f:(fun ~key:name ~data:entry names ->
        let inferred_scheme =
          match Name_bindings.Name_entry.type_ entry with
          | Scheme scheme -> scheme
          | Type type_ -> generalize_memoized type_
        in
        Name_bindings.set_inferred_scheme names name inferred_scheme ~shadowing_allowed)
    in
    let pat =
      map_types pat ~f:(fun (type_, (_ : Pattern_names.t)) -> generalize_memoized type_)
    in
    names, pat, generalize_memoized typ
  ;;
end

module Effect_pattern = struct
  type 'typ t =
    { operation : Value_name.Absolute.t
    ; args : 'typ Pattern.t Node.t Nonempty.t
    }
  [@@deriving equal, sexp]

  let map_types { operation; args } ~f =
    { operation; args = Nonempty.map args ~f:(Node.map ~f:(Pattern.map_types ~f)) }
  ;;
end

module Effect_branch = struct
  type 'typ t =
    { effect_pattern : 'typ Effect_pattern.t
    ; arg_types : 'typ Nonempty.t
    ; resume_type : 'typ
    }
  [@@deriving sexp]

  let of_untyped_with_names
    ~names
    ~types
    ~result_effects
    ~result_type
    pat_names
    ({ operation; args } : Untyped.Effect_pattern.t)
    =
    let operation = Name_bindings.absolutify_value_name names operation in
    let operation_name_entry = Name_bindings.find_absolute_entry names operation in
    (match Name_bindings.Name_entry.type_source operation_name_entry with
     | Effect_operation -> ()
     | _ ->
       Compilation_error.raise
         Other
         ~msg:
           [%message "Expected an effect operation" (operation : Value_name.Absolute.t)]);
    let operation_type =
      Name_bindings.Name_entry.type_ operation_name_entry
      |> Type_bindings.instantiate_type_or_scheme ~names ~types
    in
    let pat_names, args_with_types =
      Nonempty.fold_map args ~init:pat_names ~f:(fun pat_names arg ->
        let arg_span = Node.span arg in
        let arg_names, (arg, arg_type) =
          Node.with_value arg ~f:(fun arg ->
            Pattern.of_untyped_with_names
              ~names
              ~types
              Pattern_names.empty
              arg
              ~fixity:None)
        in
        ( Pattern_names.merge pat_names arg_names ~combine:(fun ~key:_ _ x -> x)
        , (Node.create arg arg_span, (arg_type, arg_names)) ))
    in
    let args, arg_types = Nonempty.unzip args_with_types in
    let operation_effects, operation_result_type =
      Pattern.check_cnstr_application
        ~names
        ~types
        ~cnstr_type:operation_type
        ~arg_types:(Nonempty.to_list arg_types |> List.map ~f:fst)
    in
    let operation_effects =
      let error () =
        compiler_bug
          [%message
            "Unexpected effects for effect operation"
              (operation_effects : Internal_type.effects option)]
      in
      match operation_effects with
      | Some { effects; effect_var = None } ->
        (match Map.to_alist effects with
         | [ effect ] -> effect
         | _ -> error ())
      | Some { effects = _; effect_var = Some _ } | None -> error ()
    in
    let resume_type : Internal_type.t =
      Function ([ operation_result_type ], result_effects, result_type)
    in
    let resume_pat_names =
      Pattern_names.add_name
        Pattern_names.empty
        Value_name.resume_keyword
        resume_type
        ~type_source:Placeholder
        ~fixity:None
    in
    let pat_names =
      Pattern_names.merge pat_names resume_pat_names ~combine:(fun ~key:_ _ x -> x)
    in
    ( pat_names
    , ( { effect_pattern = { operation; args }
        ; arg_types
        ; resume_type = resume_type, resume_pat_names
        }
      , operation_effects ) )
  ;;

  let of_untyped_into ~names ~types ~result_effects ~result_type effect_pattern =
    let ((pat_names, _) as pat) =
      of_untyped_with_names
        ~names
        ~types
        ~result_effects
        ~result_type
        Pattern_names.empty
        effect_pattern
    in
    let names =
      Name_bindings.merge_names names pat_names ~combine:(fun _ _ new_entry -> new_entry)
    in
    Type_bindings.record_context_vars types pat_names;
    names, pat
  ;;
end

module Expr = struct
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Absolute.t
    | Fun_call of 'typ t Node.t * 'typ * ('typ t Node.t * 'typ) Nonempty.t
    | Lambda of 'typ Pattern.t Node.t Nonempty.t * 'typ t Node.t
    | Match of 'typ t Node.t * 'typ * ('typ Pattern.t Node.t * 'typ t Node.t) Nonempty.t
    | Handle of
        { expr : 'typ t Node.t
        ; expr_type : 'typ
        ; value_branch : (('typ Pattern.t * 'typ) Node.t * 'typ t Node.t) option
        ; effect_branches : ('typ Effect_branch.t Node.t * 'typ t Node.t) list
        }
    | Let of ('typ Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t Node.t list
    | Record_literal of (Value_name.t * 'typ t Node.t option) list
    | Record_update of 'typ t Node.t * (Value_name.t * 'typ t Node.t option) list
    | Record_field_access of 'typ t Node.t * Value_name.t Node.t
  [@@deriving sexp]

  type generalized =
    Module_path.absolute Type_scheme.t t * Module_path.absolute Type_scheme.t
  [@@deriving sexp_of]

  let type_recursive_let_bindings =
    let collect_effects ~names ~types f =
      let effects : Internal_type.effects =
        { effects = Effect_name.Absolute.Map.empty
        ; effect_var = Some (Type_var.create ())
        }
      in
      let (expr : _ t Node.t), (typ : Internal_type.t) =
        f ~add_effects:(fun subtype span ->
          Node.with_value (Node.create subtype span) ~f:(fun subtype ->
            Type_bindings.constrain_effects ~names ~types ~subtype ~supertype:effects))
      in
      expr, typ, effects
    in
    let rec of_untyped ~names ~types ~f_name expr
      : (Internal_type.t * Pattern_names.t) t Node.t
        * Internal_type.t
        * Internal_type.effects
      =
      let expr_span = Node.span expr in
      let node e = Node.create e expr_span in
      let result =
        Node.with_value expr ~f:(fun expr ->
          match (expr : Untyped.Expr.t) with
          | Literal lit ->
            ( node (Literal lit)
            , Type_bindings.instantiate_type_scheme ~names ~types (Literal.typ lit)
            , Internal_type.no_effects )
          | Name name ->
            let name, name_entry = Name_bindings.find_entry_with_path names name in
            f_name name name_entry;
            let type_ =
              Name_bindings.Name_entry.type_ name_entry
              |> Type_bindings.instantiate_type_or_scheme ~names ~types
            in
            node (Name name), type_, Internal_type.no_effects
          | Qualified (path, expr) ->
            let names = Name_bindings.import_all names path in
            of_untyped ~names ~types ~f_name expr
          | Fun_call (fun_, args) ->
            collect_effects ~names ~types (fun ~add_effects ->
              let fun_, fun_type, fun_effects = of_untyped ~names ~types ~f_name fun_ in
              add_effects fun_effects (Node.span fun_);
              let args =
                Nonempty.map args ~f:(fun arg ->
                  let arg, arg_type, arg_effects = of_untyped ~names ~types ~f_name arg in
                  add_effects arg_effects (Node.span arg);
                  arg, (arg_type, Pattern_names.empty))
              in
              let arg_types = Nonempty.map args ~f:(fun (_, (arg_type, _)) -> arg_type) in
              let result_var = Type_var.create () in
              let call_effects : Internal_type.effects =
                { effects = Effect_name.Absolute.Map.empty
                ; effect_var = Some (Type_var.create ())
                }
              in
              add_effects call_effects expr_span;
              Type_bindings.constrain
                ~names
                ~types
                ~subtype:fun_type
                ~supertype:(Partial_function (arg_types, call_effects, result_var));
              ( node (Fun_call (fun_, (fun_type, Pattern_names.empty), args))
              , Var result_var ))
          | Op_tree tree ->
            of_untyped ~names ~types ~f_name (Op_tree.to_untyped_expr ~names tree)
          | Op_section { op_side; op; expr } ->
            let op_span = Node.span op in
            let expr_span = Node.span expr in
            let left_var = Constant_names.synthetic_arg 0 in
            let right_var = Constant_names.synthetic_arg 1 in
            let applied_arg_var, unapplied_arg_var, left_var_span, right_var_span =
              match op_side with
              | `Left -> right_var, left_var, op_span, expr_span
              | `Right -> left_var, right_var, expr_span, op_span
            in
            let qualified = Value_name.Relative.with_path Module_path.Relative.empty in
            let rewritten_expr : Untyped.Expr.t =
              Let
                { rec_ = false
                ; bindings =
                    [ ( Node.create
                          (Untyped.Pattern.catch_all (Some applied_arg_var))
                          expr_span
                      , None
                      , expr )
                    ]
                ; body =
                    Node.create
                      (Untyped.Expr.Lambda
                         ( [ Node.create
                               (Untyped.Pattern.catch_all (Some unapplied_arg_var))
                               op_span
                           ]
                         , Node.create
                             (Untyped.Expr.Fun_call
                                ( Node.map op ~f:Untyped.Expr.name
                                , [ Node.create
                                      (Untyped.Expr.Name (qualified left_var))
                                      left_var_span
                                  ; Node.create
                                      (Untyped.Expr.Name (qualified right_var))
                                      right_var_span
                                  ] ))
                             expr_span ))
                      (Span.combine left_var_span right_var_span)
                }
            in
            of_untyped ~names ~types ~f_name (node rewritten_expr)
          | Lambda (args, body) ->
            let names, args_and_types =
              Nonempty.fold_map args ~init:names ~f:(fun names arg ->
                let span = Node.span arg in
                let names, ((_ : Pattern_names.t), (arg, arg_type)) =
                  Node.with_value
                    arg
                    ~f:(Pattern.of_untyped_into ~names ~types ~fixity:None)
                in
                names, (Node.create arg span, arg_type))
            in
            let args, arg_types = Nonempty.unzip args_and_types in
            let body, body_type, body_effects = of_untyped ~names ~types ~f_name body in
            eprint_s [%lazy_message (body_type : Internal_type.t)];
            ( node (Lambda (args, body))
            , Function (arg_types, body_effects, body_type)
            , Internal_type.no_effects )
          | If (cond, then_, else_) ->
            collect_effects ~names ~types (fun ~add_effects ->
              let cond, cond_type, cond_effects = of_untyped ~names ~types ~f_name cond in
              add_effects cond_effects (Node.span cond);
              let bool_type =
                Type_bindings.instantiate_type_scheme ~names ~types Intrinsics.Bool.typ
              in
              Type_bindings.constrain
                ~names
                ~types
                ~subtype:cond_type
                ~supertype:bool_type;
              let (then_, then_type, then_effects), (else_, else_type, else_effects) =
                ( of_untyped ~names ~types ~f_name then_
                , of_untyped ~names ~types ~f_name else_ )
              in
              add_effects then_effects (Node.span then_);
              add_effects else_effects (Node.span else_);
              let result_type = Internal_type.fresh_var () in
              Type_bindings.constrain
                ~names
                ~types
                ~subtype:then_type
                ~supertype:result_type;
              Type_bindings.constrain
                ~names
                ~types
                ~subtype:else_type
                ~supertype:result_type;
              let branch name expr =
                Node.create (Cnstr_appl (name, []) : _ Pattern.t) (Node.span expr), expr
              in
              ( node
                  (Match
                     ( cond
                     , (bool_type, Pattern_names.empty)
                     , [ branch Intrinsics.Bool.true_ then_
                       ; branch Intrinsics.Bool.false_ else_
                       ] ))
              , result_type ))
          | Match (expr, branches) ->
            collect_effects ~names ~types (fun ~add_effects ->
              let expr, expr_type, expr_effects = of_untyped ~names ~types ~f_name expr in
              add_effects expr_effects (Node.span expr);
              let result_type = Internal_type.fresh_var () in
              eprint_s
                [%lazy_message
                  "typing match"
                    (result_type : Internal_type.t)
                    (expr_type : Internal_type.t)];
              let branches =
                Nonempty.map branches ~f:(fun (pat, branch) ->
                  let pat_span = Node.span pat in
                  let names, ((_ : Pattern_names.t), (pat, pat_type)) =
                    Node.with_value
                      pat
                      ~f:(Pattern.of_untyped_into ~names ~types ~fixity:None)
                  in
                  Type_bindings.constrain
                    ~names
                    ~types
                    ~subtype:expr_type
                    ~supertype:pat_type;
                  let branch, branch_type, branch_effects =
                    of_untyped ~names ~types ~f_name branch
                  in
                  add_effects branch_effects (Node.span branch);
                  Type_bindings.constrain
                    ~names
                    ~types
                    ~subtype:branch_type
                    ~supertype:result_type;
                  Node.create pat pat_span, branch)
              in
              node (Match (expr, (expr_type, Pattern_names.empty), branches)), result_type)
          | Match_function branches ->
            let name = Constant_names.match_ in
            of_untyped
              ~names
              ~types
              ~f_name
              (node
                 (Untyped.Expr.Lambda
                    ( [ node (Untyped.Pattern.catch_all (Some name)) ]
                    , node
                        (Untyped.Expr.Match
                           ( node (Untyped.Expr.Name (Module_path.Relative.empty, name))
                           , branches )) )))
          | Handle (expr, branches) ->
            let result_effect_var = Type_var.create () in
            let result_effects : Internal_type.effects =
              { effects = Effect_name.Absolute.Map.empty
              ; effect_var = Some result_effect_var
              }
            in
            let result_type = Internal_type.fresh_var () in
            let expr, expr_type, expr_effects = of_untyped ~names ~types ~f_name expr in
            let handled_effects = Effect_name.Absolute.Table.create () in
            let all_branch_effects = Queue.create () in
            let value_branches, effect_branches =
              List.partition_map
                (Nonempty.to_list branches)
                ~f:(fun (pattern, branch_expr) ->
                let pattern_span = Node.span pattern in
                match Node.with_value pattern ~f:Fn.id with
                | `Value pattern ->
                  let names, (pattern_names, (pattern, pattern_type)) =
                    Pattern.of_untyped_into ~names ~types pattern ~fixity:None
                  in
                  Type_bindings.constrain
                    ~names
                    ~types
                    ~subtype:expr_type
                    ~supertype:pattern_type;
                  let branch_expr, branch_type, branch_effects =
                    of_untyped ~names ~types ~f_name branch_expr
                  in
                  Type_bindings.constrain
                    ~names
                    ~types
                    ~subtype:branch_type
                    ~supertype:result_type;
                  Queue.enqueue all_branch_effects branch_effects;
                  First
                    ( Node.create (pattern, (pattern_type, pattern_names)) pattern_span
                    , branch_expr )
                | `Effect effect_pattern ->
                  let ( names
                      , ( (_ : Pattern_names.t)
                        , (effect_branch, (effect_name, effect_args)) ) )
                    =
                    Effect_branch.of_untyped_into
                      ~names
                      ~types
                      ~result_effects
                      ~result_type
                      effect_pattern
                  in
                  let operation_name = snd effect_branch.effect_pattern.operation in
                  Hashtbl.update handled_effects effect_name ~f:(function
                    | None -> [ effect_args ], Value_name.Set.singleton operation_name
                    | Some (existing_args, existing_operations) ->
                      if Set.mem existing_operations operation_name
                      then
                        Compilation_error.raise
                          Type_error
                          ~msg:
                            [%message
                              "Multiple branches handling the same effect operation"
                                (operation_name : Value_name.t)];
                      ( effect_args :: existing_args
                      , Set.add existing_operations operation_name ));
                  let branch_expr, branch_type, branch_effects =
                    of_untyped ~names ~types ~f_name branch_expr
                  in
                  Type_bindings.constrain
                    ~names
                    ~types
                    ~subtype:branch_type
                    ~supertype:result_type;
                  Queue.enqueue all_branch_effects branch_effects;
                  Second (Node.create effect_branch pattern_span, branch_expr))
            in
            let value_branch =
              match value_branches with
              | [] ->
                (* If there are no value branches, it's equivalent to having a branch
                   which looks like `x -> x`. *)
                Type_bindings.constrain
                  ~names
                  ~types
                  ~subtype:expr_type
                  ~supertype:result_type;
                None
              | [ branch ] -> Some branch
              | _ :: _ ->
                (* TODO: Support multiple branches. Equivalent to using match. *)
                Compilation_error.raise
                  Syntax_error
                  ~msg:
                    [%message
                      "Multiple value branches in handle expression are not supported"
                        (value_branches
                          : (((Internal_type.t * Pattern_names.t) Pattern.t
                             * (Internal_type.t * Pattern_names.t))
                             Node.t
                            * (Internal_type.t * Pattern_names.t) t Node.t)
                            list)]
            in
            let handled_effects =
              List.map
                (Hashtbl.to_alist handled_effects)
                ~f:(fun (effect_name, (args, operations_handled)) ->
                (* Check arguments to effect type match. *)
                let args =
                  match args with
                  | [] -> []
                  | initial_args :: rest ->
                    List.iter rest ~f:(fun args ->
                      List.iter2_exn initial_args args ~f:(fun initial_arg arg ->
                        Type_bindings.constrain
                          ~names
                          ~types
                          ~subtype:arg
                          ~supertype:initial_arg));
                    initial_args
                in
                (* Check that all of the operations were handled. *)
                let operations_from_effect_decl =
                  Name_bindings.find_absolute_effect_decl names effect_name
                  |> Effect.operations
                  |> Option.value ~default:[]
                  |> List.map ~f:Effect.Operation.name
                  |> Value_name.Set.of_list
                in
                if not (Set.equal operations_handled operations_from_effect_decl)
                then
                  Compilation_error.raise
                    Type_error
                    ~msg:
                      [%message
                        "Not all operations are handled"
                          (operations_handled : Value_name.Set.t)
                          (operations_from_effect_decl : Value_name.Set.t)];
                effect_name, args)
              |> Effect_name.Absolute.Map.of_alist_exn
            in
            let result_plus_handled_effects : Internal_type.effects =
              { effects = handled_effects; effect_var = Some result_effect_var }
            in
            eprint_s [%lazy_message (result_plus_handled_effects : Internal_type.effects)];
            Type_bindings.constrain_effects
              ~names
              ~types
              ~subtype:expr_effects
              ~supertype:result_plus_handled_effects;
            Queue.iter all_branch_effects ~f:(fun branch_effects ->
              Type_bindings.constrain_effects
                ~names
                ~types
                ~subtype:branch_effects
                ~supertype:result_plus_handled_effects);
            let expr_type = expr_type, Pattern_names.empty in
            ( node (Handle { expr; expr_type; value_branch; effect_branches })
            , result_type
            , result_effects )
          | Let { rec_; bindings; body } ->
            collect_effects ~names ~types (fun ~add_effects ->
              (* FIXME: Don't use Pattern.of_untyped_into because it records context vars
                 Can we get away with not recording context vars for let bindings at all?
                 I think not, e.g. `let foo x = let y = x in y` might not work. *)
              let names, rec_, bindings =
                if rec_
                then (
                  let names, bindings =
                    Nonempty.fold_map
                      bindings
                      ~init:names
                      ~f:(fun names (pat, fixity, expr) ->
                      let pat_span = Node.span pat in
                      let pat_names, (pat, pat_type) =
                        Node.with_value
                          pat
                          ~f:
                            (Pattern.of_untyped_with_names
                               ~names
                               ~types
                               Pattern_names.empty
                               ~fixity)
                      in
                      let names =
                        Name_bindings.merge_names
                          names
                          pat_names
                          ~combine:(fun _ _ new_entry -> new_entry)
                      in
                      ( names
                      , (Node.create (pat, (pat_type, pat_names)) pat_span, fixity, expr)
                      ))
                  in
                  type_recursive_let_bindings ~names ~types ~f_name ~add_effects bindings)
                else (
                  (* TODO: For parallel nonrec bindings, they are not supposed to be in
                     each other's scopes (or their own scope!). Write a test. *)
                  (* Process non-recursive bindings in order without any recursion.
                     Importantly, each one in the group is processed "in parallel",
                     meaning they each get passed the original scope. To make this less
                     error-prone, ban using the "names" variable here. *)
                  let names, bindings =
                    type_non_recursive_let_bindings
                      ~names
                      ~types
                      ~f_name
                      ~add_effects
                      bindings
                  in
                  names, false, bindings)
              in
              (* FIXME: We can't have context vars recorded while generalizing the exprs
                 in a recursive binding group, but they need to be present when
                 generalizing the body. This doesn't really work since we record context
                 vars up front, then generalize everything after in a separate pass. We
                 could maybe change to generalize as we go? *)
              let body, body_type, body_effects = of_untyped ~names ~types ~f_name body in
              (* eprint_s [%message (body_effects : Internal_type.effects)]; *)
              add_effects body_effects (Node.span body);
              node (Let { rec_; bindings; body }), body_type)
          | Tuple items ->
            collect_effects ~names ~types (fun ~add_effects ->
              let items, types =
                List.map items ~f:(fun item ->
                  let item, type_, effects = of_untyped item ~names ~types ~f_name in
                  add_effects effects (Node.span item);
                  item, type_)
                |> List.unzip
              in
              node (Tuple items), Tuple types)
          | Seq_literal items ->
            (* TODO: Come up with a proper sequence abstraction. This should probably be a
               trait with a method to construct it from a list/array. Alternatively, it
               would be reasonable to just have e.g. array literals and force other
               collections to implement `of_array` functions.
               
               For now, to make things easy this just desugars to calls to `Cons` and
               `Nil` constructors, like in OCaml. *)
            let make_constructor name =
              Node.dummy_span
                (Untyped.Expr.Name
                   (Value_name.Relative.of_ustrings_unchecked
                      ([], Ustring.of_string_exn name)))
            in
            let desugared_expr =
              List.fold_right items ~init:(make_constructor "Nil") ~f:(fun item expr ->
                Node.create
                  (Untyped.Expr.Fun_call (make_constructor "Cons", [ item; expr ]))
                  (Span.combine (Node.span item) (Node.span expr)))
            in
            of_untyped ~names ~types ~f_name desugared_expr
          | Record_literal _fields -> failwith "TODO: record1"
          | Record_update (_expr, _fields) -> failwith "TODO: record2"
          | Record_field_access (_record, _name) -> failwith "TODO: record3"
          | Type_annotation (expr, annotated_type) ->
            let annotated_type =
              Node.with_value annotated_type ~f:(fun annotated_type ->
                Type_bindings.instantiate_type_scheme
                  ~names
                  ~types
                  (Type_scheme.map'
                     annotated_type
                     ~type_name:(Name_bindings.absolutify_type_name names)
                     ~effect_name:(Name_bindings.absolutify_effect_name names)))
            in
            let expr, inferred_type, expr_effects =
              of_untyped ~names ~types ~f_name expr
            in
            eprint_s
              [%lazy_message
                "type annotation constraint"
                  (inferred_type : Internal_type.t)
                  (annotated_type : Internal_type.t)];
            Type_bindings.constrain
              ~names
              ~types
              ~subtype:inferred_type
              ~supertype:annotated_type;
            expr, annotated_type, expr_effects)
      in
      eprint_s
        [%lazy_message
          "Expr.of_untyped"
            (result
              : (Internal_type.t * Pattern_names.t) t Node.t
                * Internal_type.t
                * Internal_type.effects)];
      result
    and type_recursive_let_bindings ~names ~types ~f_name ~add_effects bindings =
      let all_bound_names =
        Nonempty.fold
          bindings
          ~init:Pattern_names.empty
          ~f:(fun all_bound_names (pattern_etc, (_ : Fixity.t option), _expr) ->
          Node.with_value pattern_etc ~f:(fun (_, (_, pat_names)) ->
            let all_bound_names =
              Pattern_names.merge all_bound_names pat_names ~combine:(fun ~key:_ v _ -> v)
            in
            all_bound_names))
      in
      let used_a_bound_name = ref false in
      let current_path = Name_bindings.current_path names in
      let f_name name name_entry =
        f_name name name_entry;
        let path, name = name in
        if Module_path.Absolute.equal path current_path
           && Option.exists
                (Pattern_names.find all_bound_names name)
                ~f:(Name_bindings.Name_entry.identical name_entry)
        then used_a_bound_name := true
      in
      let bindings =
        Nonempty.map bindings ~f:(fun (pat, fixity, expr) ->
          let expr, expr_type, expr_effects = of_untyped expr ~f_name ~names ~types in
          eprint_s
            [%lazy_message
              "typed binding"
                (pat : (_ Pattern.t * (Internal_type.t * _)) Node.t)
                (expr_type : Internal_type.t)
                (expr_effects : Internal_type.effects)];
          add_effects expr_effects (Node.span expr);
          Node.with_value pat ~f:(fun (_, (pat_type, _)) ->
            Type_bindings.constrain ~names ~types ~subtype:expr_type ~supertype:pat_type);
          pat, fixity, expr)
      in
      let rec_ = !used_a_bound_name in
      names, rec_, bindings
    and type_non_recursive_let_bindings
      ~names:original_names
      ~types
      ~f_name
      ~add_effects
      bindings
      =
      Nonempty.fold_map
        bindings
        ~init:original_names
        ~f:(fun new_names (pat, fixity, expr) ->
        let pat_span = Node.span pat in
        let pat_names, (pat, pat_type) =
          Node.with_value
            pat
            ~f:
              (Pattern.of_untyped_with_names
                 ~names:original_names
                 ~types
                 ~fixity
                 Pattern_names.empty)
        in
        let new_names =
          Name_bindings.merge_names new_names pat_names ~combine:(fun _ _ entry -> entry)
        in
        (* FIXME: pretty sure we need this*)
        (* Type_bindings.record_context_vars types bound_names; *)
        let expr, expr_type, expr_effects =
          of_untyped ~names:original_names ~types ~f_name expr
        in
        add_effects expr_effects (Node.span expr);
        Type_bindings.constrain
          ~names:original_names
          ~types
          ~subtype:expr_type
          ~supertype:pat_type;
        new_names, (Node.create (pat, (pat_type, pat_names)) pat_span, fixity, expr))
    in
    type_recursive_let_bindings ~f_name:(fun _ _ -> ())
  ;;

  let rec map :
            'a 'b.
            'a t -> f:('a t -> ('a t, 'b t) Map_action.t) -> f_type:('a -> 'b) -> 'b t
    =
   fun expr ~f ~f_type ->
    match (f expr : _ Map_action.t) with
    | Halt expr -> expr
    | Retry expr -> map ~f ~f_type expr
    | Defer expr ->
      (match expr with
       | Let { rec_; bindings; body } ->
         let bindings =
           Nonempty.map bindings ~f:(fun (pat_and_type, fixity, expr) ->
             let pat_and_type =
               Node.map pat_and_type ~f:(fun (pat, typ) ->
                 Pattern.map_types pat ~f:f_type, f_type typ)
             in
             let expr = map' expr ~f ~f_type in
             pat_and_type, fixity, expr)
         in
         let body = map' body ~f ~f_type in
         Let { rec_; bindings; body }
       | (Literal _ | Name _) as expr -> expr
       | Fun_call (fun_, fun_type, args) ->
         let fun_ = map' fun_ ~f ~f_type in
         let fun_type = f_type fun_type in
         let args =
           Nonempty.map args ~f:(fun (arg, arg_type) ->
             let arg = map' arg ~f ~f_type in
             let arg_type = f_type arg_type in
             arg, arg_type)
         in
         Fun_call (fun_, fun_type, args)
       | Lambda (args, body) ->
         let args = Nonempty.map args ~f:(Node.map ~f:(Pattern.map_types ~f:f_type)) in
         let body = map' body ~f ~f_type in
         Lambda (args, body)
       | Match (expr, expr_type, branches) ->
         let expr = map' expr ~f ~f_type in
         let expr_type = f_type expr_type in
         let branches =
           Nonempty.map branches ~f:(fun (pat, expr) ->
             let pat = Node.map pat ~f:(Pattern.map_types ~f:f_type) in
             let expr = map' expr ~f ~f_type in
             pat, expr)
         in
         Match (expr, expr_type, branches)
       | Handle { expr; expr_type; value_branch; effect_branches } ->
         let expr = map' expr ~f ~f_type in
         let expr_type = f_type expr_type in
         let value_branch =
           Option.map value_branch ~f:(fun (pat_and_type, expr) ->
             let pat_and_type =
               Node.map pat_and_type ~f:(fun (pat, typ) ->
                 let pat = Pattern.map_types pat ~f:f_type in
                 let typ = f_type typ in
                 pat, typ)
             in
             let expr = map' expr ~f ~f_type in
             pat_and_type, expr)
         in
         let effect_branches =
           List.map effect_branches ~f:(fun (effect_branch, expr) ->
             let effect_branch =
               Node.map
                 effect_branch
                 ~f:(fun { effect_pattern; arg_types; resume_type } : _ Effect_branch.t ->
                 let effect_pattern = Effect_pattern.map_types effect_pattern ~f:f_type in
                 let arg_types = Nonempty.map arg_types ~f:f_type in
                 let resume_type = f_type resume_type in
                 { effect_pattern; arg_types; resume_type })
             in
             let expr = map' expr ~f ~f_type in
             effect_branch, expr)
         in
         Handle { expr; expr_type; value_branch; effect_branches }
       | Tuple fields -> Tuple (List.map fields ~f:(map' ~f ~f_type))
       | Record_literal fields ->
         Record_literal
           (List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map' ~f ~f_type))))
       | Record_update (expr, fields) ->
         let expr = map' ~f ~f_type expr in
         Record_update
           (expr, List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map' ~f ~f_type))))
       | Record_field_access (record, field) ->
         Record_field_access (map' ~f ~f_type record, field))

  and map' expr ~f ~f_type = Node.map expr ~f:(map ~f ~f_type)

  let map_types expr ~f = map expr ~f_type:f ~f:Map_action.defer

  (* TODO: Use this or remove it *)
  (* let rec fold_until expr ~init:acc ~f ~f_type =
    match (f acc expr : _ Fold_action.t) with
    | Stop _ as stop -> stop
    | Continue (`Halt acc) -> Continue acc
    | Continue (`Defer acc) ->
      (match expr with
       | Literal _ | Name (_, _) -> Continue acc
       | Let { rec_ = _; bindings; body } ->
         let%bind.Fold_action acc =
           Nonempty.fold_until bindings ~init:acc ~f:(fun acc (pat_and_type, expr) ->
             let acc =
               Node.with_value pat_and_type ~f:(fun ((_ : Pattern.t), type_) ->
                 f_type acc type_)
             in
             Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type))
         in
         Node.with_value body ~f:(fold_until ~init:acc ~f ~f_type)
       | Fun_call (fun_, fun_type, args) ->
         let%bind.Fold_action acc =
           Node.with_value fun_ ~f:(fold_until ~init:acc ~f ~f_type)
         in
         let acc = f_type acc fun_type in
         Nonempty.fold_until args ~init:acc ~f:(fun acc (arg, arg_type) ->
           let%map.Fold_action acc =
             Node.with_value arg ~f:(fold_until ~init:acc ~f ~f_type)
           in
           f_type acc arg_type)
       | Lambda ((_ : Pattern.t Node.t Nonempty.t), body) ->
         Node.with_value body ~f:(fold_until ~init:acc ~f ~f_type)
       | Match (expr, expr_type, branches) ->
         let%bind.Fold_action acc =
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type)
         in
         let acc = f_type acc expr_type in
         Nonempty.fold_until
           branches
           ~init:acc
           ~f:(fun acc ((_ : Pattern.t Node.t), expr) ->
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type))
       | Handle { expr; expr_type; value_branch; effect_branches } ->
         let%bind.Fold_action acc =
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type)
         in
         let acc = f_type acc expr_type in
         let%bind.Fold_action acc =
           Option.fold_until
             value_branch
             ~init:acc
             ~f:(fun acc ((_ : Pattern.t Node.t), expr) ->
             Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type))
         in
         List.fold_until
           effect_branches
           ~init:acc
           ~f:(fun acc ((_ : Effect_pattern.t Node.t), expr) ->
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type))
       | Tuple fields ->
         List.fold_until fields ~init:acc ~f:(fun acc expr ->
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type))
       | Record_literal fields ->
         List.fold_until fields ~init:acc ~f:(fun acc ((_ : Value_name.t), expr) ->
           Option.fold_until expr ~init:acc ~f:(fun acc expr ->
             Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type)))
       | Record_update (expr, fields) ->
         let%bind.Fold_action acc =
           Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type)
         in
         List.fold_until fields ~init:acc ~f:(fun acc ((_ : Value_name.t), expr) ->
           Option.fold_until expr ~init:acc ~f:(fun acc expr ->
             Node.with_value expr ~f:(fold_until ~init:acc ~f ~f_type)))
       | Record_field_access (record, (_ : Value_name.t Node.t)) ->
         Node.with_value record ~f:(fold_until ~init:acc ~f ~f_type))
  ;; *)

  (* TODO: Use this or remove it *)
  (* let fold_types t ~init ~f =
    fold_until t ~init ~f_type:f ~f:(fun init (_ : _ t) -> Continue (`Defer init))
    |> Fold_action.id
  ;; *)

  (* TODO: This is poorly named - it doesn't reflect the way generalization usually works
     in HM type inference. What it actually does it infer all the types up-front, then
     go back and "generalize" all the inferred types in one go. "Generalize" means to
     convert it from `Internal_type.t` to `Type_scheme.t` and do type simplification. *)
  let rec generalize_let_bindings ~names ~types =
    map
      ~f_type:(fun (typ, _) -> Type_bindings.generalize types typ)
      ~f:(function
        | Let { rec_; bindings; body } ->
          let bindings =
            Nonempty.map bindings ~f:(fun (pattern_etc, fixity, expr) ->
              let pat_span = Node.span pattern_etc in
              let names, pat, scheme =
                Node.with_value pattern_etc ~f:(fun (pat, (pat_type, pat_names)) ->
                  Pattern.generalize pat ~names ~types pat_names pat_type ~toplevel:false)
              in
              ( Node.create (pat, scheme) pat_span
              , fixity
              , Node.map expr ~f:(generalize_let_bindings ~names ~types) ))
          in
          let body = Node.map body ~f:(generalize_let_bindings ~names ~types) in
          Halt (Let { rec_; bindings; body })
        | expr -> Defer expr)
  ;;
end

module Let_binding_group = struct
  module Index : sig
    type t [@@deriving compare, sexp_of]

    val of_int : int -> t
  end = struct
    type t = int [@@deriving compare, sexp_of]

    let of_int = Fn.id
  end

  type t =
    { rec_ : bool
    ; bindings :
        (Pattern.generalized Node.t * Fixity.t option * Expr.generalized Node.t)
        Nonempty.t
    ; index : Index.t
    }
  [@@deriving sexp_of, fields]
end

module Module = struct
  include Module

  type nonrec t = (Let_binding_group.t, Module_path.absolute) t [@@deriving sexp_of]
  type nonrec def = (Let_binding_group.t, Module_path.absolute) def [@@deriving sexp_of]

  let rec gather_names ~names ~f_common ?f_sig ?f_def module_name sigs defs =
    let f_sig =
      match f_sig with
      | Some f_sig -> f_sig
      | None ->
        fun names sig_ ->
          Node.with_value sig_ ~f:(function
            | Common_sig common -> f_common names common
            | Val _ | Trait_sig _ -> names
            | Module_sig (module_name, sigs) ->
              gather_names ~names module_name sigs [] ~f_common ?f_def)
    in
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        List.fold sigs ~init:names ~f:f_sig)
    in
    let f_def =
      match f_def with
      | Some f_def -> f_def
      | None ->
        fun names def ->
          Node.with_value def ~f:(function
            | Common_def common -> f_common names common
            | Module (module_name, sigs, defs) ->
              gather_names ~names module_name sigs defs ~f_common ?f_def
            | Let _ | Trait _ | Impl _ -> names)
    in
    Name_bindings.with_submodule ~place:`Def names module_name ~f:(fun names ->
      List.fold defs ~init:names ~f:f_def)
  ;;

  module Sig_data : sig
    (** Represents data from a module signature that might want to be copied to its
        definition. Currently we only copy type definitions, but you could imagine copying
        more things like fixities from val/extern declarations, or just the whole
        val/extern declaration. If we were to implicitly copy mmore, we would have to
        think about the interaction with sig/def diffs and error messages. Keeping sigs
        and defs mostly separate is simpler to think about. *)
    type t

    val empty : t
    val add_type_decl : t -> type_name:Type_name.t -> def:Untyped.Module.def Node.t -> t

    val add_effect_decl
      :  t
      -> effect_name:Effect_name.t
      -> def:Untyped.Module.def Node.t
      -> t

    val remove_type_decl : t -> Type_name.t -> t
    val remove_effect_decl : t -> Effect_name.t -> t

    val fold_defs
      :  t
      -> init:'acc
      -> f:('acc -> Untyped.Module.def Node.t -> 'acc)
      -> 'acc
  end = struct
    type t =
      { type_decls : Untyped.Module.def Node.t Type_name.Map.t
      ; effect_decls : Untyped.Module.def Node.t Effect_name.Map.t
      }
    [@@deriving fields]

    let empty = { type_decls = Type_name.Map.empty; effect_decls = Effect_name.Map.empty }

    let add_internal (t : t) field ~key ~data =
      Field.map field t ~f:(fun map ->
        match Map.add map ~key ~data with
        | `Ok map -> map
        | `Duplicate -> compiler_bug [%message "Sig_data.add: duplicate value"])
    ;;

    let remove_internal (t : t) field key =
      Field.map field t ~f:(fun map -> Map.remove map key)
    ;;

    let add_type_decl t ~type_name ~def =
      add_internal t Fields.type_decls ~key:type_name ~data:def
    ;;

    let add_effect_decl t ~effect_name ~def =
      add_internal t Fields.effect_decls ~key:effect_name ~data:def
    ;;

    let remove_type_decl t type_name = remove_internal t Fields.type_decls type_name

    let remove_effect_decl t effect_name =
      remove_internal t Fields.effect_decls effect_name
    ;;

    let fold_defs { type_decls; effect_decls } ~init ~f =
      let init = Map.fold type_decls ~init ~f:(fun ~key:_ ~data:def acc -> f acc def) in
      Map.fold effect_decls ~init ~f:(fun ~key:_ ~data:def acc -> f acc def)
    ;;
  end

  (* Copy type and module declarations to defs if they were left out *)
  (* TODO: A problem with just copying values over from the sigs to the defs textually is
     that they might refer to different things due to imports. It's also just kind of
     re-implementing the features of [Name_bindings]. Doing the checks at the lookup stage
     does seem like a recipe for pain though. Maybe we could do this step *after*
     resolving imports and absolutifying everything? Actually, that would mess with our
     ability to know what names exist where before resolving imports, so it wouldn't
     quite work. Maybe we could include the values as placeholders in the def, and then
     fill their actual values in after resolving imports? Pretty complicated. *)
  let rec copy_some_sigs_to_defs sigs defs =
    let rec gather_decls ~sig_map sigs =
      List.fold sigs ~init:sig_map ~f:(fun sig_map sig_ ->
        Node.with_value sig_ ~f:(function
          | Common_sig common ->
            (match common with
             | Type_decl (type_name, _) ->
               let def = Node.set sig_ (Common_def common) in
               Nested_map.map sig_map ~f:(Sig_data.add_type_decl ~type_name ~def)
             | Effect (effect_name, _) ->
               let def = Node.set sig_ (Common_def common) in
               Nested_map.map sig_map ~f:(Sig_data.add_effect_decl ~effect_name ~def)
             | Extern _
             (* We could consider handling imports bringing in type declarations to copy
                over, but the cost-benefit of this feature isn't clear right now. *)
             | Import _ -> sig_map)
          | Val _ -> sig_map
          | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
          | Module_sig (module_name, sigs) ->
            Nested_map.with_module sig_map module_name ~f:(fun sig_map ->
              gather_decls
                ~sig_map:
                  (Option.value sig_map ~default:(Nested_map.create Sig_data.empty))
                sigs)))
    in
    let rec copy_to_defs ~sig_map defs =
      (* We want to add missing information from the sig to the def e.g. type declarations.
         This is done by removing matching entries from [sig_map] when found, then adding
         the remaining ones at the end. *)
      let sig_map, defs =
        List.fold_map defs ~init:sig_map ~f:(fun sig_map defs ->
          Node.fold_map sig_map defs ~f:(fun sig_map def ->
            match def with
            | Common_def common ->
              (match common with
               | Type_decl (type_name, _) ->
                 ( Nested_map.map sig_map ~f:(Fn.flip Sig_data.remove_type_decl type_name)
                 , def )
               | Effect (effect_name, _) ->
                 ( Nested_map.map
                     sig_map
                     ~f:(Fn.flip Sig_data.remove_effect_decl effect_name)
                 , def )
               | Extern _ | Import _ -> sig_map, def)
            | Module (module_name, sigs, defs) ->
              (match Nested_map.find_module sig_map module_name with
               | Some child_map ->
                 let sig_map = Nested_map.remove_module sig_map module_name in
                 if List.is_empty sigs
                 then
                   ( sig_map
                   , Module (module_name, sigs, copy_to_defs ~sig_map:child_map defs) )
                 else
                   (* Don't copy inherited sigs from the parent over (at least for now)
                      because it's complicated. However, we do want to copy the local sigs
                      to its defs here. *)
                   sig_map, Module (module_name, sigs, copy_some_sigs_to_defs sigs defs)
               | None ->
                 (* If there's no module in the signature, there's nothing to copy from
                    there. We still want to copy the local sigs to its defs here though. *)
                 sig_map, Module (module_name, sigs, copy_some_sigs_to_defs sigs defs))
            | Trait _ -> failwith "TODO: copy trait_sigs to defs, without overriding"
            | Let _ | Impl _ -> sig_map, def))
      in
      let defs =
        Sig_data.fold_defs (Nested_map.current sig_map) ~init:defs ~f:(fun defs def ->
          def :: defs)
      in
      (* Copy over modules and try to populate them with declarations *)
      Nested_map.fold_modules
        sig_map
        ~init:defs
        ~f:(fun ~key:module_name ~data:sig_map defs ->
        match copy_to_defs ~sig_map [] with
        | [] -> defs
        | defs' -> Node.dummy_span (Module (module_name, [], defs')) :: defs)
    in
    copy_to_defs
      ~sig_map:(gather_decls ~sig_map:(Nested_map.create Sig_data.empty) sigs)
      defs
  ;;

  (** Gather placeholders for all declared names and types.
      (Needed for imports of submodules to work.) *)
  let rec gather_name_placeholders ~names module_name sigs defs =
    let f_common names = function
      | Extern (name, _, _, _) -> Name_bindings.add_name_placeholder names name
      | Type_decl (type_name, decl) ->
        Name_bindings.add_type_decl_placeholder names type_name decl
      | Effect (effect_name, effect) ->
        Name_bindings.add_effect_placeholder names effect_name effect
      | Import _ -> names
    in
    gather_names
      ~names
      module_name
      sigs
      defs
      ~f_common
      ~f_sig:(fun names sig_ ->
        Node.with_value sig_ ~f:(function
          | Common_sig common -> f_common names common
          | Val (name, _, _) -> Name_bindings.add_name_placeholder names name
          | Trait_sig _ -> names
          | Module_sig (module_name, sigs) ->
            gather_name_placeholders ~names module_name sigs []))
      ~f_def:(fun names def ->
        Node.with_value def ~f:(function
          | Let bindings ->
            Nonempty.fold bindings ~init:names ~f:(fun names (pat, fixity, _expr) ->
              Node.with_value pat ~f:(fun pat ->
                Name_bindings.merge_names
                  names
                  (Pattern_names.gather
                     pat
                     ~type_source:Placeholder
                     ~fixity
                     ~fold:Untyped.Pattern.fold_names)
                  ~combine:(fun _ _ entry' -> entry')))
          | Module (module_name, sigs, defs) ->
            gather_name_placeholders ~names module_name sigs defs
          | Common_def common -> f_common names common
          | Trait _ | Impl _ -> failwith "TODO: trait/impl (gather_name_placeholders)"))
  ;;

  let import_mentions_prelude : Module.Import.t -> bool = function
    | { kind = Absolute; paths = Module (module_name, paths) } ->
      Module_name.equal module_name Intrinsics.std_module_name
      && Nonempty.exists paths ~f:(function
           | Module (module_name, _) ->
             Module_name.equal module_name Intrinsics.prelude_module_name
           | Name name | Name_as (name, _) | Name_excluded name ->
             Unidentified_name.equal
               name
               (Module_name.unidentify Intrinsics.prelude_module_name)
           | All -> true)
    | { kind = Absolute; paths = All | Name _ | Name_as _ | Name_excluded _ }
    | { kind = Relative _; paths = _ } -> false
  ;;

  let gather_imports ~names ~include_std module_name sigs defs =
    let import_mentioned_prelude = ref false in
    let names =
      gather_names ~names module_name sigs defs ~f_common:(fun names -> function
        | Import import ->
          if include_std && not !import_mentioned_prelude
          then import_mentioned_prelude := import_mentions_prelude import;
          Name_bindings.import names import
        | Extern _ | Type_decl _ | Effect _ -> names)
    in
    if !import_mentioned_prelude || not include_std
    then names
    else
      (* TODO: This always imports the prelude into all sigs and defs, even empty ones.
         [Sig_def_diff.check] then checks these for *every* module. We should really have
         a way to import bindings without "including" them. Maybe [include] and [import]
         or some @exporting annotation on an import or something. *)
      List.fold [ `Sig; `Def ] ~init:names ~f:(fun names place ->
        Name_bindings.with_submodule
          names
          ~place
          module_name
          ~f:(Fn.flip Name_bindings.import_all_absolute Intrinsics.prelude_module_path))
  ;;

  let absolutify_everything =
    let absolutify_common ~names common =
      match (common : _ Module.common) with
      | Extern (name, fixity, type_, extern_name) ->
        Extern
          (name, fixity, Name_bindings.absolutify_type_scheme names type_, extern_name)
      | Type_decl (type_name, decl) ->
        Type_decl (type_name, Name_bindings.absolutify_type_decl names decl)
      | Effect (effect_name, effect) ->
        Effect (effect_name, Name_bindings.absolutify_effect names effect)
      | Import _ as common -> common
    in
    let rec absolutify_sigs ~names sigs =
      List.map sigs ~f:(fun sig_ ->
        Node.map sig_ ~f:(function
          | Common_sig common -> Common_sig (absolutify_common ~names common)
          | Val (name, fixity, type_) ->
            Val (name, fixity, Name_bindings.absolutify_type_scheme names type_)
          | Trait_sig _ -> failwith "absolutify_everything: traits"
          | Module_sig (module_name, sigs) ->
            Module_sig
              ( module_name
              , absolutify_sigs
                  ~names:(Name_bindings.into_module names ~place:`Sig module_name)
                  sigs )))
    in
    let rec absolutify_defs ~names defs =
      List.map defs ~f:(fun sig_ ->
        Node.map sig_ ~f:(function
          | Common_def common -> Common_def (absolutify_common ~names common)
          | Let _ as def -> def
          | Module (module_name, sigs, defs) ->
            Module
              ( module_name
              , absolutify_sigs
                  ~names:(Name_bindings.into_module names ~place:`Sig module_name)
                  sigs
              , absolutify_defs
                  ~names:(Name_bindings.into_module names ~place:`Def module_name)
                  defs )
          | Trait _ | Impl _ -> failwith "absolutify_everything: traits and impls"))
    and absolutify_module ~names module_name sigs defs =
      ( absolutify_sigs
          ~names:(Name_bindings.into_module names ~place:`Sig module_name)
          sigs
      , absolutify_defs
          ~names:(Name_bindings.into_module names ~place:`Def module_name)
          defs )
    in
    absolutify_module
  ;;

  let gather_type_decls ~names sigs defs =
    gather_names ~names sigs defs ~f_common:(fun names -> function
      | Type_decl (type_name, decl) -> Name_bindings.add_type_decl names type_name decl
      | Effect (effect_name, effect) -> Name_bindings.add_effect names effect_name effect
      | Extern _ | Import _ -> names)
  ;;

  (** Raise an error upon finding any cycles in a given type alias. *)
  let check_cyclic_type_alias ~names alias =
    (* TODO: can rewrite this with [Type.Expr.map] *)
    (* TODO: This could stop checking for cyclic aliases early if it reaches a type in
       another file. (There should be a separate check for cyclic imports.) *)
    let rec loop ~names ~aliases_seen (alias : Module_path.absolute Type_scheme.type_) =
      match alias with
      | Type_app (name, args) ->
        let type_entry = Name_bindings.find_absolute_type_entry names name in
        let decl = Name_bindings.Type_entry.decl type_entry in
        (match snd decl with
         | Alias alias ->
           let id = Name_bindings.Type_entry.id type_entry in
           if Set.mem aliases_seen id
           then
             Compilation_error.raise
               Type_error
               ~msg:
                 [%message
                   "Cyclic type alias"
                     (name : Type_name.Absolute.t)
                     (decl : Module_path.absolute Type_decl.t)]
           else loop ~names ~aliases_seen:(Set.add aliases_seen id) alias
         | Abstract | Variants _ | Record _ -> ());
        List.iter args ~f:(loop ~names ~aliases_seen)
      | Function (args, effects, body) ->
        Nonempty.iter args ~f:(loop ~names ~aliases_seen);
        loop_effects effects ~names ~aliases_seen;
        loop ~names ~aliases_seen body
      | Tuple items -> List.iter items ~f:(loop ~names ~aliases_seen)
      | Union items | Intersection items ->
        Non_single_list.iter items ~f:(loop ~names ~aliases_seen)
      | Var _ -> ()
    and loop_effects
      ~names
      ~aliases_seen
      (effects : Module_path.absolute Type_scheme.effects)
      =
      match effects with
      | Effect ((_ : Effect_name.Absolute.t), args) ->
        List.iter args ~f:(loop ~names ~aliases_seen)
      | Effect_union effects ->
        Non_single_list.iter effects ~f:(loop_effects ~names ~aliases_seen)
      | Effect_var _ -> ()
    in
    loop ~names ~aliases_seen:Name_bindings.Type_entry.Id.Set.empty alias
  ;;

  (** Types for intermediate stages of type-checking, after resolving names, but before
      inferring types. *)
  module Intermediate = struct
    type binding =
      (Untyped.Pattern.t * Pattern_names.t) Node.t
      * Fixity.t option
      * Untyped.Expr.t Node.t

    type let_binding_group =
      { rec_ : bool
      ; bindings : binding Nonempty.t
      }

    type def = (let_binding_group, Module_path.absolute) Module.def
  end

  (** Handle all `val` and `let` statements (value bindings/type annotations).
      Also type the patterns in each let binding and assign the names fresh type
      variables. *)
  let rec handle_value_bindings
    ~names
    module_name
    sigs
    (defs : (Untyped.Let_binding_group.t, Module_path.absolute) Module.def Node.t list)
    : Name_bindings.t * Intermediate.def Node.t list
    =
    let handle_common ~names = function
      | Extern (name, fixity, typ, extern_name) ->
        Name_bindings.add_extern names name fixity typ extern_name
      | Type_decl (_, (_, Alias alias)) ->
        check_cyclic_type_alias ~names alias;
        names
      | Effect _ ->
        (* TODO: Check for recursive effect type aliases when those are implemented. *)
        names
      | Type_decl _ | Import _ -> names
    in
    let rec handle_sigs ~names ~handle_common =
      List.fold ~init:names ~f:(fun names sig_ ->
        Node.with_value sig_ ~f:(function
          | Common_sig common -> handle_common ~names common
          | Val (name, fixity, typ) -> Name_bindings.add_val names name fixity typ
          | Trait_sig _ -> names
          | Module_sig (module_name, sigs) ->
            Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
              handle_sigs ~names ~handle_common sigs)))
    in
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        handle_sigs ~names ~handle_common sigs)
    in
    let handle_bindings ~names =
      Nonempty.fold_map ~init:names ~f:(fun names (pat, fixity, expr) ->
        let pat =
          Node.map pat ~f:(fun pat ->
            let pat_names =
              Pattern_names.gather
                pat
                ~type_source:Placeholder
                ~fixity
                ~fold:Untyped.Pattern.fold_names
            in
            pat, pat_names)
        in
        names, (pat, fixity, expr))
    in
    Name_bindings.with_submodule' ~place:`Def names module_name ~f:(fun names ->
      List.fold_map defs ~init:names ~f:(fun def ->
        Node.fold_map def ~f:(fun names -> function
          | Let bindings ->
            handle_bindings ~names bindings
            |> Tuple2.map_snd ~f:(fun bindings ->
                 Let { Intermediate.rec_ = true; bindings })
          | Module (module_name, sigs, defs) ->
            let names, defs = handle_value_bindings ~names module_name sigs defs in
            names, Module (module_name, sigs, defs)
          | Common_def common as def -> handle_common ~names common, def
          | Impl _ | Trait _ -> failwith "TODO: handle_value_bindings traits/impls")))
  ;;

  (** Re-group and re-order toplevel let bindings so that each group is mutually
      recursive, and the groups are given in an order appropriate for generalization.
      This is done by topologically sorting the strongly-connected components of the call
      graph (dependencies between bindings). *)
  let extract_binding_groups ~names (defs : Intermediate.def Node.t list) =
    let rec gather_bindings_in_defs ~names defs acc =
      List.fold_right defs ~init:acc ~f:(gather_bindings ~names)
    and gather_bindings ~names (def : Intermediate.def Node.t) (other_defs, bindings) =
      Node.with_value def ~f:(function
        | Let { bindings = bindings'; rec_ } ->
          (* All toplevel bindings are initially considered to be recursive. If found to
             be non-recursive, we will set `rec_ = false` later. *)
          assert_or_compiler_bug ~here:[%here] rec_;
          ( other_defs
          , Nonempty.fold_right
              bindings'
              ~init:bindings
              ~f:(fun (pat, fixity, expr) bindings ->
              let pat_names = Node.with_value pat ~f:snd in
              let current_path = Name_bindings.current_path names in
              let bound_names = Map.key_set pat_names in
              let used_names = Node.with_value expr ~f:(Untyped.Expr.names_used ~names) in
              ( { Call_graph.Binding.bound_names; used_names; info = pat, fixity, expr }
              , current_path )
              :: bindings) )
        | Module (module_name, sigs, defs) ->
          let names = Name_bindings.into_module ~place:`Def names module_name in
          let other_defs', bindings =
            gather_bindings_in_defs ~names defs ([], bindings)
          in
          Node.set def (Module (module_name, sigs, other_defs')) :: other_defs, bindings
        | Common_def _ as node -> Node.set def node :: other_defs, bindings
        | Trait _ | Impl _ -> failwith "TODO: traits/impls")
    in
    let other_defs, bindings = gather_bindings_in_defs ~names defs ([], []) in
    let binding_groups = Call_graph.regroup_bindings bindings in
    other_defs, binding_groups
  ;;

  let rename_type_vars_in_bindings bindings =
    let map_var =
      let generator = Type_param.Generator.create () in
      let vars = Type_param.Table.create () in
      Hashtbl.find_or_add vars ~default:(fun () -> Type_param.Generator.next generator)
    in
    let map_type : 'a. 'a Type_scheme.t -> 'a Type_scheme.t =
      Tuple2.map_fst ~f:(Type_scheme.map_vars ~f:map_var)
    in
    Nonempty.map bindings ~f:(fun (pat, fixity, expr_and_scheme) ->
      ( pat
      , fixity
      , Node.map expr_and_scheme ~f:(fun (expr, scheme) ->
          let scheme = map_type scheme in
          let expr = Expr.map_types expr ~f:map_type in
          expr, scheme) ))
  ;;

  let type_binding_group ~names ~index (bindings : Intermediate.binding Nonempty.t) =
    (* Order bindings in the group by span, then take the first span to represent the
       whole group. This is done to get a consistent ordering among bindings. *)
    let get_spans (pat, (_ : Fixity.t option), expr) = Node.span pat, Node.span expr in
    let bindings =
      Nonempty.sort
        bindings
        ~compare:(Comparable.lift [%compare: Span.t * Span.t] ~f:get_spans)
    in
    let representative_span =
      get_spans (Nonempty.hd bindings) |> Tuple2.uncurry Span.combine
    in
    let types = Type_bindings.create () in
    let names, bindings =
      Nonempty.fold_map bindings ~init:names ~f:(fun names (pat, fixity, expr) ->
        let pat_span = Node.span pat in
        let pat_names, (pat, pat_type) =
          Node.with_value pat ~f:(fun (pat, (_ : Pattern_names.t)) ->
            (* TODO: Maybe we could reuse the names from before? *)
            Pattern.of_untyped_with_names ~fixity ~names ~types Pattern_names.empty pat)
        in
        let names =
          (* TODO: I'm not sure why we merge here instead of shadowing. It might be wrong. *)
          Name_bindings.merge_names
            names
            pat_names
            ~combine:(fun (_ : Value_name.t) old_entry new_entry ->
            Name_bindings.Name_entry.merge old_entry new_entry)
        in
        let pat = Node.create (pat, (pat_type, pat_names)) pat_span in
        names, (pat, fixity, expr))
    in
    let (_ : Name_bindings.t), rec_, bindings =
      Expr.type_recursive_let_bindings
        ~names
        ~types
        bindings
        ~add_effects:(fun effects span ->
        (* TODO: Allow handlers to be installed at the toplevel. *)
        (* No effects are handled at toplevel, so get rid of any produced effects. *)
        Node.with_value (Node.create effects span) ~f:(fun effects ->
          Type_bindings.constrain_effects_to_be_total ~names ~types effects))
    in
    let names, bindings =
      Nonempty.fold_map bindings ~init:names ~f:(fun names (pattern_etc, fixity, expr) ->
        let pat_span = Node.span pattern_etc in
        let names, pat, scheme =
          Node.with_value pattern_etc ~f:(fun (pat, (pat_type, pat_names)) ->
            Pattern.generalize pat ~names ~types pat_names pat_type ~toplevel:true)
        in
        (* Generalize local let bindings just after the parent binding *)
        let expr_and_scheme =
          Node.map expr ~f:(fun expr ->
            let expr = Expr.generalize_let_bindings expr ~names ~types in
            expr, scheme)
        in
        names, (Node.create pat pat_span, fixity, expr_and_scheme))
    in
    (* TODO: This renames the vars in the AST but _not_ in the [Name_bindings]. *)
    let bindings = rename_type_vars_in_bindings bindings in
    names, Node.create (Let { rec_; bindings; index } : def) representative_span
  ;;

  (** Reintegrate the re-ordered binding groups from [extract_binding_groups] back into
      the AST. *)
  let reintegrate_binding_groups
    (path : Module_path.Absolute.t)
    (other_defs : def Node.t list)
    (binding_groups : (def Node.t * Module_path.Absolute.t) list)
    : def Node.t list
    =
    (* NOTE: As we disallow cross-module mutual recursion, binding groups will always be
       contained within a single module and can just be put back into the AST *)
    let binding_table = Module_path.Absolute.Table.create () in
    List.iter binding_groups ~f:(fun (def, path) ->
      Hashtbl.add_multi binding_table ~key:path ~data:def);
    let rec loop binding_table path defs =
      let defs =
        List.map
          defs
          ~f:
            (Node.map ~f:(function
              | Module (module_name, sigs, defs) ->
                let path' = Module_path.append path [ module_name ] in
                Module (module_name, sigs, loop binding_table path' defs)
              | def -> def))
      in
      (* Sort defs by span to get them back into their original order *)
      List.sort
        (Hashtbl.find_multi binding_table path @ defs)
        ~compare:(Comparable.lift Span.compare ~f:Node.span)
    in
    loop binding_table path other_defs
  ;;

  (** Type-check the expressions (definitions) for each let binding and trait implementation.
      Performs let-generalization on free variables in let statement bindings.
      (Let-generalization is not currently done for local let bindings within expressions.)
      This is the final step in the type checking process. *)
  let type_defs ~names module_name (defs : Intermediate.def Node.t list)
    : Name_bindings.t * def Node.t list
    =
    Name_bindings.with_submodule' ~place:`Def names module_name ~f:(fun names ->
      let other_defs, binding_groups = extract_binding_groups ~names defs in
      let names, binding_groups =
        List.fold_mapi binding_groups ~init:names ~f:(fun i names (bindings, path) ->
          Name_bindings.with_path_into_defs names path ~f:(fun names ->
            let names, def =
              type_binding_group ~names ~index:(Let_binding_group.Index.of_int i) bindings
            in
            names, (def, path)))
      in
      let path = Name_bindings.current_path names in
      let defs = reintegrate_binding_groups path other_defs binding_groups in
      let rec check_def_modules ~names defs =
        List.iter defs ~f:(fun def ->
          Node.with_value def ~f:(function
            | Module (module_name, _, defs) ->
              check_def_modules
                ~names:(Name_bindings.into_module names module_name ~place:`Def)
                defs;
              Sig_def_diff.check ~names module_name
            | Common_def _ | Let _ | Trait _ | Impl _ -> ()))
      in
      check_def_modules ~names defs;
      names, defs)
  ;;

  let of_untyped ~names ~include_std (module_name, sigs, defs) =
    try
      let defs = copy_some_sigs_to_defs sigs defs in
      let names = gather_name_placeholders ~names module_name sigs defs in
      let names = gather_imports ~names ~include_std module_name sigs defs in
      let sigs, defs = absolutify_everything ~names module_name sigs defs in
      let names = gather_type_decls ~names module_name sigs defs in
      let names, defs = handle_value_bindings ~names module_name sigs defs in
      let names, defs = type_defs ~names module_name defs in
      eprint_s [%lazy_message "Finished type-checking. Checking sig-def compatbility."];
      Sig_def_diff.check ~names module_name;
      Ok (names, (module_name, sigs, defs))
    with
    | Compilation_error.Compilation_error error ->
      Error { error with backtrace = Some (Backtrace.Exn.most_recent ()) }
  ;;
end
