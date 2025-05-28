open! Core
open! Import
open Names

let relativize_name_internal ~names name ~f ~to_string =
  Option.value_or_thunk (f names name) ~default:(fun () ->
    raise_s
      [%message
        "Failed to relativize name, this could be expected due to shadowing"
          ~name:(to_string name : string)])
;;

let relativize_value_name =
  relativize_name_internal
    ~f:Name_bindings.relativize_value_name
    ~to_string:Value_name.Absolute.to_string
;;

let relativize_type_name =
  relativize_name_internal
    ~f:Name_bindings.relativize_type_name
    ~to_string:Type_name.Absolute.to_string
;;

let relativize_effect_name =
  relativize_name_internal
    ~f:Name_bindings.relativize_effect_name
    ~to_string:Effect_name.Absolute.to_string
;;

let relativize_cnstr_name ~names (path, name) =
  relativize_value_name ~names (path, Value_name.of_cnstr_name name)
  |> Tuple2.map_snd ~f:(ok_exn << Value_name.to_cnstr_name)
;;

let relativize_type ~names type_ =
  Type_scheme.map
    type_
    ~type_name:(relativize_type_name ~names)
    ~effect_name:(relativize_effect_name ~names)
;;

let relativize_type' ~names type_ =
  Type_scheme.map'
    type_
    ~type_name:(relativize_type_name ~names)
    ~effect_name:(relativize_effect_name ~names)
;;

let rec relativize_pattern ~names (pattern : Typed_ast.Pattern.generalized)
  : Untyped_ast.Pattern.t
  =
  match pattern with
  | Constant lit -> Constant lit
  | Catch_all name -> Catch_all name
  | As (pattern, name) -> As (relativize_pattern ~names pattern, name)
  | Cnstr_appl (cnstr, args) ->
    Cnstr_appl
      ( relativize_cnstr_name ~names cnstr
      , List.map args ~f:(fun (arg, _type) -> relativize_pattern ~names arg) )
  | Tuple fields ->
    Tuple (List.map fields ~f:(fun (field, _type) -> relativize_pattern ~names field))
  | Record fields ->
    Record
      (Nonempty.map
         fields
         ~f:(Tuple2.map_snd ~f:(Option.map ~f:(relativize_pattern ~names))))
  | Union (left, right) ->
    Union (relativize_pattern ~names left, relativize_pattern ~names right)
;;

let annotated_pattern
  ~names
  (pattern : Typed_ast.Pattern.generalized)
  (type_ : Module_path.relative Type_scheme.type_)
  : Untyped_ast.Pattern.t
  =
  match pattern with
  | Tuple fields ->
    Tuple
      (List.map fields ~f:(fun (field, field_type) ->
         Untyped_ast.Pattern.Type_annotation
           (relativize_pattern ~names field, relativize_type' ~names field_type)))
  | Cnstr_appl (cnstr, args) ->
    Cnstr_appl
      ( relativize_cnstr_name ~names cnstr
      , List.map args ~f:(fun (arg, arg_type) ->
          Untyped_ast.Pattern.Type_annotation
            (relativize_pattern ~names arg, relativize_type' ~names arg_type)) )
  | Constant _ | Catch_all _ | As _ | Record _ | Union _ ->
    Type_annotation (relativize_pattern ~names pattern, (type_, []))
;;

let rec convert_expr
  ?(should_annotate = true)
  ~names
  (expr : Module_path.absolute Type_scheme.t Typed_ast.Expr.t)
  ~(type_ : Module_path.relative Type_scheme.t)
  : Untyped_ast.Expr.t
  =
  let annotated expr =
    if should_annotate
    then (
      match fst type_ with
      | Tuple [] ->
        (* On balance, annotating unit `()` types tends to reduce readability and isn't
             particularly necessary to see. *)
        expr
      | _ -> Untyped_ast.Expr.Type_annotation (Node.dummy_span expr, Node.dummy_span type_))
    else expr
  in
  match expr with
  | Literal lit -> Literal lit
  | Name name -> Name (relativize_value_name ~names name)
  | Fun_call (fun_, fun_type, args) ->
    annotated
      (Fun_call
         ( Node.map fun_ ~f:(convert_expr ~names ~type_:(relativize_type' ~names fun_type))
         , Nonempty.map args ~f:(fun (arg, arg_type) ->
             Node.map arg ~f:(fun arg ->
               convert_expr arg ~names ~type_:(relativize_type' ~names arg_type))) ))
  | Lambda (args, body) ->
    let arg_types, body_type =
      match fst type_ with
      | Function (arg_types, _, body_type) -> arg_types, body_type
      | _ ->
        compiler_bug [%message "Unexpected type for function" (type_ : _ Type_scheme.t)]
    in
    let convert_lambda () =
      annotated
        (Lambda
           ( Nonempty.map2 args arg_types ~f:(fun arg arg_type ->
               Node.map arg ~f:(fun arg ->
                 if should_annotate
                 then annotated_pattern ~names arg arg_type
                 else relativize_pattern ~names arg))
           , Node.map body ~f:(convert_expr ~names ~type_:(body_type, [])) ))
    in
    (* Handle `match` functions with their anonymous argument. *)
    (match args, Node.with_value body ~f:Fn.id with
     | [ arg ], Match (expr, _expr_type, branches) ->
       Node.with_value2 arg expr ~f:(fun arg expr ->
         match arg, expr with
         | Catch_all (Some name), Name (_, name')
           when Value_name.equal name Constant_names.match_ && Value_name.equal name name'
           ->
           annotated
             (Match_function
                (Nonempty.map branches ~f:(fun (pattern, branch) ->
                   ( Node.map pattern ~f:(relativize_pattern ~names)
                   , Node.map
                       branch
                       ~f:
                         (convert_expr
                            ~names
                            ~type_:(body_type, [])
                            ~should_annotate:false) ))))
         | _ -> convert_lambda ())
     | _ -> convert_lambda ())
  | Match (expr, expr_type, branches) ->
    let expr_type = relativize_type' ~names expr_type in
    annotated
      (Match
         ( Node.map expr ~f:(convert_expr ~names ~type_:expr_type)
         , Nonempty.map branches ~f:(fun (pattern, branch) ->
             ( Node.map pattern ~f:(relativize_pattern ~names)
             , Node.map branch ~f:(convert_expr ~names ~type_ ~should_annotate:false) ))
         ))
  | Handle { expr; expr_type; value_branch; effect_branches } ->
    let branches =
      List.map effect_branches ~f:(fun (effect_pattern, branch) ->
        ( Node.map effect_pattern ~f:(fun { effect_pattern = { operation; args }; _ } ->
            `Effect
              { Untyped_ast.Effect_pattern.operation =
                  relativize_value_name ~names operation
              ; args = Nonempty.map args ~f:(Node.map ~f:(relativize_pattern ~names))
              })
        , Node.map branch ~f:(convert_expr ~names ~type_ ~should_annotate:false) ))
    in
    let branches : _ Nonempty.t =
      match value_branch with
      | Some (pattern, branch) ->
        let value_branch =
          ( Node.map pattern ~f:(fun (pattern, _type_) ->
              (* TODO: Use the type here in an annotation *)
              `Value (relativize_pattern ~names pattern))
          , Node.map branch ~f:(convert_expr ~names ~type_) )
        in
        value_branch :: branches
      | None ->
        (match Nonempty.of_list branches with
         | Some branches -> branches
         | None -> compiler_bug [%message "Handle expression with no branches"])
    in
    annotated
      (Handle
         ( Node.map
             expr
             ~f:(convert_expr ~names ~type_:(relativize_type' ~names expr_type))
         , branches ))
  | Let { rec_; bindings; body } ->
    let convert_let_bindings () : Untyped_ast.Expr.t =
      Let
        { rec_
        ; bindings =
            Nonempty.map bindings ~f:(fun (pat_and_type, fixity, expr) ->
              let pattern, type_ = Node.with_value pat_and_type ~f:Fn.id in
              let type_ = relativize_type' ~names type_ in
              ( Node.create
                  (annotated_pattern ~names pattern (fst type_))
                  (Node.span pat_and_type)
              , fixity
              , Node.map expr ~f:(convert_expr ~names ~type_ ~should_annotate:false) ))
        ; body = Node.map body ~f:(convert_expr ~names ~type_)
        }
    in
    (* Identify op sections. *)
    (* TODO: This is pretty hacky. Maybe we could bite the bullet and just model op
         sections explicitly in the typed ast? It might make sense to have a simplified
         typed IR we only use as an extra step before MIR (or replacing MIR). *)
    (match bindings, Node.with_value body ~f:Fn.id with
     | [ (outer_pattern, None, outer_expr) ], Lambda ([ inner_pattern ], body) ->
       let outer_pattern, outer_type = Node.with_value outer_pattern ~f:Fn.id in
       let inner_pattern = Node.with_value inner_pattern ~f:Fn.id in
       let body = Node.with_value body ~f:Fn.id in
       (match outer_pattern, inner_pattern, body with
        | ( Catch_all (Some outer_name)
          , Catch_all (Some _inner_name)
          , Fun_call (fun_, _fun_type, [ _; _ ]) ) ->
          (match Node.with_value fun_ ~f:Fn.id with
           | Name fun_name ->
             let op_side =
               if Value_name.equal outer_name (Constant_names.synthetic_arg 0)
               then Some `Right
               else if Value_name.equal outer_name (Constant_names.synthetic_arg 1)
               then Some `Left
               else None
             in
             (match op_side with
              | Some op_side ->
                Op_section
                  { op_side
                  ; op =
                      Node.create (relativize_value_name ~names fun_name) (Node.span fun_)
                  ; expr =
                      Node.map
                        outer_expr
                        ~f:
                          (convert_expr
                             ~names
                             ~type_:(relativize_type' ~names outer_type))
                  }
              | None -> convert_let_bindings ())
           | _ -> convert_let_bindings ())
        | _ -> convert_let_bindings ())
     | _ -> convert_let_bindings ())
  | Tuple fields ->
    let field_types =
      match fst type_ with
      | Tuple field_types -> field_types
      | _ -> compiler_bug [%message "Unexpected type for tuple" (type_ : _ Type_scheme.t)]
    in
    Tuple
      (List.map2_exn fields field_types ~f:(fun field field_type ->
         Node.map field ~f:(fun field ->
           convert_expr ~names field ~type_:(field_type, []))))
  | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
    (* TODO: Implement this *)
    failwith "TODO: Typed -> Untyped AST for record expressions"
;;

let convert_module =
  let rec convert_common ~names
    : Module_path.absolute Module.common -> Module_path.relative Module.common
    = function
    | Extern (name, fixity, type_, extern_name) ->
      Extern (name, fixity, relativize_type' ~names type_, extern_name)
    | Type_decl (type_name, decl) ->
      Type_decl (type_name, Type_decl.map_exprs decl ~f:(relativize_type ~names))
    | Effect (effect_name, effect) ->
      Effect (effect_name, Effect.map_exprs effect ~f:(relativize_type ~names))
    | Import import -> Import import
  and convert_sig ~names
    : Module_path.absolute Module.sig_ -> Module_path.relative Module.sig_
    = function
    | Common_sig common -> Common_sig (convert_common ~names common)
    | Val (name, fixity, type_) -> Val (name, fixity, relativize_type' ~names type_)
    | Trait_sig (trait_name, params, sigs) ->
      Trait_sig (trait_name, params, List.map sigs ~f:(Node.map ~f:(convert_sig ~names)))
    | Module_sig (module_name, sigs) ->
      let names = Name_bindings.into_module names module_name ~place:`Sig in
      Module_sig (module_name, List.map sigs ~f:(Node.map ~f:(convert_sig ~names)))
  and convert_def ~names : Typed_ast.Module.def -> Untyped_ast.Module.def = function
    | Common_def common -> Common_def (convert_common ~names common)
    | Let { rec_ = _; bindings; index = _ } ->
      Let
        (Nonempty.map bindings ~f:(fun (pattern, fixity, expr_and_type) ->
           let expr, type_ = Node.with_value expr_and_type ~f:Fn.id in
           let type_ = relativize_type' ~names type_ in
           ( Node.map pattern ~f:(fun pattern ->
               annotated_pattern ~names pattern (fst type_))
           , fixity
           , Node.create
               (convert_expr expr ~names ~type_ ~should_annotate:false)
               (Node.span expr_and_type) )))
    | Module (module_name, sigs, defs) ->
      let names = Name_bindings.into_module names module_name ~place:`Def in
      Module
        ( module_name
        , List.map sigs ~f:(Node.map ~f:(convert_sig ~names))
        , List.map defs ~f:(Node.map ~f:(convert_def ~names)) )
    | Trait (trait_name, params, sigs, defs) ->
      Trait
        ( trait_name
        , params
        , List.map sigs ~f:(Node.map ~f:(convert_sig ~names))
        , List.map defs ~f:(Node.map ~f:(convert_def ~names)) )
    | Impl (trait_bound, trait_name, args, defs) ->
      Impl
        ( trait_bound
        , trait_name
        , Nonempty.map args ~f:(relativize_type ~names)
        , List.map defs ~f:(Node.map ~f:(convert_def ~names)) )
  in
  fun ~names ((module_name, sigs, defs) : Typed_ast.Module.t) : Untyped_ast.Module.t ->
    ( module_name
    , List.map
        sigs
        ~f:
          (Node.map
             ~f:
               (convert_sig
                  ~names:(Name_bindings.into_module names module_name ~place:`Sig)))
    , List.map
        defs
        ~f:
          (Node.map
             ~f:
               (convert_def
                  ~names:(Name_bindings.into_module names module_name ~place:`Def))) )
;;

let%expect_test "match function" =
  let node = Node.dummy_span in
  let untyped : Untyped_ast.Module.t =
    let def : Untyped_ast.Module.def =
      Let
        [ ( node (Untyped_ast.Pattern.Catch_all (Some (Value_name.of_string_exn "foo")))
          , None
          , node
              (Untyped_ast.Expr.Match_function
                 [ ( node (Untyped_ast.Pattern.Catch_all None)
                   , node (Untyped_ast.Expr.Tuple []) )
                 ]) )
        ]
    in
    Module_name.of_string_exn "MatchFunction", [], [ node def ]
  in
  let typed =
    match
      Typed_ast.Module.of_untyped ~names:Name_bindings.core ~include_std:false untyped
    with
    | Ok (_names, typed) -> typed
    | Error error -> raise (Compilation_error.Compilation_error error)
  in
  print_s [%sexp (convert_module ~names:Name_bindings.core typed : Untyped_ast.Module.t)];
  [%expect
    {|
      (MatchFunction ()
       ((Let
         (((Type_annotation (Catch_all (foo))
            ((Function ((Intersection ())) (Effect_union ()) (Tuple ())) ()))
           () (Match_function (((Catch_all ()) (Tuple ()))))))))) |}]
;;
