open! Import
open Names

(* FIXME: Define functions to:
   1) convert from typed ast to untyped ast with annotations
   2) pretty-print the untyped ast back into code (like a formatter would). 
   
   This is basically just implementing a code formatter, but shouldn't be too hard. *)

let typed_ast_to_untyped_annotated_module ((module_name, sigs, defs) : Typed.Module.t)
  : Untyped.Module.t
  =
  (* NOTE: It isn't semantics-preserving to interpret an absolute path as a relative path
     due to shadowing. This is kinda fine for the purposes of pretty-printing the AST
     for test output or error messages. *)
  let relativize_name =
    Tuple2.map_fst ~f:(fun (path : Module_path.Absolute.t) ->
      Module_path.Relative.of_module_names (path :> Module_name.t list))
  in
  let relativize_type type_ =
    Type_scheme.map type_ ~type_name:relativize_name ~effect_name:relativize_name
  in
  let relativize_type' type_ =
    Type_scheme.map' type_ ~type_name:relativize_name ~effect_name:relativize_name
  in
  let rec relativize_pattern : Typed.Pattern.t -> Untyped.Pattern.t = function
    | (Constant _ | Catch_all _) as pattern -> pattern
    | As (pattern, name) -> As (relativize_pattern pattern, name)
    | Cnstr_appl (cnstr, args) ->
      Cnstr_appl (relativize_name cnstr, List.map args ~f:relativize_pattern)
    | Tuple fields -> Tuple (List.map fields ~f:relativize_pattern)
    | Record fields ->
      Record
        (Nonempty.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:relativize_pattern)))
    | Union (left, right) -> Union (relativize_pattern left, relativize_pattern right)
    | Type_annotation _ -> .
  in
  let rec handle_expr
    ?(should_annotate = true)
    (expr : Module_path.absolute Type_scheme.t Typed.Expr.t)
    ~type_
    : Untyped.Expr.t
    =
    let annotated expr =
      if should_annotate
      then Untyped.Expr.Type_annotation (Node.dummy_span expr, Node.dummy_span type_)
      else expr
    in
    match expr with
    | Literal lit -> Literal lit
    | Name name -> Name (relativize_name name)
    | Fun_call (fun_, fun_type, args) ->
      annotated
        (Fun_call
           ( Node.map fun_ ~f:(handle_expr ~type_:(relativize_type' fun_type))
           , Nonempty.map args ~f:(fun (arg, arg_type) ->
               Node.map arg ~f:(fun arg ->
                 handle_expr arg ~type_:(relativize_type' arg_type))) ))
    | Lambda (args, body) ->
      let arg_types, body_type =
        match fst type_ with
        | Function (arg_types, _, body_type) -> arg_types, body_type
        | _ ->
          compiler_bug [%message "Unexpected type for function" (type_ : _ Type_scheme.t)]
      in
      annotated
        (Lambda
           ( Nonempty.map2 args arg_types ~f:(fun arg arg_type ->
               Node.map arg ~f:(fun arg ->
                 Pattern.Type_annotation (relativize_pattern arg, (arg_type, []))))
           , Node.map body ~f:(handle_expr ~type_:(body_type, [])) ))
    | Match (expr, expr_type, branches) ->
      annotated
        (Match
           ( Node.map expr ~f:(handle_expr ~type_:(relativize_type' expr_type))
           , Nonempty.map branches ~f:(fun (pattern, branch) ->
               ( Node.map pattern ~f:relativize_pattern
               , Node.map branch ~f:(handle_expr ~type_ ~should_annotate:false) )) ))
    | Handle { expr; expr_type; value_branch; effect_branches } ->
      let branches =
        List.map effect_branches ~f:(fun (effect_pattern, branch) ->
          ( Node.map effect_pattern ~f:(fun { operation; args } ->
              `Effect
                { Effect_pattern.operation = relativize_name operation
                ; args = Nonempty.map args ~f:relativize_pattern
                })
          , Node.map branch ~f:(handle_expr ~type_ ~should_annotate:false) ))
      in
      let branches : _ Nonempty.t =
        match value_branch with
        | Some (pattern, branch) ->
          let value_branch =
            ( Node.map pattern ~f:(fun pattern -> `Value (relativize_pattern pattern))
            , Node.map branch ~f:(handle_expr ~type_) )
          in
          value_branch :: branches
        | None ->
          (match Nonempty.of_list branches with
           | Some branches -> branches
           | None -> compiler_bug [%message "Handle expression with no branches"])
      in
      annotated
        (Handle
           (Node.map expr ~f:(handle_expr ~type_:(relativize_type' expr_type)), branches))
    | Let { rec_; bindings; body } ->
      Let
        { rec_
        ; bindings =
            Nonempty.map bindings ~f:(fun (pat_and_type, expr) ->
              let pat_span = Node.span pat_and_type in
              let pat, type_ =
                Node.with_value pat_and_type ~f:(fun (pat, type_) ->
                  relativize_pattern pat, relativize_type' type_)
              in
              let pat : Untyped.Pattern.t = Type_annotation (pat, type_) in
              Node.create pat pat_span, Node.map expr ~f:(handle_expr ~type_))
        ; body = Node.map body ~f:(handle_expr ~type_)
        }
    | Tuple fields ->
      let field_types =
        match fst type_ with
        | Tuple field_types -> field_types
        | _ ->
          compiler_bug [%message "Unexpected type for tuple" (type_ : _ Type_scheme.t)]
      in
      Tuple
        (List.map2_exn fields field_types ~f:(fun field field_type ->
           Node.map field ~f:(fun field -> handle_expr field ~type_:(field_type, []))))
    | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
      (* TODO: Implement this *)
      failwith "TODO: Typed -> Untyped AST for record expressions"
  in
  let rec handle_common
    : Module_path.absolute Module.common -> Module_path.relative Module.common
    = function
    | Val (name, fixity, type_) -> Val (name, fixity, relativize_type' type_)
    | Extern (name, fixity, type_, extern_name) ->
      Extern (name, fixity, relativize_type' type_, extern_name)
    | Type_decl (type_name, decl) ->
      Type_decl (type_name, Type_decl.map_exprs decl ~f:relativize_type)
    | Effect (effect_name, effect) ->
      Effect (effect_name, Effect.map_exprs effect ~f:relativize_type)
    | Trait_sig (trait_name, params, sigs) ->
      Trait_sig (trait_name, params, List.map sigs ~f:(Node.map ~f:handle_sig))
    | Import import -> Import import
  and handle_sig : Module_path.absolute Module.sig_ -> Module_path.relative Module.sig_
    = function
    | Common_sig common -> Common_sig (handle_common common)
    | Module_sig (module_name, sigs) ->
      Module_sig (module_name, List.map sigs ~f:(Node.map ~f:handle_sig))
  and handle_def : Typed.Module.def -> Untyped.Module.def = function
    | Common_def common -> Common_def (handle_common common)
    | Let { rec_; bindings } ->
      Let
        { rec_
        ; bindings =
            Nonempty.map bindings ~f:(fun (pat, expr_and_type) ->
              ( Node.map pat ~f:relativize_pattern
              , Node.map expr_and_type ~f:(fun (expr, type_) ->
                  handle_expr expr ~type_:(relativize_type' type_) ~should_annotate:false)
              ))
        }
    | Module (module_name, sigs, defs) ->
      Module
        ( module_name
        , List.map sigs ~f:(Node.map ~f:handle_sig)
        , List.map defs ~f:(Node.map ~f:handle_def) )
    | Trait (trait_name, params, sigs, defs) ->
      Trait
        ( trait_name
        , params
        , List.map sigs ~f:(Node.map ~f:handle_sig)
        , List.map defs ~f:(Node.map ~f:handle_def) )
    | Impl (trait_bound, trait_name, args, defs) ->
      Impl
        ( trait_bound
        , trait_name
        , Nonempty.map args ~f:relativize_type
        , List.map defs ~f:(Node.map ~f:handle_def) )
  in
  ( module_name
  , List.map sigs ~f:(Node.map ~f:handle_sig)
  , List.map defs ~f:(Node.map ~f:handle_def) )
;;
