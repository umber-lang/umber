open! Import
open Names

module Typed_to_untyped = struct
  let convert_module ((outer_module_name, sigs, defs) : Typed.Module.t) : Untyped.Module.t
    =
    (* NOTE: It isn't semantics-preserving to interpret an absolute path as a relative path
     due to shadowing. This is kinda fine for the purposes of pretty-printing the AST
     for test output or error messages. *)
    let outer_module_path = Module_path.Relative.of_module_names [ outer_module_name ] in
    let relativize_name =
      Tuple2.map_fst ~f:(fun (path : Module_path.Absolute.t) ->
        Module_path.Relative.of_module_names (path :> Module_name.t list)
        (* Chopping the current module name is necessary for basic correctness. *)
        |> Module_path.chop_prefix_if_exists ~prefix:outer_module_path)
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
    let annotated_pattern (pattern : Typed.Pattern.t) (type_ : _ Type_scheme.type_)
      : Untyped.Pattern.t
      =
      match pattern with
      | Tuple fields ->
        let field_types =
          match type_ with
          | Tuple field_types -> field_types
          | _ ->
            compiler_bug
              [%message "Unexpected type for tuple" (type_ : _ Type_scheme.type_)]
        in
        Tuple
          (List.map2_exn fields field_types ~f:(fun field field_type ->
             Pattern.Type_annotation (relativize_pattern field, (field_type, []))))
      | _ -> Type_annotation (relativize_pattern pattern, (type_, []))
    in
    let rec convert_expr
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
             ( Node.map fun_ ~f:(convert_expr ~type_:(relativize_type' fun_type))
             , Nonempty.map args ~f:(fun (arg, arg_type) ->
                 Node.map arg ~f:(fun arg ->
                   convert_expr arg ~type_:(relativize_type' arg_type))) ))
      | Lambda (args, body) ->
        let arg_types, body_type =
          match fst type_ with
          | Function (arg_types, _, body_type) -> arg_types, body_type
          | _ ->
            compiler_bug
              [%message "Unexpected type for function" (type_ : _ Type_scheme.t)]
        in
        let convert_lambda () =
          annotated
            (Lambda
               ( Nonempty.map2 args arg_types ~f:(fun arg arg_type ->
                   Node.map arg ~f:(fun arg ->
                     if should_annotate
                     then annotated_pattern arg arg_type
                     else relativize_pattern arg))
               , Node.map body ~f:(convert_expr ~type_:(body_type, [])) ))
        in
        (* Handle `match` functions with their anonymous argument. *)
        (match args, Node.with_value body ~f:Fn.id with
         | [ arg ], Match (expr, _expr_type, branches) ->
           Node.with_value2 arg expr ~f:(fun arg expr ->
             match arg, expr with
             | Catch_all (Some name), Name (_, name')
               when Value_name.equal name Constant_names.match_
                    && Value_name.equal name name' ->
               annotated
                 (Match_function
                    (Nonempty.map branches ~f:(fun (pattern, branch) ->
                       ( Node.map pattern ~f:relativize_pattern
                       , Node.map
                           branch
                           ~f:(convert_expr ~type_:(body_type, []) ~should_annotate:false)
                       ))))
             | _ -> convert_lambda ())
         | _ -> convert_lambda ())
      | Match (expr, expr_type, branches) ->
        annotated
          (Match
             ( Node.map expr ~f:(convert_expr ~type_:(relativize_type' expr_type))
             , Nonempty.map branches ~f:(fun (pattern, branch) ->
                 ( Node.map pattern ~f:relativize_pattern
                 , Node.map branch ~f:(convert_expr ~type_ ~should_annotate:false) )) ))
      | Handle { expr; expr_type; value_branch; effect_branches } ->
        let branches =
          List.map effect_branches ~f:(fun (effect_pattern, branch) ->
            ( Node.map effect_pattern ~f:(fun { operation; args } ->
                `Effect
                  { Effect_pattern.operation = relativize_name operation
                  ; args = Nonempty.map args ~f:relativize_pattern
                  })
            , Node.map branch ~f:(convert_expr ~type_ ~should_annotate:false) ))
        in
        let branches : _ Nonempty.t =
          match value_branch with
          | Some (pattern, branch) ->
            let value_branch =
              ( Node.map pattern ~f:(fun pattern -> `Value (relativize_pattern pattern))
              , Node.map branch ~f:(convert_expr ~type_) )
            in
            value_branch :: branches
          | None ->
            (match Nonempty.of_list branches with
             | Some branches -> branches
             | None -> compiler_bug [%message "Handle expression with no branches"])
        in
        annotated
          (Handle
             ( Node.map expr ~f:(convert_expr ~type_:(relativize_type' expr_type))
             , branches ))
      | Let { rec_; bindings; body } ->
        Let
          { rec_
          ; bindings =
              Nonempty.map bindings ~f:(fun (pat_and_type, expr) ->
                let pattern, type_ = Node.with_value pat_and_type ~f:Fn.id in
                let type_ = relativize_type' type_ in
                ( Node.create
                    (annotated_pattern pattern (fst type_))
                    (Node.span pat_and_type)
                , Node.map expr ~f:(convert_expr ~type_) ))
          ; body = Node.map body ~f:(convert_expr ~type_)
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
             Node.map field ~f:(fun field -> convert_expr field ~type_:(field_type, []))))
      | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
        (* TODO: Implement this *)
        failwith "TODO: Typed -> Untyped AST for record expressions"
    in
    let rec convert_common
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
        Trait_sig (trait_name, params, List.map sigs ~f:(Node.map ~f:convert_sig))
      | Import import -> Import import
    and convert_sig : Module_path.absolute Module.sig_ -> Module_path.relative Module.sig_
      = function
      | Common_sig common -> Common_sig (convert_common common)
      | Module_sig (module_name, sigs) ->
        Module_sig (module_name, List.map sigs ~f:(Node.map ~f:convert_sig))
    and convert_def : Typed.Module.def -> Untyped.Module.def = function
      | Common_def common -> Common_def (convert_common common)
      | Let { rec_; bindings } ->
        Let
          { rec_
          ; bindings =
              Nonempty.map bindings ~f:(fun (pattern, expr_and_type) ->
                let expr, type_ = Node.with_value expr_and_type ~f:Fn.id in
                let type_ = relativize_type' type_ in
                ( Node.map pattern ~f:(fun pattern ->
                    annotated_pattern pattern (fst type_))
                , Node.create
                    (convert_expr expr ~type_ ~should_annotate:false)
                    (Node.span expr_and_type) ))
          }
      | Module (module_name, sigs, defs) ->
        Module
          ( module_name
          , List.map sigs ~f:(Node.map ~f:convert_sig)
          , List.map defs ~f:(Node.map ~f:convert_def) )
      | Trait (trait_name, params, sigs, defs) ->
        Trait
          ( trait_name
          , params
          , List.map sigs ~f:(Node.map ~f:convert_sig)
          , List.map defs ~f:(Node.map ~f:convert_def) )
      | Impl (trait_bound, trait_name, args, defs) ->
        Impl
          ( trait_bound
          , trait_name
          , Nonempty.map args ~f:relativize_type
          , List.map defs ~f:(Node.map ~f:convert_def) )
    in
    ( outer_module_name
    , List.map sigs ~f:(Node.map ~f:convert_sig)
    , List.map defs ~f:(Node.map ~f:convert_def) )
  ;;

  let%expect_test "match function" =
    let node = Node.dummy_span in
    let untyped : Untyped.Module.t =
      ( Module_name.of_string_exn "MatchFunction"
      , []
      , [ node
            (Module.Let
               { rec_ = true
               ; bindings =
                   [ ( node (Pattern.Catch_all (Some (Value_name.of_string_exn "foo")))
                     , node
                         (Untyped.Expr.Match_function
                            [ node (Pattern.Catch_all None), node (Untyped.Expr.Tuple [])
                            ]) )
                   ]
               })
        ] )
    in
    let typed =
      match
        Typed.Module.of_untyped
          ~names:Name_bindings.core
          ~types:(Type_bindings.create ())
          ~include_std:false
          untyped
      with
      | Ok (_names, typed) -> typed
      | Error error -> raise (Compilation_error.Compilation_error error)
    in
    print_s [%sexp (convert_module typed : Untyped.Module.t)];
    [%expect
      {|
      (MatchFunction ()
       ((Let (rec_ false)
         (bindings
          (((Type_annotation (Catch_all (foo))
             ((Function ((Intersection ())) (Effect_union ()) (Tuple ())) ()))
            (Match_function (((Catch_all ()) (Tuple ())))))))))) |}]
  ;;
end

module Config = struct
  type t =
    { max_line_length : int
    ; indent_size : int
    }

  let default = { max_line_length = 90; indent_size = 2 }
end

(* TODO: Add more groups so we can handle breaking more kinds of nodes, test with low max
   line length. *)
let format_to_document
  ?config:({ Config.max_line_length = _; indent_size } = Config.default)
  (((_ : Module_name.t), sigs, defs) : Untyped.Module.t)
  =
  let open Auto_format.Document in
  let format_option option ~f =
    match option with
    | Some value -> f value
    | None -> Empty
  in
  let indent ?(prefix = Break) doc =
    match doc with
    | Empty -> Empty
    | _ -> Indent (indent_size, prefix ^^ Group doc)
  in
  let parens doc = Text "(" ^^ doc ^^ Text ")" in
  let comma_separated = separated ~sep:(Text "," ^^ Break) in
  (* FIXME: How to express "two line breaks between multi-line elements, and only one
     between single-line elements"? We'd need to support that explicitly in the document
     language, I think, since only it knows whether something ends up being broken.
     Conditional syntax based on mode of adjacent groups or something?
     
     Or, we give each group an id, and you can branch based on the mode of an id.
     
     Like [Group of { id; doc }] and [Branch of { group_id; if_flat; if_break; }]
      
     Or, like Pprint, we can have [Check_mode { if_flat; if_break }]. *)
  let format_inside_block sigs_or_defs ~f =
    indent ~prefix:Force_break (separated ~sep:Force_break (List.map sigs_or_defs ~f))
    ^^ Force_break
  in
  let format_block prefix eq_or_colon sigs_or_defs ~on_empty ~f =
    match sigs_or_defs with
    | [] -> Group (prefix ^| Text (Char.to_string eq_or_colon) ^| on_empty)
    | _ :: _ ->
      Group (prefix ^| Text (Char.to_string eq_or_colon) ^| Text "{")
      ^^ format_inside_block sigs_or_defs ~f
      ^^ Text "}"
  in
  let format_sigs_and_defs prefix ~sigs ~defs ~f_sigs ~f_defs =
    if List.is_empty sigs
    then format_block prefix '=' defs ~f:f_defs ~on_empty:(Text "{}")
    else
      Group (prefix ^| Text ":" ^| Text "{")
      ^^ format_inside_block sigs ~f:f_sigs
      ^^ Group (Text "}" ^| Text "=" ^| Text "{")
      ^^ format_inside_block defs ~f:f_defs
      ^^ Text "}"
  in
  let format_application fun_ args = fun_ ^^ indent (separated args) in
  let format_params_application type_name (params : Type_param_name.t Unique_list.t) =
    format_application
      (Text type_name)
      (List.map
         (params :> Type_param_name.t list)
         ~f:(fun param -> Text (Type_param_name.to_string param)))
  in
  let format_tuple args = parens (comma_separated args) in
  let rec format_type' ((type_, constraints) : _ Type_scheme.t) =
    if not (List.is_empty constraints) then failwith "TODO: formatting type constraints";
    format_type type_
  and format_type type_ =
    match type_ with
    | Var param -> Text (Type_param_name.to_string param)
    | Type_app (type_name, args) ->
      format_application
        (Text (Type_name.Relative.to_string type_name))
        (List.map args ~f:format_type_term)
    | Tuple args -> format_tuple (List.map args ~f:format_type)
    | Function (arg_types, effects, result_type) ->
      let format_fun_part (type_ : _ Type_scheme.type_) =
        (* [Type_app] binds tighter than "->", but "->" is non-associative so higher-order
           function types must be parenthesized. *)
        match type_ with
        | Type_app _ -> format_type type_
        | Var _ | Tuple _ | Function _ | Union _ | Intersection _ ->
          format_type_term type_
      in
      comma_separated (Nonempty.to_list (Nonempty.map arg_types ~f:format_fun_part))
      ^| Text "->"
      ^| format_effects effects
      ^| format_fun_part result_type
    | Union types ->
      (* FIXME: Make this | and & syntaxes real (parseable) *)
      (* FIXME: Don't just write "Never" or "Any", it could be shadowed. Maybe turn into a
         new type varable or something. *)
      let types =
        Non_single_list.to_list (Non_single_list.map types ~f:format_type_term)
      in
      if List.is_empty types
      then Text "Never"
      else separated ~sep:(Break ^^ Text "|" ^^ Break) types
    | Intersection types ->
      let types =
        Non_single_list.to_list (Non_single_list.map types ~f:format_type_term)
      in
      if List.is_empty types
      then Text "Any"
      else separated ~sep:(Break ^^ Text "&" ^^ Break) types
  and format_type_term (type_ : _ Type_scheme.type_) =
    match type_ with
    | Var _ | Tuple _ | Type_app (_, []) | Union [] | Intersection [] -> format_type type_
    | Type_app (_, _ :: _) | Function _ | Union (_ :: _ :: _) | Intersection (_ :: _ :: _)
      -> parens (format_type type_)
  and format_effects effects =
    let effects =
      match effects with
      | Effect_var param -> Text (Type_param_name.to_string param)
      | Effect (effect_name, args) ->
        format_application
          (Text (Effect_name.Relative.to_string effect_name))
          (List.map args ~f:format_type)
      | Effect_union effects | Effect_intersection effects ->
        let format_part (effects : _ Type_scheme.effects) =
          match effects with
          | Effect_var _ | Effect _ -> format_effects effects
          | Effect_union _ | Effect_intersection _ ->
            (* TODO: Decide what to do with this *)
            failwith "TODO: Nested effect union/intersection"
        in
        comma_separated (List.map (Non_single_list.to_list effects) ~f:format_part)
    in
    match effects with
    | Empty -> Empty
    | _ -> Text "<" ^^ effects ^^ Text ">"
  in
  let format_value_name name =
    let text = Text (Value_name.to_string name) in
    if Parsing.Utils.value_name_is_infix_operator name then parens text else text
  in
  let format_qualified (path, name) ~f =
    if Module_path.is_empty path
    then f name
    else Text (Module_path.to_string path ^ ".") ^^ f name
  in
  let format_annotated doc type_ = doc ^^ indent (Text ":" ^| type_) in
  let format_equals ?body_prefix doc body =
    match body with
    | Empty -> doc
    | _ -> Group (doc ^| Text "=") ^^ indent ?prefix:body_prefix body
  in
  let format_fixity =
    format_option ~f:(fun ((assoc, level) : Fixity.t) ->
      let assoc_keyword =
        match assoc with
        | Non_assoc -> "infix"
        | Left -> "infixl"
        | Right -> "infixr"
      in
      parens (Text assoc_keyword ^| Text (Int.to_string (level :> int))))
  in
  let format_char_literal c =
    (* FIXME: Handle escaping *)
    Text ("'" ^ Uchar.to_string c ^ "'")
  in
  let format_string_literal str =
    (* FIXME: Handle escaping *)
    Text ("\"" ^ Ustring.to_string str ^ "\"")
  in
  let format_literal : Literal.t -> t = function
    (* TODO: int/float formatting may not match up exactly with parsing *)
    | Int i -> Text (Int.to_string i)
    | Float x -> Text (Float.to_string x)
    | Char c -> format_char_literal c
    | String ustr -> format_string_literal ustr
  in
  let rec format_pattern : Untyped.Pattern.t -> t = function
    | Constant literal -> format_literal literal
    | Catch_all None -> Text "_"
    | Catch_all (Some name) -> format_value_name name
    | As (pat, name) -> format_pattern pat ^| Text "as" ^| format_value_name name
    | Cnstr_appl (cnstr, args) ->
      format_application
        (Text (Cnstr_name.Relative.to_string cnstr))
        (List.map args ~f:format_pattern_term)
    | Tuple args -> format_tuple (List.map args ~f:format_pattern)
    | Union (left, right) -> format_pattern left ^| Text "|" ^| format_pattern right
    | Type_annotation (pattern, type_) ->
      format_annotated (format_pattern pattern) (format_type' type_)
    | Record _ -> failwith "TODO: format record patterns"
  and format_pattern_term (pattern : Untyped.Pattern.t) =
    match pattern with
    | Constant _ | Catch_all _ | Tuple _ | Record _ -> format_pattern pattern
    | As (_, _) | Cnstr_appl (_, _) | Union (_, _) | Type_annotation (_, _) ->
      parens (format_pattern pattern)
  in
  let rec format_expr : Untyped.Expr.t -> t = function
    | Literal literal -> format_literal literal
    | Name name -> format_qualified name ~f:format_value_name
    | Qualified (path, expr) ->
      format_qualified (path, expr) ~f:(Node.with_value ~f:format_expr_term)
    | Fun_call (fun_, args) ->
      let format_fun_call fun_ =
        format_application
          fun_
          (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_expr_term))
      in
      Node.with_value fun_ ~f:(function
        | Name (path, name) when Parsing.Utils.value_name_is_infix_operator name ->
          (match (path :> Module_name.t list), args with
           | [], [ left; right ] ->
             (* TODO: Remove unnecessary parentheses. Requires understanding precedence
                and associativity again. *)
             let name = Value_name.to_string name in
             let left = Node.with_value left ~f:format_expr_term_or_fun_call in
             let right = Node.with_value right ~f:format_expr_term_or_fun_call in
             Group
               (match name with
                | ";" ->
                  (* Format as `a; b` instead of `a ; b`. *)
                  Group (left ^^ Text name) ^| right
                | _ -> left ^| Group (Text name ^| right))
           | _ ->
             format_fun_call
               (format_qualified (path, ()) ~f:(fun () ->
                  parens (Text (Value_name.to_string name)))))
        | _ -> format_fun_call (Node.with_value fun_ ~f:format_expr_term))
    | Lambda (args, body) ->
      Group
        (Text "\\"
         ^^ separated
              (List.map
                 (Nonempty.to_list args)
                 ~f:(Node.with_value ~f:format_pattern_term))
         ^| Text "->")
      ^^ indent_expr body
    | If (cond, then_, else_) ->
      Text "if"
      ^^ indent_expr cond
      ^| Text "then"
      ^^ indent_expr then_
      ^| Text "else"
      ^^ indent_expr else_
    | Match (expr, branches) ->
      Group (Text "match" ^^ indent_expr expr) ^^ format_match_branches branches
    | Match_function branches -> Text "match" ^^ format_match_branches branches
    | Handle (expr, branches) ->
      Group (Text "handle" ^^ indent_expr expr) ^^ format_handle_branches branches
    | Let { rec_; bindings; body } ->
      Group (format_let_binding ~rec_ ~bindings ^| Text "in")
      ^^ Force_break
      ^^ Group (Node.with_value body ~f:format_expr)
    | Tuple args -> format_tuple (List.map args ~f:(Node.with_value ~f:format_expr))
    | Type_annotation (expr, type_) ->
      format_annotated
        (Node.with_value expr ~f:format_expr_term_or_fun_call)
        (Node.with_value type_ ~f:format_type')
    | Op_tree op_tree ->
      Node.with_value (Op_tree.to_untyped_expr_as_is op_tree) ~f:format_expr
    | Seq_literal _ -> failwith "TODO: format seq literal"
    | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
      failwith "TODO: format record expressions"
  and format_expr_term (expr : Untyped.Expr.t) =
    match expr with
    | Literal _
    | Name _
    | Qualified _
    | Tuple _
    | Seq_literal _
    | Record_literal _
    | Record_update _
    | Record_field_access _ -> format_expr expr
    | Fun_call _
    | Op_tree _
    | Lambda _
    | If _
    | Match _
    | Match_function _
    | Handle _
    | Let _
    | Type_annotation _ -> parens (format_expr expr)
  and format_expr_term_or_fun_call (expr : Untyped.Expr.t) =
    match expr with
    | Fun_call _ -> format_expr expr
    | expr -> format_expr_term expr
  and indent_expr expr = indent (Node.with_value expr ~f:format_expr)
  and format_branches_aux
        : 'a. ('a Node.t * Untyped.Expr.t Node.t) Nonempty.t -> f:('a -> t) -> t
    =
   fun branches ~f ->
    concat_all
      (List.map (Nonempty.to_list branches) ~f:(fun (pattern, expr) ->
         Force_break
         ^^ Group
              (Group (Text "|" ^| Node.with_value pattern ~f ^| Text "->")
               ^^ indent_expr expr)))
  and format_match_branches branches = format_branches_aux branches ~f:format_pattern
  and format_handle_branches branches =
    format_branches_aux branches ~f:(function
      | `Value pattern -> format_pattern pattern
      | `Effect ({ operation; args } : _ Effect_pattern.t) ->
        format_application
          (format_qualified operation ~f:format_value_name)
          (List.map (Nonempty.to_list args) ~f:format_pattern))
  and format_let_binding
    ~rec_
    ~bindings:((first_pat, first_expr) :: bindings : _ Nonempty.t)
    =
    let format_binding keyword pattern expr =
      Node.with_value2
        pattern
        expr
        ~f:(fun (pattern : Untyped.Pattern.t) (expr : Untyped.Expr.t) ->
        match pattern, expr with
        | Catch_all (Some _fun_name), Lambda (args, body) ->
          Node.with_value body ~f:(fun body ->
            format_equals
              (Text keyword
               ^| format_pattern pattern
               ^| separated
                    (List.map
                       (Nonempty.to_list args)
                       ~f:(Node.with_value ~f:format_pattern_term)))
              (format_expr body))
        | Catch_all (Some _fun_name), Match_function branches ->
          Group (format_equals (Text keyword ^| format_pattern pattern) (Text "match"))
          ^^ indent ~prefix:Empty (format_match_branches branches)
        | _ -> format_equals (Text keyword ^| format_pattern pattern) (format_expr expr))
    in
    Group
      (format_binding (if rec_ then "let" else "let'") first_pat first_expr
       ^| separated
            (List.map bindings ~f:(fun (pat, expr) -> format_binding "and" pat expr)))
  in
  let rec format_common : _ Module.common -> t = function
    | Val (name, fixity, type_) ->
      Group
        (format_annotated
           (Text "val" ^| format_value_name name ^| format_fixity fixity)
           (format_type' type_))
    | Extern (name, fixity, type_, extern_name) ->
      Group
        (format_equals
           (format_annotated
              (Text "extern" ^| format_value_name name ^| format_fixity fixity)
              (format_type' type_))
           (format_string_literal (Extern_name.to_ustring extern_name)))
    | Type_decl (type_name, (params, decl)) ->
      let body, body_prefix =
        match decl with
        | Abstract -> Empty, None
        | Alias type_ -> format_type type_, None
        | Variants cnstrs ->
          if List.is_empty cnstrs
          then Text "|", None
          else (
            let body =
              concat_all
                (List.map cnstrs ~f:(fun (cnstr, args) ->
                   Force_break
                   ^^ Group
                        (Text "|"
                         ^| format_application
                              (Text (Cnstr_name.to_string cnstr))
                              (List.map args ~f:format_type_term))))
            in
            (* Variants starting with [Force_break] means breaks double-up if another is
               inserted.*)
            body, Some Empty)
        | Record _ -> failwith "TODO: Format record type decl"
      in
      Group
        (format_equals
           (Text "type"
            ^| format_params_application (Type_name.to_string type_name) params)
           body
           ?body_prefix)
    | Effect (effect_name, { params; operations }) ->
      format_block
        (Text "effect"
         ^| format_params_application (Effect_name.to_string effect_name) params)
        '='
        (match operations with
         | None -> []
         | Some operations ->
           List.map operations ~f:(fun { name; args; result } ->
             Module.Val (name, None, (Function (args, Effect_union [], result), []))))
        ~f:format_common
        ~on_empty:(Text "{}")
    | Trait_sig _ -> failwith "TODO: format trait sig"
    | Import { kind; paths } ->
      (* TODO: Put toplevel imports at the top (and they should probably apply to sigs
         too) *)
      let format_unidentified_name name =
        format_value_name
          (Value_name.of_ustring_unchecked (Unidentified_name.to_ustring name))
      in
      let rec format_import_paths : Module.Import.Paths.t -> t = function
        | All -> Text "_"
        | Module (module_name, paths) ->
          Text (Module_name.to_string module_name ^ ".")
          ^^ format_multiple_import_paths paths
        | Name name -> format_unidentified_name name
        | Name_as (name, as_name) ->
          format_unidentified_name name ^| Text "as" ^| format_unidentified_name as_name
        | Name_excluded name -> format_unidentified_name name ^| Text "as" ^| Text "_"
      and format_multiple_import_paths : _ Nonempty.t -> t = function
        | [ paths ] -> format_import_paths paths
        | multiple_paths ->
          format_tuple (List.map (Nonempty.to_list multiple_paths) ~f:format_import_paths)
      in
      Group
        (Text "import"
         ^| Text (String.init (Module.Import.Kind.to_n_periods kind) ~f:(const '.'))
         ^^ format_import_paths paths)
  and format_sig : _ Module.sig_ -> t = function
    | Common_sig common -> format_common common
    | Module_sig (module_name, sigs) ->
      format_block
        (Text "module" ^| Text (Module_name.to_string module_name))
        ':'
        sigs
        ~f:(Node.with_value ~f:format_sig)
        ~on_empty:(Text "{}")
  and format_def : _ Module.def -> t = function
    | Common_def common -> format_common common
    | Let { rec_ = _; bindings } -> format_let_binding ~rec_:true ~bindings
    | Module (module_name, sigs, defs) ->
      format_sigs_and_defs
        (Text "module" ^| Text (Module_name.to_string module_name))
        ~sigs
        ~defs
        ~f_sigs:(Node.with_value ~f:format_sig)
        ~f_defs:(Node.with_value ~f:format_def)
    | Trait _ | Impl _ -> failwith "TODO: formatting traits and impls"
  in
  (if List.is_empty sigs
   then Empty
   else
     format_block
       (Text "module")
       ':'
       sigs
       ~f:(Node.with_value ~f:format_sig)
       ~on_empty:Empty)
  ^| separated ~sep:Force_break (List.map defs ~f:(Node.with_value ~f:format_def))
;;

let format ?(config = Config.default) module_ =
  format_to_document ~config module_
  |> Auto_format.format ~max_line_length:config.max_line_length
;;

(* FIXME: Also test let _ = match ... formatting *)
