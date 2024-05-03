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

module Config = struct
  type t =
    { max_line_length : int
    ; indent_size : int
    }

  let default = { max_line_length = 90; indent_size = 2 }
end

(* TODO: Add more groups so we can handle breaking more kinds of nodes, test with low max
   line length. *)
(* FIXME: Think about how to insert parentheses where necessary. *)
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
  let format_block sigs_or_defs ~on_empty ~f =
    (* FIXME: How to express "two line breaks between multi-line elements, and only one
       between single-line elements"? We'd need to support that explicitly in the document
       language, I think, since only it knows whether something ends up being broken.
       Conditional syntax based on mode of adjacent groups or something?
       
       Or, we give each group an id, and you can branch based on the mode of an id.
       
       Like [Group of { id; doc }] and [Branch of { group_id; if_flat; if_break; }] *)
    if List.is_empty sigs_or_defs
    then on_empty
    else
      Text "{"
      ^^ indent
           ~prefix:Force_break
           (separated ~sep:Force_break (List.map sigs_or_defs ~f))
      ^^ Force_break
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
        | Var _ | Tuple _ | Type_app _ -> format_type type_
        | Function _ | Union _ | Intersection _ -> parens (format_type type_)
      in
      comma_separated (Nonempty.to_list (Nonempty.map arg_types ~f:format_fun_part))
      ^| Text "->"
      ^| format_effects effects
      ^| format_fun_part result_type
    | Union types ->
      (* FIXME: Make this | and & syntaxes real (parseable) *)
      separated
        ~sep:(Break ^^ Text "|" ^^ Break)
        (Non_single_list.to_list (Non_single_list.map types ~f:format_type_term))
    | Intersection types ->
      separated
        ~sep:(Break ^^ Text "&" ^^ Break)
        (Non_single_list.to_list (Non_single_list.map types ~f:format_type_term))
  and format_type_term (type_ : _ Type_scheme.type_) =
    match type_ with
    | Var _ | Tuple _ | Type_app (_, []) -> format_type type_
    | Type_app (_, _ :: _) | Function _ | Union _ | Intersection _ ->
      parens (format_type type_)
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
  let format_annotated doc type_ = doc ^| Text ":" ^^ indent type_ in
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
  (* FIXME: Think about using groups more *)
  let rec format_pattern : Untyped.Pattern.t -> t = function
    | Constant literal -> format_literal literal
    | Catch_all None -> Text "_"
    | Catch_all (Some name) -> Text (Value_name.to_string name)
    | As (pat, name) ->
      format_pattern pat ^| Text "as" ^| Text (Value_name.to_string name)
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
    | Name name -> Text (Value_name.Relative.to_string name)
    | Qualified (path, expr) ->
      Text (Module_path.to_string path)
      ^^ Text "."
      ^^ Node.with_value expr ~f:format_expr_term
    | Fun_call (fun_, args) ->
      format_application
        (Node.with_value fun_ ~f:format_expr_term)
        (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_expr_term))
    | Lambda (args, body) ->
      Text "\\"
      ^^ separated
           (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_pattern_term))
      ^| Text "->"
      ^^ indent_expr body
    | If (cond, then_, else_) ->
      Text "if"
      ^^ indent_expr cond
      ^| Text "then"
      ^^ indent_expr then_
      ^| Text "else"
      ^^ indent_expr else_
    | Match (expr, branches) ->
      Text "match"
      ^^ indent_expr expr
      ^| separated
           (List.map (Nonempty.to_list branches) ~f:(fun (pattern, expr) ->
              Text "|"
              ^| Node.with_value pattern ~f:format_pattern
              ^| Text "->"
              ^^ indent_expr expr))
    | Handle (expr, branches) ->
      Text "handle"
      ^^ indent_expr expr
      ^| separated
           (List.map (Nonempty.to_list branches) ~f:(fun (effect_pattern, expr) ->
              Text "|"
              ^| Node.with_value effect_pattern ~f:(function
                   | `Value pattern -> format_pattern pattern
                   | `Effect { operation; args } ->
                     format_application
                       (Text (Value_name.Relative.to_string operation))
                       (List.map (Nonempty.to_list args) ~f:format_pattern))
              ^| Text "->"
              ^^ indent_expr expr))
    | Let { rec_; bindings; body } ->
      Group (format_let_binding ~rec_ ~bindings ^| Text "in")
      ^| Node.with_value body ~f:format_expr
    | Tuple args -> format_tuple (List.map args ~f:(Node.with_value ~f:format_expr))
    | Type_annotation (expr, type_) ->
      format_annotated
        (Node.with_value expr ~f:format_expr)
        (Node.with_value type_ ~f:format_type')
    | Op_tree _ -> failwith "TODO: format op tree"
    | Seq_literal _ -> failwith "TODO: format seq literal"
    | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
      failwith "TODO: format record expressions"
  and format_expr_term (expr : Untyped.Expr.t) =
    match expr with
    | Literal _ | Name _
    | Qualified (_, _)
    | Tuple _ | Seq_literal _ | Record_literal _ | Record_update _ | Record_field_access _
      -> format_expr expr
    | Fun_call (_, _)
    | Op_tree _
    | Lambda (_, _)
    | If (_, _, _)
    | Match (_, _)
    | Handle (_, _)
    | Let _
    | Type_annotation (_, _) -> parens (format_expr expr)
  and indent_expr expr = indent (Node.with_value expr ~f:format_expr)
  and format_let_binding
    ~rec_
    ~bindings:((first_pat, first_expr) :: bindings : _ Nonempty.t)
    =
    Group
      (format_equals
         (Text (if rec_ then "let" else "let'")
          ^| Node.with_value first_pat ~f:format_pattern)
         (Node.with_value first_expr ~f:format_expr)
       ^| separated
            (List.map bindings ~f:(fun (pat, expr) ->
               format_equals
                 (Text "and" ^| Node.with_value pat ~f:format_pattern)
                 (Node.with_value expr ~f:format_expr))))
  in
  let rec format_common : _ Module.common -> t = function
    | Val (name, fixity, type_) ->
      format_annotated
        (Text "val" ^| Text (Value_name.to_string name) ^| format_fixity fixity)
        (format_type' type_)
    | Extern (name, fixity, type_, extern_name) ->
      format_equals
        (format_annotated
           (Text "extern" ^| Text (Value_name.to_string name) ^| format_fixity fixity)
           (format_type' type_))
        (format_string_literal (Extern_name.to_ustring extern_name))
    | Type_decl (type_name, (params, decl)) ->
      let body, body_prefix =
        match decl with
        | Abstract -> Empty, None
        | Alias type_ -> format_type type_, None
        | Variants cnstrs ->
          let body =
            if List.is_empty cnstrs
            then Text "|"
            else
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
          body, Some Empty
        | Record _ -> failwith "TODO: Format record type decl"
      in
      format_equals
        (Text "type" ^| format_params_application (Type_name.to_string type_name) params)
        body
        ?body_prefix
    | Effect (effect_name, { params; operations }) ->
      format_equals
        (Text "effect"
         ^| format_params_application (Effect_name.to_string effect_name) params)
        (match operations with
         | None -> Empty
         | Some operations ->
           format_block
             ~on_empty:(Text "{}")
             ~f:format_common
             (List.map operations ~f:(fun { name; args; result } ->
                Module.Val (name, None, (Function (args, Effect_union [], result), [])))))
    | Trait_sig _ -> failwith "TODO: format trait sig"
    | Import { kind; paths } ->
      (* FIXME: Put toplevel imports at the top (and they should probably apply to sigs
         too)*)
      let rec format_import_paths : Module.Import.Paths.t -> t = function
        | All -> Text "_"
        | Module (module_name, paths) ->
          Text (Module_name.to_string module_name)
          ^^ Text "."
          ^^ format_multiple_import_paths paths
        | Name name -> Text (Unidentified_name.to_string name)
        | Name_as (name, as_name) ->
          Text (Unidentified_name.to_string name)
          ^| Text "as"
          ^| Text (Unidentified_name.to_string as_name)
        | Name_excluded name ->
          Text (Unidentified_name.to_string name) ^| Text "as" ^| Text "_"
      and format_multiple_import_paths : _ Nonempty.t -> t = function
        | [ paths ] -> format_import_paths paths
        | multiple_paths ->
          format_tuple (List.map (Nonempty.to_list multiple_paths) ~f:format_import_paths)
      in
      Text (String.init (Module.Import.Kind.to_n_periods kind) ~f:(const '.'))
      ^^ format_import_paths paths
  and format_sig : _ Module.sig_ -> t = function
    | Common_sig common -> format_common common
    | Module_sig (module_name, sigs) ->
      format_annotated
        (Text "module" ^| Text (Module_name.to_string module_name))
        (format_block sigs ~on_empty:(Text "{}") ~f:(Node.with_value ~f:format_sig))
  and format_def : _ Module.def -> t = function
    | Common_def common -> format_common common
    | Let { rec_ = _; bindings } -> format_let_binding ~rec_:true ~bindings
    | Module (module_name, sigs, defs) ->
      let module_name = Text (Module_name.to_string module_name) in
      let defs =
        format_block ~on_empty:(Text "{}") defs ~f:(Node.with_value ~f:format_def)
      in
      if List.is_empty sigs
      then format_equals module_name defs
      else
        format_equals
          (format_annotated
             module_name
             (format_block ~on_empty:Empty sigs ~f:(Node.with_value ~f:format_sig)))
          defs
    | Trait _ | Impl _ -> failwith "TODO: formatting traits and impls"
  in
  (if List.is_empty sigs
   then Empty
   else
     format_annotated
       (Text "module")
       (format_block ~on_empty:Empty sigs ~f:(Node.with_value ~f:format_sig)))
  ^| separated ~sep:Force_break (List.map defs ~f:(Node.with_value ~f:format_def))
;;

let format ?(config = Config.default) module_ =
  format_to_document ~config module_
  |> Auto_format.format ~max_line_length:config.max_line_length
;;
