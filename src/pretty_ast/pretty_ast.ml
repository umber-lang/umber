open! Core
open! Import
open Names

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
  (((_ : Module_name.t), sigs, defs) : Untyped_ast.Module.t)
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
  (* TODO: Consider adding an [Indent (1, _)] here to align parenthesized multiline
     expressions. *)
  let parens doc = Text "(" ^^ doc ^^ Text ")" in
  let comma_separated = separated ~sep:(Text "," ^^ Break) in
  (* TODO: How to express "two line breaks between multi-line elements, and only one
     between single-line elements"? We'd need to support that explicitly in the document
     language, I think, since only it knows whether something ends up being broken.
     Conditional syntax based on mode of adjacent groups or something?
     
     Or, we give each group an id, and you can branch based on the mode of an id.
     
     Like [Group of { id; doc }] and [Branch of { group_id; if_flat; if_break; }]
      
     Or, like Pprint, we can have [Check_mode { if_flat; if_break }]. *)
  let format_inside_block sigs_or_defs ~f =
    indent
      ~prefix:Force_break
      (separated ~sep:(Force_break ^^ Force_break) (List.map sigs_or_defs ~f))
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
  let format_tuple args = Group (parens (comma_separated args)) in
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
      (* TODO: Don't just write "Never" or "Any", it could be shadowed. Maybe turn into a
         new type varable or something. Or this could be a pre-processing step. *)
      let types =
        Non_single_list.to_list (Non_single_list.map types ~f:format_type_term)
      in
      if List.is_empty types
      then Text "Never"
      else parens (separated ~sep:(Break ^^ Text "|" ^^ Break) types)
    | Intersection types ->
      let types =
        Non_single_list.to_list (Non_single_list.map types ~f:format_type_term)
      in
      if List.is_empty types
      then Text "Any"
      else parens (separated ~sep:(Break ^^ Text "&" ^^ Break) types)
  and format_type_term (type_ : _ Type_scheme.type_) =
    match type_ with
    | Var _ | Tuple _ | Type_app (_, []) | Union _ | Intersection _ -> format_type type_
    | Type_app (_, _ :: _) | Function _ -> parens (format_type type_)
  and format_effects effects =
    let rec format_effects_internal : _ Type_scheme.effects -> t = function
      | Effect_var param -> Text (Type_param_name.to_string param)
      | Effect (effect_name, args) ->
        format_application
          (Text (Effect_name.Relative.to_string effect_name))
          (List.map args ~f:format_type)
      | Effect_union effects ->
        let format_part (effects : _ Type_scheme.effects) =
          match effects with
          | Effect_var _ | Effect _ -> format_effects_internal effects
          | Effect_union _ ->
            (* TODO: Decide what to do with this *)
            failwith "TODO: Nested effect union"
        in
        comma_separated (List.map (Non_single_list.to_list effects) ~f:format_part)
    in
    match format_effects_internal effects with
    | Empty -> Empty
    | effects -> Text "<" ^^ effects ^^ Text ">"
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
  let format_operator_name (path, name) =
    if Module_path.is_empty path && Parsing.Utils.value_name_is_infix_operator name
    then Text (Value_name.to_string name)
    else Text ":" ^^ format_qualified (path, name) ~f:format_value_name ^^ Text ":"
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
      Text assoc_keyword ^| Text (Int.to_string (level :> int)))
  in
  let format_char_literal c = Text ("'" ^ Lexer.escape_char_literal c ^ "'") in
  let format_string_literal str = Text ("\"" ^ Lexer.escape_string_literal str ^ "\"") in
  let format_literal : Literal.t -> t = function
    (* TODO: int/float formatting may not match up exactly with parsing *)
    | Int i -> Text (Int.to_string i)
    | Float x -> Text (Float.to_string x)
    | Char c -> format_char_literal c
    | String ustr -> format_string_literal ustr
  in
  let rec format_pattern : Untyped_ast.Pattern.t -> t = function
    | Constant literal -> format_literal literal
    | Catch_all None -> Text "_"
    | Catch_all (Some name) -> format_value_name name
    | As (pat, name) -> format_pattern pat ^| Text "as" ^| format_value_name name
    | Cnstr_appl (cnstr, args) ->
      format_application
        (Text (Cnstr_name.Relative.to_string cnstr))
        (List.map args ~f:format_pattern_term)
    | Tuple args ->
      let format_tuple_part (pattern : Untyped_ast.Pattern.t) =
        (* Type annotations can contain function types, and the commas in these can get
           mixed up with the commas in a tuple, so parenthesize them. *)
        match pattern with
        | Type_annotation _ -> parens (format_pattern pattern)
        | _ -> format_pattern pattern
      in
      format_tuple (List.map args ~f:format_tuple_part)
    | Union (left, right) -> format_pattern left ^| Text "|" ^| format_pattern right
    | Type_annotation (pattern, type_) ->
      format_annotated (format_pattern pattern) (format_type' type_)
    | Record _ -> failwith "TODO: format record patterns"
  and format_pattern_term (pattern : Untyped_ast.Pattern.t) =
    match pattern with
    | Constant _ | Catch_all _ | Tuple _ | Record _ | Cnstr_appl (_, []) ->
      format_pattern pattern
    | As (_, _) | Cnstr_appl (_, _ :: _) | Union (_, _) | Type_annotation (_, _) ->
      parens (format_pattern pattern)
  in
  let rec format_expr : Untyped_ast.Expr.t -> t = function
    | Literal literal -> format_literal literal
    | Name name -> format_qualified name ~f:format_value_name
    | Qualified (path, expr) ->
      format_qualified (path, expr) ~f:(Node.with_value ~f:format_expr_term)
    | Fun_call (fun_, args) ->
      let format_fun_call fun_ =
        (* Special-case formatting of applications which end in a lambda e.g. 
           `foo arg1 arg2 (fun x y z -> ...)`
           We'd like to group the function, initial args, and lambda args, onto one line,
           and then break before the lambda body. *)
        let initial_args, last_arg = Nonempty.split_last args in
        match Node.with_value last_arg ~f:Fn.id with
        | Lambda (lambda_args, lambda_body) ->
          let fun_args_and_lambda_args =
            List.map initial_args ~f:(Node.with_value ~f:format_expr_term)
            @ [ Text "(" ^^ format_lambda_args lambda_args ]
          in
          Group (format_application fun_ fun_args_and_lambda_args)
          ^^ indent_expr lambda_body
          ^^ Text ")"
        | _ ->
          format_application
            fun_
            (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_expr_term))
      in
      Node.with_value fun_ ~f:(function
        | Name (path, name) when Parsing.Utils.value_name_is_infix_operator name ->
          (match (path :> Module_name.t list), args with
           | [], [ left; right ] ->
             let name = Value_name.to_string name in
             let left = Node.with_value left ~f:format_expr_term_or_fun_call in
             let right = Node.with_value right ~f:format_expr_term_or_fun_call in
             Group
               (match name with
                | ";" ->
                  (* TODO: Maybe semicolon should just be represented explicitly as a
                     separate kind of expression in the AST if we're going to treat it so
                     specially. *)
                  (* Format as `A;\nB` instead of `A ; B`. *)
                  Group (left ^^ Text ";") ^^ Force_break ^^ Group right
                | _ -> Group left ^| Group (Text name ^| right))
           | _ ->
             format_fun_call
               (format_qualified (path, ()) ~f:(fun () ->
                  parens (Text (Value_name.to_string name)))))
        | _ -> format_fun_call (Node.with_value fun_ ~f:format_expr_term))
    | Lambda (args, body) -> Group (format_lambda_args args) ^^ indent_expr body
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
    | Tuple args ->
      let format_tuple_part (expr : Untyped_ast.Expr.t) =
        (* Type annotations can contain function types, and the commas in these can get
           mixed up with the commas in a tuple, so parenthesize them. *)
        match expr with
        | Type_annotation _ -> parens (format_expr expr)
        | _ -> format_expr expr
      in
      format_tuple (List.map args ~f:(Node.with_value ~f:format_tuple_part))
    | Type_annotation (expr, type_) ->
      format_annotated
        (Node.with_value expr ~f:format_expr_term_or_fun_call)
        (Node.with_value type_ ~f:format_type')
    | Op_tree op_tree ->
      Node.with_value (Op_tree.to_untyped_expr_as_is op_tree) ~f:format_expr
    | Op_section { op_side; op; expr } ->
      (match op_side with
       | `Left ->
         parens
           (Node.with_value op ~f:format_operator_name
            ^^ Node.with_value expr ~f:(indent << format_expr_op_term))
       | `Right ->
         parens
           (Node.with_value expr ~f:format_expr_op_term
            ^^ Node.with_value op ~f:(indent << format_operator_name)))
    | Seq_literal _ -> failwith "TODO: format seq literal"
    | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
      failwith "TODO: format record expressions"
  and format_lambda_args args =
    Text "\\"
    ^^ separated
         (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_pattern_term))
    ^| Text "->"
  and format_expr_term (expr : Untyped_ast.Expr.t) =
    match expr with
    | Literal _
    | Name _
    | Qualified _
    | Tuple _
    | Op_section _
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
  and format_expr_term_or_fun_call (expr : Untyped_ast.Expr.t) =
    match expr with
    | Fun_call _ -> format_expr expr
    | expr -> format_expr_term expr
  and format_expr_op_term (expr : Untyped_ast.Expr.t) =
    match expr with
    | Fun_call _ -> format_expr expr
    | _ -> format_expr_term expr
  and indent_expr expr = indent (Node.with_value expr ~f:format_expr)
  and format_branches_aux
        : 'a. ('a Node.t * Untyped_ast.Expr.t Node.t) Nonempty.t -> f:('a -> t) -> t
    =
   fun branches ~f ->
    let format_branch (pattern, expr) ~is_last_branch =
      Force_break
      ^^ Group
           (Group (Text "|" ^| Node.with_value pattern ~f ^| Text "->")
            ^^ format_branch_expr expr ~is_last_branch)
    in
    let (last :: rest) = Nonempty.rev branches in
    concat_all
      (List.rev
         (format_branch last ~is_last_branch:true
          :: List.map rest ~f:(format_branch ~is_last_branch:false)))
  and format_match_branch_pattern (pattern : Untyped_ast.Pattern.t) =
    match pattern with
    | Type_annotation _ ->
      (* An unparenthesized type annotation in a match branch gets confused with a
         function type due to the following arrow, so force parentheses to be added. *)
      format_pattern_term pattern
    | _ -> format_pattern pattern
  and format_branch_expr (expr : Untyped_ast.Expr.t Node.t) ~is_last_branch =
    Node.with_value expr ~f:(function
      | (Match _ | Match_function _) as expr when not is_last_branch ->
        indent (parens (format_expr expr))
      | _ -> indent_expr expr)
  and format_match_branches branches =
    format_branches_aux branches ~f:format_match_branch_pattern
  and format_handle_branches branches =
    format_branches_aux branches ~f:(function
      | `Value pattern -> format_match_branch_pattern pattern
      | `Effect ({ operation; args } : Untyped_ast.Effect_pattern.t) ->
        Text "<"
        ^^ format_application
             (format_qualified operation ~f:format_value_name)
             (List.map (Nonempty.to_list args) ~f:(Node.with_value ~f:format_pattern))
        ^^ Text ">")
  and format_let_binding
    ~rec_
    ~bindings:((first_pat, first_fixity, first_expr) :: bindings : _ Nonempty.t)
    =
    let format_binding keyword pattern fixity expr =
      Node.with_value2
        pattern
        expr
        ~f:(fun (pattern : Untyped_ast.Pattern.t) (expr : Untyped_ast.Expr.t) ->
        Group
          (match pattern, expr with
           | Catch_all (Some _fun_name), Lambda (args, body) ->
             Node.with_value body ~f:(fun body ->
               format_equals
                 (Text keyword
                  ^| format_fixity fixity
                  ^| format_pattern pattern
                  ^| separated
                       (List.map
                          (Nonempty.to_list args)
                          ~f:(Node.with_value ~f:format_pattern_term)))
                 (format_expr body))
           | Catch_all (Some _fun_name), Match_function branches ->
             format_equals
               (Text keyword ^| format_fixity fixity ^| format_pattern pattern)
               (Text "match")
             ^^ indent ~prefix:Empty (format_match_branches branches)
           | _ ->
             format_equals
               (Text keyword ^| format_fixity fixity ^| format_pattern pattern)
               (format_expr expr)))
    in
    separated
      ~sep:Force_break
      (format_binding (if rec_ then "let" else "let'") first_pat first_fixity first_expr
       :: List.map bindings ~f:(fun (pat, fixity, expr) ->
            format_binding "and" pat fixity expr))
  in
  let rec format_common : _ Module.common -> t = function
    | Extern (name, fixity, type_, extern_name) ->
      Group
        (format_equals
           (format_annotated
              (Text "extern" ^| format_fixity fixity ^| format_value_name name)
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
      let effect_and_params =
        Text "effect"
        ^| format_params_application (Effect_name.to_string effect_name) params
      in
      (match operations with
       | None -> Group effect_and_params
       | Some operations ->
         format_block
           effect_and_params
           '='
           (List.map operations ~f:(fun { name; args; result } ->
              Module.Val (name, None, (Function (args, Effect_union [], result), []))))
           ~f:format_sig
           ~on_empty:(Text "{}"))
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
    | Val (name, fixity, type_) ->
      Group
        (format_annotated
           (Text "val" ^| format_fixity fixity ^| format_value_name name)
           (format_type' type_))
    | Trait_sig _ -> failwith "TODO: format trait sig"
    | Module_sig (module_name, sigs) ->
      format_block
        (Text "module" ^| Text (Module_name.to_string module_name))
        ':'
        sigs
        ~f:(Node.with_value ~f:format_sig)
        ~on_empty:(Text "{}")
  and format_def : _ Module.def -> t = function
    | Common_def common -> format_common common
    | Let bindings -> format_let_binding ~rec_:true ~bindings
    | Module (module_name, sigs, defs) ->
      format_sigs_and_defs
        (Text "module" ^| Text (Module_name.to_string module_name))
        ~sigs
        ~defs
        ~f_sigs:(Node.with_value ~f:format_sig)
        ~f_defs:(Node.with_value ~f:format_def)
    | Trait _ | Impl _ -> failwith "TODO: formatting traits and impls"
  in
  (* TODO: We don't distinguish in the AST between empty sigs and no sigs at all, which
     seems wrong - it can make sense to have e.g. a test file with an empty signature. *)
  let module_sig =
    if List.is_empty sigs
    then Empty
    else
      format_block
        (Text "module")
        ':'
        sigs
        ~f:(Node.with_value ~f:format_sig)
        ~on_empty:Empty
  in
  (* TODO: We don't keep track of where the module sig is written in the original code,
     which leads to shuffling it around, which is confusing. *)
  let defs = List.map defs ~f:(Node.with_value ~f:format_def) in
  let sigs_and_defs =
    match module_sig with
    | Empty -> defs
    | _ -> module_sig :: defs
  in
  separated ~sep:(Force_break ^^ Force_break) sigs_and_defs
;;

let format ?(config = Config.default) module_ =
  format_to_document ~config module_
  |> Auto_format.format ~max_line_length:config.max_line_length
;;
