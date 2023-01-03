open Import
open Names
open Sedlexing

type token = Parser.token =
  | WITHOUT
  | WITH
  | VAL
  | UPPER_NAME of Ustring.t
  | UNDERSCORE
  | TYPE
  | TRAIT
  | THEN
  | STRING of Ustring.t
  | R_PAREN
  | R_BRACKET
  | R_BRACE
  | PIPE
  | PERIOD
  | OPERATOR of Ustring.t
  | MODULE
  | MATCH
  | L_PAREN
  | L_BRACKET
  | L_BRACE
  | LOWER_NAME of Ustring.t
  | LINE_SEP
  | LET_NONREC
  | LET
  | INT of int
  | INFIXR
  | INFIXL
  | INFIX
  | INDENT
  | IMPORT
  | IMPL
  | IF
  | FLOAT of float
  | FILE_MODULE
  | FAT_ARROW
  | EXTERN
  | EQUALS_ONLY_LINE
  | EQUALS
  | EOF
  | ELSE
  | DEDENT
  | COMMA
  | COLON_SPACED
  | COLON
  | CHAR of Uchar.t
  | BACKSLASH
  | ASTERISK
  | AS
  | ARROW
  | ALIAS
[@@deriving sexp]

let fprint_s sexp ~out =
  Sexp.to_string_hum sexp |> Out_channel.output_string out;
  Out_channel.newline out
;;

(* Code from:
   https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)

module I = Parser.MenhirInterpreter

module Terminal = struct
  type 'a t = 'a Parser.MenhirInterpreter.terminal =
    | T_error : unit t
    | T_WITHOUT : unit t
    | T_WITH : unit t
    | T_VAL : unit t
    | T_UPPER_NAME : Ustring.t t
    | T_UNDERSCORE : unit t
    | T_TYPE : unit t
    | T_TRAIT : unit t
    | T_THEN : unit t
    | T_STRING : Ustring.t t
    | T_R_PAREN : unit t
    | T_R_BRACKET : unit t
    | T_R_BRACE : unit t
    | T_PIPE : unit t
    | T_PERIOD : unit t
    | T_OPERATOR : Ustring.t t
    | T_MODULE : unit t
    | T_MATCH : unit t
    | T_L_PAREN : unit t
    | T_L_BRACKET : unit t
    | T_L_BRACE : unit t
    | T_LOWER_NAME : Ustring.t t
    | T_LINE_SEP : unit t
    | T_LET_NONREC : unit t
    | T_LET : unit t
    | T_INT : int t
    | T_INFIXR : unit t
    | T_INFIXL : unit t
    | T_INFIX : unit t
    | T_INDENT : unit t
    | T_IMPORT : unit t
    | T_IMPL : unit t
    | T_IF : unit t
    | T_FLOAT : float t
    | T_FILE_MODULE : unit t
    | T_FAT_ARROW : unit t
    | T_EXTERN : unit t
    | T_EQUALS_ONLY_LINE : unit t
    | T_EQUALS : unit t
    | T_EOF : unit t
    | T_ELSE : unit t
    | T_DEDENT : unit t
    | T_COMMA : unit t
    | T_COLON_SPACED : unit t
    | T_COLON : unit t
    | T_CHAR : Uchar.t t
    | T_BACKSLASH : unit t
    | T_ASTERISK : unit t
    | T_AS : unit t
    | T_ARROW : unit t
    | T_ALIAS : unit t
  [@@deriving sexp_of]
end

module Nonterminal = struct
  type 'a t = 'a Parser.MenhirInterpreter.nonterminal =
    | N_val_operator : Ustring.t t
    | N_val_name : Ustring.t t
    | N_type_tuple_or_fun : (Type.Param.t, Core.never_returns) Type.Expr.t t
    | N_type_term : (Type.Param.t, Core.never_returns) Type.Expr.t t
    | N_type_record : Type.Decl.decl t
    | N_type_non_fun : (Type.Param.t, Core.never_returns) Type.Expr.t t
    | N_type_fun_args : (Type.Param.t, Core.never_returns) Type.Expr.t Nonempty.t t
    | N_type_expr : (Type.Param.t, Core.never_returns) Type.Expr.t t
    | N_type_decl_variants
        : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) list t
    | N_type_decl : Type.Decl.decl t
    | N_type_cnstr_decl : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) t
    | N_stmt_sig_ : Module.sig_ t
    | N_stmt_common : Module.common t
    | N_stmt_ : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def t
    | N_separated_nonempty_list_option_LINE_SEP__stmt_sig_ : Module.sig_ Node.t list t
    | N_separated_nonempty_list_option_LINE_SEP__stmt_
        : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list t
    | N_separated_nonempty_list_line_sep_pipe_type_cnstr_decl_
        : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) list t
    | N_separated_nonempty_list_line_sep_pipe_match_branch_
        : (Umber__Untyped.Pattern.t * Untyped.Expr.t) list t
    | N_separated_nonempty_list_PIPE_type_cnstr_decl_
        : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) list t
    | N_separated_nonempty_list_PIPE_match_branch_
        : (Umber__Untyped.Pattern.t * Untyped.Expr.t) list t
    | N_separated_nonempty_list_PERIOD_UPPER_NAME_ : Ustring.t list t
    | N_separated_nonempty_list_LINE_SEP_let_binding__
        : (Umber__Untyped.Pattern.t * Untyped.Expr.t) list t
    | N_separated_nonempty_list_LINE_SEP_let_binding_
        : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Node.t list t
    | N_separated_nonempty_list_COMMA_import_item_
        : Parser_scope.Unidentified_name.t list t
    | N_qualified_val_name_ : (Ustring.t list * Ustring.t) t
    | N_qualified_tuple_expr__ : (Ustring.t list * Untyped.Expr.t list) t
    | N_qualified_parens_operator__ : (Ustring.t list * (Ustring.t list * Ustring.t)) t
    | N_qualified_parens_op_section__ : (Ustring.t list * Untyped.Expr.t) t
    | N_qualified_either_LOWER_NAME_UPPER_NAME__ : (Ustring.t list * Ustring.t) t
    | N_qualified_UPPER_NAME_ : (Ustring.t list * Ustring.t) t
    | N_prog : Untyped.Module.t t
    | N_pattern_term : (Type.Scheme.Bounded.t, Value_name.t) Pattern.t t
    | N_pattern_name : Parser_scope.Value_name.t option t
    | N_pattern : (Type.Scheme.Bounded.t, Value_name.t) Pattern.t t
    | N_optional_sig_def
        : (Module.sig_ Node.t list
          * (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list)
          t
    | N_option_preceded_EQUALS_type_decl__ : Type.Decl.decl option t
    | N_option_preceded_EQUALS_pattern__
        : (Type.Scheme.Bounded.t, Value_name.t) Pattern.t option t
    | N_option_preceded_EQUALS_expr__ : Untyped.Expr.t option t
    | N_option_parens_fixity__ : Fixity.t option t
    | N_option_PIPE_ : unit option t
    | N_option_LINE_SEP_ : unit option t
    | N_operator : (Ustring.t list * Ustring.t) t
    | N_op_section : Untyped.Expr.t t
    | N_nonempty_list_type_term_ : (Type.Param.t, Core.never_returns) Type.Expr.t list t
    | N_nonempty_list_pattern_term_
        : (Type.Scheme.Bounded.t, Value_name.t) Pattern.t list t
    | N_nonempty_list_expr_term_ : Untyped.Expr.t list t
    | N_nonempty_list_LOWER_NAME_ : Ustring.t list t
    | N_match_branches : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Nonempty.t t
    | N_match_branch : (Umber__Untyped.Pattern.t * Untyped.Expr.t) t
    | N_loption_trait_bound_ : Trait_bound.t t
    | N_loption_separated_nonempty_list_option_LINE_SEP__stmt_sig__
        : Module.sig_ Node.t list t
    | N_loption_separated_nonempty_list_option_LINE_SEP__stmt__
        : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list t
    | N_loption_preceded_FILE_MODULE_block_lines_stmt_sig____ : Module.sig_ Node.t list t
    | N_literal : Literal.t t
    | N_list_type_term_ : Type.Scheme.Table.key list t
    | N_list_LOWER_NAME_ : Ustring.t list t
    | N_list_DEDENT_ : unit list t
    | N_let_rec : bool t
    | N_let_binding_ : (Umber__Untyped.Pattern.t * Untyped.Expr.t) t
    | N_let_binding : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Node.t t
    | N_import_stmt : Module.common t
    | N_flexible_optional_list_LINE_SEP_stmt_
        : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list t
    | N_flexible_nonempty_COMMA_type_non_fun_
        : (Type.Param.t, Core.never_returns) Type.Expr.t Nonempty.t t
    | N_flexible_nonempty_COMMA_type_annot_non_fun_LOWER_NAME__
        : (Ustring.t * Type.Scheme.Table.key) Nonempty.t t
    | N_flexible_nonempty_COMMA_record_field_equals_pattern__
        : (Parser_scope.Value_name.t
          * (Type.Scheme.Bounded.t, Value_name.t) Pattern.t option)
          Nonempty.t
          t
    | N_flexible_nonempty_COMMA_record_field_equals_expr__
        : (Parser_scope.Value_name.t * Untyped.Expr.t option) Nonempty.t t
    | N_flexible_nonempty_COMMA_pattern_
        : (Type.Scheme.Bounded.t, Value_name.t) Pattern.t Nonempty.t t
    | N_flexible_nonempty_COMMA_pair_UPPER_NAME_type_params_nonempty__
        : (Ustring.t, Type.Param.t Nonempty.t) Import.Tuple2.t Nonempty.t t
    | N_flexible_nonempty_COMMA_expr_ : Untyped.Expr.t Nonempty.t t
    | N_fixity : Fixity.t t
    | N_expr_term : Untyped.Expr.t t
    | N_expr_op_tree : (Parser_scope.Value_name.Qualified.t, Untyped.Expr.t) Btree.t t
    | N_expr_op_term : Untyped.Expr.t t
    | N_expr : Untyped.Expr.t t
    | N_either_val_name_UPPER_NAME_ : Ustring.t t
    | N_either_type_record_delimited_INDENT_type_record_DEDENT__ : Type.Decl.decl t
    | N_either_type_expr_bounded_delimited_INDENT_type_expr_bounded_DEDENT__
        : Type.Scheme.Bounded.t t
    | N_either_type_expr_delimited_INDENT_type_expr_DEDENT__
        : (Type.Param.t, Core.never_returns) Type.Expr.t t
    | N_either_pair_lines_stmt_sig__def__delimited_INDENT_pair_lines_stmt_sig__def__DEDENT__
        : (Module.sig_ Node.t list
          * (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list)
          t
    | N_either_nonempty_lines_stmt_sig__delimited_INDENT_nonempty_lines_stmt_sig__DEDENT__
        : Module.sig_ Node.t list t
    | N_either_nonempty_lines_stmt__delimited_INDENT_nonempty_lines_stmt__DEDENT__
        : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list t
    | N_either_lines_stmt_sig__delimited_INDENT_lines_stmt_sig__DEDENT__
        : Module.sig_ Node.t list t
    | N_either_LOWER_NAME_UPPER_NAME_ : Ustring.t t
    | N_either_EQUALS_EQUALS_ONLY_LINE_ : unit t
    | N_either_COLON_COLON_SPACED_ : unit t
  [@@deriving sexp_of]
end

let handle_syntax_error f =
  try Ok (f ()) with
  | Lexer.Syntax_error (pos, msg) ->
    Error
      (Compilation_error.create
         Syntax_error
         ~span:(Span.of_pos pos)
         ~msg:(Atom (Ustring.to_string msg)))
;;

let rec lex ~print_tokens_to lexbuf lexer =
  let token = Lexer.read lexer lexbuf in
  sexp_of_token token |> fprint_s ~out:print_tokens_to;
  match token with
  | EOF -> ()
  | _ -> lex ~print_tokens_to lexbuf lexer
;;

let try_lex ~print_tokens_to lexbuf lexer =
  handle_syntax_error (fun () -> lex ~print_tokens_to lexbuf lexer)
;;

let parse ?print_tokens_to lexbuf =
  let lex_remaining ~print_tokens_to lexbuf lexer =
    Option.iter print_tokens_to ~f:(fun print_tokens_to ->
      lex ~print_tokens_to lexbuf lexer)
  in
  let last_prod = ref None in
  let rec parse ?print_tokens_to last_token lexbuf checkpoint lexer =
    match checkpoint with
    | I.InputNeeded _env ->
      let token = Lexer.read lexer lexbuf in
      Option.iter print_tokens_to ~f:(fun out -> sexp_of_token token |> fprint_s ~out);
      let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
      let checkpoint = I.offer checkpoint (token, start_pos, end_pos) in
      parse ?print_tokens_to token lexbuf checkpoint lexer
    | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint lexer
    | I.AboutToReduce (_, prod) ->
      last_prod := Some prod;
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint lexer
    | I.HandlingError env ->
      (* TODO: you can actually resume the parser here, should look into that *)
      lex_remaining ~print_tokens_to lexbuf lexer;
      let state = I.current_state_number env in
      let prod_msg =
        match !last_prod with
        | Some prod ->
          let (X symbol) = I.lhs prod in
          let empty_sexp _ = Sexp.List [] in
          let str =
            match symbol with
            | T terminal -> Terminal.sexp_of_t empty_sexp terminal
            | N nonterminal -> Nonterminal.sexp_of_t empty_sexp nonterminal
          in
          sprintf ". (Last production: %s)" (Sexp.to_string str)
        | None -> ""
      in
      Lexer.syntax_error ~msg:(sprintf "Parser error in state %d%s" state prod_msg) lexbuf
    | I.Accepted v -> v
    | I.Rejected ->
      lex_remaining ~print_tokens_to lexbuf lexer;
      Lexer.syntax_error ~msg:"Invalid syntax (parser rejected the input)" lexbuf
  in
  let start, _ = lexing_positions lexbuf in
  Module.with_filename
    (parse ?print_tokens_to EOF lexbuf (Parser.Incremental.prog start) (Lexer.create ()))
    start.pos_fname
;;

let try_parse ?print_tokens_to lexbuf =
  handle_syntax_error (fun () -> parse ?print_tokens_to lexbuf)
;;

let with_file filename ~f =
  In_channel.with_file filename ~f:(fun inx ->
    let lexbuf = Utf8.from_channel inx in
    Sedlexing.set_filename lexbuf filename;
    f lexbuf)
;;

let lex_file ~print_tokens_to =
  with_file ~f:(fun lexbuf -> try_lex ~print_tokens_to lexbuf (Lexer.create ()))
;;

let parse_file ?print_tokens_to = with_file ~f:(try_parse ?print_tokens_to)
