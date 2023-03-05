open Import
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
  | LET_NONREC
  | LET
  | INT of int
  | INFIXR
  | INFIXL
  | INFIX
  | IN
  | IMPORT
  | IMPL
  | IF
  | FLOAT of float
  | FILE_MODULE
  | FAT_ARROW
  | EXTERN
  | EQUALS
  | EOF
  | ELSE
  | EFFECT
  | COMMA
  | COLON_SPACED
  | COLON
  | CHAR of Uchar.t
  | BACKSLASH
  | ASTERISK
  | AS
  | ARROW
  | AND
[@@deriving sexp]

let fprint_s sexp ~out =
  Sexp.to_string_hum sexp |> Out_channel.output_string out;
  Out_channel.newline out
;;

(* Code from:
   https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)

module I = Parser.MenhirInterpreter

type 'a terminal = 'a Parser.MenhirInterpreter.terminal =
  | T_error : unit terminal
  | T_WITHOUT : unit terminal
  | T_WITH : unit terminal
  | T_VAL : unit terminal
  | T_UPPER_NAME : Ustring.t terminal
  | T_UNDERSCORE : unit terminal
  | T_TYPE : unit terminal
  | T_TRAIT : unit terminal
  | T_THEN : unit terminal
  | T_STRING : Ustring.t terminal
  | T_R_PAREN : unit terminal
  | T_R_BRACKET : unit terminal
  | T_R_BRACE : unit terminal
  | T_PIPE : unit terminal
  | T_PERIOD : unit terminal
  | T_OPERATOR : Ustring.t terminal
  | T_MODULE : unit terminal
  | T_MATCH : unit terminal
  | T_L_PAREN : unit terminal
  | T_L_BRACKET : unit terminal
  | T_L_BRACE : unit terminal
  | T_LOWER_NAME : Ustring.t terminal
  | T_LET_NONREC : unit terminal
  | T_LET : unit terminal
  | T_INT : int terminal
  | T_INFIXR : unit terminal
  | T_INFIXL : unit terminal
  | T_INFIX : unit terminal
  | T_IN : unit terminal
  | T_IMPORT : unit terminal
  | T_IMPL : unit terminal
  | T_IF : unit terminal
  | T_FLOAT : float terminal
  | T_FILE_MODULE : unit terminal
  | T_FAT_ARROW : unit terminal
  | T_EXTERN : unit terminal
  | T_EQUALS : unit terminal
  | T_EOF : unit terminal
  | T_ELSE : unit terminal
  | T_EFFECT : unit terminal
  | T_COMMA : unit terminal
  | T_COLON_SPACED : unit terminal
  | T_COLON : unit terminal
  | T_CHAR : Uchar.t terminal
  | T_BACKSLASH : unit terminal
  | T_ASTERISK : unit terminal
  | T_AS : unit terminal
  | T_ARROW : unit terminal
  | T_AND : unit terminal
[@@deriving sexp_of]

type 'a nonterminal = 'a Parser.MenhirInterpreter.nonterminal =
  | N_val_operator : Ustring.t nonterminal
  | N_val_name : Ustring.t nonterminal
  | N_type_tuple_or_fun : (Type.Param.t, Core.never_returns) Type.Expr.t nonterminal
  | N_type_term : (Type.Param.t, Core.never_returns) Type.Expr.t nonterminal
  | N_type_record : Type.Decl.decl nonterminal
  | N_type_non_fun : (Type.Param.t, Core.never_returns) Type.Expr.t nonterminal
  | N_type_fun_args
      : (Type.Param.t, Core.never_returns) Type.Expr.t Nonempty.t nonterminal
  | N_type_expr : (Type.Param.t, Core.never_returns) Type.Expr.t nonterminal
  | N_type_decl : Type.Decl.decl nonterminal
  | N_type_cnstr_decl
      : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) nonterminal
  | N_stmt_sig_ : Module.sig_ nonterminal
  | N_stmt_common : Module.common nonterminal
  | N_stmt_ : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def nonterminal
  | N_separated_nonempty_list_PIPE_type_cnstr_decl_
      : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) list nonterminal
  | N_separated_nonempty_list_PIPE_match_branch_
      : (Umber__Untyped.Pattern.t * Untyped.Expr.t) list nonterminal
  | N_separated_nonempty_list_PERIOD_UPPER_NAME_ : Ustring.t list nonterminal
  | N_separated_nonempty_list_AND_let_binding_
      : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Node.t list nonterminal
  | N_qualified_val_name_ : (Ustring.t list * Ustring.t) nonterminal
  | N_qualified_tuple_expr__ : (Ustring.t list * Untyped.Expr.t list) nonterminal
  | N_qualified_parens_operator__
      : (Ustring.t list * (Ustring.t list * Ustring.t)) nonterminal
  | N_qualified_parens_op_section__ : (Ustring.t list * Untyped.Expr.t) nonterminal
  | N_qualified_either_LOWER_NAME_UPPER_NAME__ : (Ustring.t list * Ustring.t) nonterminal
  | N_qualified_UPPER_NAME_ : (Ustring.t list * Ustring.t) nonterminal
  | N_prog : Untyped.Module.t nonterminal
  | N_pattern_term : Type.Scheme.Bounded.t Pattern.t nonterminal
  | N_pattern_name : Parser_scope.Value_name.t option nonterminal
  | N_pattern : Type.Scheme.Bounded.t Pattern.t nonterminal
  | N_option_preceded_EQUALS_type_decl__ : Type.Decl.decl option nonterminal
  | N_option_preceded_EQUALS_pattern__
      : Type.Scheme.Bounded.t Pattern.t option nonterminal
  | N_option_preceded_EQUALS_list_stmt_sig___ : Module.sig_ Node.t list option nonterminal
  | N_option_preceded_EQUALS_expr__ : Untyped.Expr.t option nonterminal
  | N_option_parens_fixity__ : Fixity.t option nonterminal
  | N_operator : (Ustring.t list * Ustring.t) nonterminal
  | N_op_section : Untyped.Expr.t nonterminal
  | N_nonempty_list_type_term_
      : (Type.Param.t, Core.never_returns) Type.Expr.t list nonterminal
  | N_nonempty_list_pattern_term_ : Type.Scheme.Bounded.t Pattern.t list nonterminal
  | N_nonempty_list_expr_term_ : Untyped.Expr.t list nonterminal
  | N_nonempty_list_LOWER_NAME_ : Ustring.t list nonterminal
  | N_match_branches : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Nonempty.t nonterminal
  | N_match_branch : (Umber__Untyped.Pattern.t * Untyped.Expr.t) nonterminal
  | N_loption_trait_bound_ : Trait_bound.t nonterminal
  | N_loption_separated_nonempty_list_PIPE_type_cnstr_decl__
      : (Parser_scope.Cnstr_name.t * Type.Scheme.Table.key list) list nonterminal
  | N_loption_preceded_FILE_MODULE_braces_list_stmt_sig____
      : Module.sig_ Node.t list nonterminal
  | N_loption_equals_defs_
      : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list nonterminal
  | N_loption_colon_sigs_ : Module.sig_ Node.t list nonterminal
  | N_literal : Literal.t nonterminal
  | N_list_type_term_ : Type.Scheme.Table.key list nonterminal
  | N_list_stmt_sig_ : Module.sig_ Node.t list nonterminal
  | N_list_stmt_
      : (Umber__Untyped.Pattern.t, Untyped.Expr.t) Module.def Node.t list nonterminal
  | N_list_LOWER_NAME_ : Ustring.t list nonterminal
  | N_let_rec : bool nonterminal
  | N_let_binding_ : (Umber__Untyped.Pattern.t * Untyped.Expr.t) nonterminal
  | N_let_binding : (Umber__Untyped.Pattern.t * Untyped.Expr.t) Node.t nonterminal
  | N_import_stmt : Module.common nonterminal
  | N_flexible_nonempty_COMMA_type_non_fun_
      : (Type.Param.t, Core.never_returns) Type.Expr.t Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_type_annot_non_fun_LOWER_NAME__
      : (Ustring.t * Type.Scheme.Table.key) Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_record_field_EQUALS_pattern__
      : (Parser_scope.Value_name.t * Type.Scheme.Bounded.t Pattern.t option) Nonempty.t
        nonterminal
  | N_flexible_nonempty_COMMA_record_field_EQUALS_expr__
      : (Parser_scope.Value_name.t * Untyped.Expr.t option) Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_pattern_
      : Type.Scheme.Bounded.t Pattern.t Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_pair_UPPER_NAME_type_params_nonempty__
      : (Ustring.t, Type.Param.t Nonempty.t) Import.Tuple2.t Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_import_item_
      : Parser_scope.Unidentified_name.t Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_expr_ : Untyped.Expr.t Nonempty.t nonterminal
  | N_flexible_list_COMMA_type_non_fun_
      : (Type.Param.t, Core.never_returns) Type.Expr.t list nonterminal
  | N_flexible_list_COMMA_type_annot_non_fun_LOWER_NAME__
      : (Ustring.t * Type.Scheme.Table.key) list nonterminal
  | N_flexible_list_COMMA_record_field_EQUALS_pattern__
      : (Parser_scope.Value_name.t * Type.Scheme.Bounded.t Pattern.t option) list
        nonterminal
  | N_flexible_list_COMMA_record_field_EQUALS_expr__
      : (Parser_scope.Value_name.t * Untyped.Expr.t option) list nonterminal
  | N_flexible_list_COMMA_pattern_ : Type.Scheme.Bounded.t Pattern.t list nonterminal
  | N_flexible_list_COMMA_pair_UPPER_NAME_type_params_nonempty__
      : (Ustring.t, Type.Param.t Nonempty.t) Import.Tuple2.t list nonterminal
  | N_flexible_list_COMMA_import_item_ : Parser_scope.Unidentified_name.t list nonterminal
  | N_flexible_list_COMMA_expr_ : Untyped.Expr.t list nonterminal
  | N_fixity : Fixity.t nonterminal
  | N_expr_term : Untyped.Expr.t nonterminal
  | N_expr_op_tree
      : (Parser_scope.Value_name.Qualified.t, Untyped.Expr.t) Btree.t nonterminal
  | N_expr_op_term : Untyped.Expr.t nonterminal
  | N_expr : Untyped.Expr.t nonterminal
  | N_either_val_name_UPPER_NAME_ : Ustring.t nonterminal
  | N_either_LOWER_NAME_UPPER_NAME_ : Ustring.t nonterminal
  | N_either_COLON_COLON_SPACED_ : unit nonterminal
[@@deriving sexp_of]

let handle_syntax_error f =
  try Ok (f ()) with
  | Lexer.Syntax_error (pos, msg) ->
    Error
      (Compilation_error.create
         Syntax_error
         ~span:(Span.of_pos pos)
         ~msg:(Atom (Ustring.to_string msg)))
;;

let rec lex ~print_tokens_to lexbuf =
  let token = Lexer.read lexbuf in
  sexp_of_token token |> fprint_s ~out:print_tokens_to;
  match token with
  | EOF -> ()
  | _ -> lex ~print_tokens_to lexbuf
;;

let try_lex ~print_tokens_to lexbuf =
  handle_syntax_error (fun () -> lex ~print_tokens_to lexbuf)
;;

let parse ?print_tokens_to lexbuf =
  let lex_remaining ~print_tokens_to lexbuf =
    Option.iter print_tokens_to ~f:(fun print_tokens_to -> lex ~print_tokens_to lexbuf)
  in
  let last_prod = ref None in
  let rec parse ?print_tokens_to last_token lexbuf checkpoint =
    match checkpoint with
    | I.InputNeeded _env ->
      let token = Lexer.read lexbuf in
      Option.iter print_tokens_to ~f:(fun out -> sexp_of_token token |> fprint_s ~out);
      let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
      let checkpoint = I.offer checkpoint (token, start_pos, end_pos) in
      parse ?print_tokens_to token lexbuf checkpoint
    | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint
    | I.AboutToReduce (_, prod) ->
      last_prod := Some prod;
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint
    | I.HandlingError env ->
      lex_remaining ~print_tokens_to lexbuf;
      let state = I.current_state_number env in
      let prod_msg =
        match !last_prod with
        | Some prod ->
          let (X symbol) = I.lhs prod in
          let empty_sexp _ = Sexp.List [] in
          let str =
            match symbol with
            | T terminal -> sexp_of_terminal empty_sexp terminal
            | N nonterminal -> sexp_of_nonterminal empty_sexp nonterminal
          in
          sprintf ". (Last production: %s)" (Sexp.to_string str)
        | None -> ""
      in
      Lexer.syntax_error ~msg:(sprintf "Parser error in state %d%s" state prod_msg) lexbuf
    | I.Accepted v -> v
    | I.Rejected ->
      lex_remaining ~print_tokens_to lexbuf;
      Lexer.syntax_error ~msg:"Invalid syntax (parser rejected the input)" lexbuf
  in
  let start, _ = lexing_positions lexbuf in
  Module.with_filename
    (parse ?print_tokens_to EOF lexbuf (Parser.Incremental.prog start))
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
  with_file ~f:(fun lexbuf -> try_lex ~print_tokens_to lexbuf)
;;

let parse_file ?print_tokens_to = with_file ~f:(try_parse ?print_tokens_to)
