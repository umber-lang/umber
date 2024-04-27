open Import
open Sedlexing

type token = Parser.token =
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
  | RESUME
  | PIPE
  | PERIOD
  | OPERATOR of Ustring.t
  | N_PERIODS of int
  | MODULE
  | MATCH
  | L_PAREN
  | L_BRACKET
  | L_BRACE
  | LOWER_NAME of Ustring.t
  | LET_NONREC
  | LET
  | LESS_THAN
  | INT of int
  | INFIXR
  | INFIXL
  | INFIX
  | IN
  | IMPORT
  | IMPL
  | IF
  | HANDLE
  | GREATER_THAN
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

(* TODO: See if we can get ppx_import working again so we don't have to write out these
   type declarations. *)

type 'a terminal = 'a Parser.MenhirInterpreter.terminal =
  | T_error : unit terminal
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
  | T_RESUME : unit terminal
  | T_PIPE : unit terminal
  | T_PERIOD : unit terminal
  | T_OPERATOR : Ustring.t terminal
  | T_N_PERIODS : int terminal
  | T_MODULE : unit terminal
  | T_MATCH : unit terminal
  | T_L_PAREN : unit terminal
  | T_L_BRACKET : unit terminal
  | T_L_BRACE : unit terminal
  | T_LOWER_NAME : Ustring.t terminal
  | T_LET_NONREC : unit terminal
  | T_LET : unit terminal
  | T_LESS_THAN : unit terminal
  | T_INT : int terminal
  | T_INFIXR : unit terminal
  | T_INFIXL : unit terminal
  | T_INFIX : unit terminal
  | T_IN : unit terminal
  | T_IMPORT : unit terminal
  | T_IMPL : unit terminal
  | T_IF : unit terminal
  | T_HANDLE : unit terminal
  | T_GREATER_THAN : unit terminal
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
  | T_AS : unit terminal
  | T_ARROW : unit terminal
  | T_AND : unit terminal
[@@deriving sexp_of]

type 'a nonterminal = 'a Parser.MenhirInterpreter.nonterminal =
  | N_val_operator : Ustring.t nonterminal
  | N_val_name : Ustring.t nonterminal
  | N_type_tuple_or_fun : Parser_scope.Module_path.relative Type_scheme.type_ nonterminal
  | N_type_term : Parser_scope.Module_path.relative Type_scheme.type_ nonterminal
  | N_type_record : Parser_scope.Module_path.relative Type_decl.decl nonterminal
  | N_type_non_fun : Parser_scope.Module_path.relative Type_scheme.type_ nonterminal
  | N_type_fun_args
      : Parser_scope.Module_path.relative Type_scheme.type_ Nonempty.t nonterminal
  | N_type_expr : Parser_scope.Module_path.relative Type_scheme.type_ nonterminal
  | N_type_effect : Parser_scope.Module_path.relative Type_scheme.effects nonterminal
  | N_type_decl : Parser_scope.Module_path.relative Type_decl.decl nonterminal
  | N_type_cnstr_decl
      : (Parser_scope.Cnstr_name.t
        * Parser_scope.Module_path.relative Type_scheme.type_ list)
        nonterminal
  | N_stmt_sig_ : Parser_scope.Module_path.relative Module.sig_ nonterminal
  | N_stmt_common : Parser_scope.Module_path.relative Module.common nonterminal
  | N_stmt_
      : ( Umber__Untyped.Pattern.t
        , Untyped.Expr.t
        , Parser_scope.Module_path.relative )
        Module.def
        nonterminal
  | N_separated_nonempty_list_PIPE_type_cnstr_decl_
      : (Parser_scope.Cnstr_name.t
        * Parser_scope.Module_path.relative Type_scheme.type_ list)
        list
        nonterminal
  | N_separated_nonempty_list_PIPE_match_branch_
      : (Umber__Untyped.Pattern.t Node.t * Untyped.Expr.t Node.t) list nonterminal
  | N_separated_nonempty_list_PIPE_handle_branch_
      : ([ `Effect of Umber__Untyped.Effect_pattern.t
         | `Value of Umber__Untyped.Pattern.t
         ]
         Node.t
        * Untyped.Expr.t Node.t)
        list
        nonterminal
  | N_separated_nonempty_list_COMMA_type_effect_
      : Parser_scope.Module_path.relative Type_scheme.effects list nonterminal
  | N_separated_nonempty_list_COMMA_import_paths_ : Module.Import.Paths.t list nonterminal
  | N_separated_nonempty_list_AND_let_binding_
      : (Umber__Untyped.Pattern.t Node.t * Untyped.Expr.t Node.t) list nonterminal
  | N_qualified_with_loc_tuple_expr___
      : (Ustring.t list * Untyped.Expr.t Node.t list Node.t) nonterminal
  | N_qualified_val_name_ : (Ustring.t list * Ustring.t) nonterminal
  | N_qualified_parens_with_loc_op_section___
      : (Ustring.t list * Untyped.Expr.t Node.t) nonterminal
  | N_qualified_parens_operator__
      : (Ustring.t list * (Ustring.t list * Ustring.t)) nonterminal
  | N_qualified_either_LOWER_NAME_UPPER_NAME__ : (Ustring.t list * Ustring.t) nonterminal
  | N_qualified_UPPER_NAME_ : (Ustring.t list * Ustring.t) nonterminal
  | N_qualified_LOWER_NAME_ : (Ustring.t list * Ustring.t) nonterminal
  | N_prog : Untyped.Module.t nonterminal
  | N_pattern_term
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        nonterminal
  | N_pattern_name : Parser_scope.Value_name.t option nonterminal
  | N_pattern
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        nonterminal
  | N_option_type_effects_
      : Parser_scope.Module_path.relative Type_scheme.effects option nonterminal
  | N_option_preceded_EQUALS_type_decl__
      : Parser_scope.Module_path.relative Type_decl.decl option nonterminal
  | N_option_preceded_EQUALS_pattern__
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        option
        nonterminal
  | N_option_preceded_EQUALS_expr__ : Untyped.Expr.t Node.t option nonterminal
  | N_option_preceded_EQUALS_braces_list_stmt_sig____
      : Parser_scope.Module_path.relative Module.sig_ Node.t list option nonterminal
  | N_option_parens_fixity__ : Fixity.t option nonterminal
  | N_operator : (Ustring.t list * Ustring.t) nonterminal
  | N_op_section : Untyped.Expr.t nonterminal
  | N_nonempty_list_with_loc_pattern_term__
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        Node.t
        list
        nonterminal
  | N_nonempty_list_with_loc_expr_term__ : Untyped.Expr.t Node.t list nonterminal
  | N_nonempty_list_type_term_
      : Parser_scope.Module_path.relative Type_scheme.type_ list nonterminal
  | N_nonempty_list_pattern_term_
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        list
        nonterminal
  | N_nonempty_list_pattern_
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        list
        nonterminal
  | N_nonempty_list_LOWER_NAME_ : Ustring.t list nonterminal
  | N_n_periods : int nonterminal
  | N_match_branches
      : (Umber__Untyped.Pattern.t Node.t * Untyped.Expr.t Node.t) Nonempty.t nonterminal
  | N_match_branch : (Umber__Untyped.Pattern.t Node.t * Untyped.Expr.t Node.t) nonterminal
  | N_loption_trait_bound_ : Trait_bound.t nonterminal
  | N_loption_separated_nonempty_list_PIPE_type_cnstr_decl__
      : (Parser_scope.Cnstr_name.t
        * Parser_scope.Module_path.relative Type_scheme.type_ list)
        list
        nonterminal
  | N_loption_separated_nonempty_list_COMMA_type_effect__
      : Parser_scope.Module_path.relative Type_scheme.effects list nonterminal
  | N_loption_preceded_FILE_MODULE_braces_list_stmt_sig____
      : Parser_scope.Module_path.relative Module.sig_ Node.t list nonterminal
  | N_loption_equals_defs_
      : ( Umber__Untyped.Pattern.t
        , Untyped.Expr.t
        , Parser_scope.Module_path.relative )
        Module.def
        Node.t
        list
        nonterminal
  | N_loption_colon_sigs_
      : Parser_scope.Module_path.relative Module.sig_ Node.t list nonterminal
  | N_literal : Literal.t nonterminal
  | N_list_type_term_
      : Parser_scope.Module_path.relative Type_scheme.type_ list nonterminal
  | N_list_stmt_sig_
      : Parser_scope.Module_path.relative Module.sig_ Node.t list nonterminal
  | N_list_stmt_
      : ( Umber__Untyped.Pattern.t
        , Untyped.Expr.t
        , Parser_scope.Module_path.relative )
        Module.def
        Node.t
        list
        nonterminal
  | N_list_LOWER_NAME_ : Ustring.t list nonterminal
  | N_let_rec : bool nonterminal
  | N_let_binding : (Umber__Untyped.Pattern.t Node.t * Untyped.Expr.t Node.t) nonterminal
  | N_import_stmt : Module.Import.t nonterminal
  | N_import_paths_after_module : Module.Import.Paths.t Nonempty.t nonterminal
  | N_import_paths : Module.Import.Paths.t nonterminal
  | N_flexible_nonempty_COMMA_type_non_fun_
      : Parser_scope.Module_path.relative Type_scheme.type_ Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_type_annot_non_fun_LOWER_NAME__
      : (Ustring.t * Parser_scope.Module_path.relative Type_scheme.type_) Nonempty.t
        nonterminal
  | N_flexible_nonempty_COMMA_record_field_EQUALS_pattern__
      : (Parser_scope.Value_name.t
        * ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
          , Parser_scope.Module_path.relative )
          Pattern.t
          option)
        Nonempty.t
        nonterminal
  | N_flexible_nonempty_COMMA_record_field_EQUALS_expr__
      : (Parser_scope.Value_name.t * Untyped.Expr.t Node.t option) Nonempty.t nonterminal
  | N_flexible_nonempty_COMMA_pattern_
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        Nonempty.t
        nonterminal
  | N_flexible_nonempty_COMMA_pair_UPPER_NAME_type_params_nonempty__
      : (Ustring.t, Parser_scope.Type_param_name.t Nonempty.t) Import.Tuple2.t Nonempty.t
        nonterminal
  | N_flexible_nonempty_COMMA_expr_ : Untyped.Expr.t Node.t Nonempty.t nonterminal
  | N_flexible_list_COMMA_type_non_fun_
      : Parser_scope.Module_path.relative Type_scheme.type_ list nonterminal
  | N_flexible_list_COMMA_type_annot_non_fun_LOWER_NAME__
      : (Ustring.t * Parser_scope.Module_path.relative Type_scheme.type_) list nonterminal
  | N_flexible_list_COMMA_record_field_EQUALS_pattern__
      : (Parser_scope.Value_name.t
        * ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
          , Parser_scope.Module_path.relative )
          Pattern.t
          option)
        list
        nonterminal
  | N_flexible_list_COMMA_record_field_EQUALS_expr__
      : (Parser_scope.Value_name.t * Untyped.Expr.t Node.t option) list nonterminal
  | N_flexible_list_COMMA_pattern_
      : ( Parser_scope.Module_path.relative Type_scheme.Bounded.t
        , Parser_scope.Module_path.relative )
        Pattern.t
        list
        nonterminal
  | N_flexible_list_COMMA_pair_UPPER_NAME_type_params_nonempty__
      : (Ustring.t, Parser_scope.Type_param_name.t Nonempty.t) Import.Tuple2.t list
        nonterminal
  | N_flexible_list_COMMA_expr_ : Untyped.Expr.t Node.t list nonterminal
  | N_fixity : Fixity.t nonterminal
  | N_expr_term : Untyped.Expr.t nonterminal
  | N_expr_op_tree
      : (Parser_scope.Value_name.Relative.t Node.t, Untyped.Expr.t Node.t) Btree.t
        nonterminal
  | N_expr_op_term : Untyped.Expr.t nonterminal
  | N_expr_ : Untyped.Expr.t nonterminal
  | N_either_val_name_UPPER_NAME_ : Ustring.t nonterminal
  | N_either_LOWER_NAME_UPPER_NAME_ : Ustring.t nonterminal
  | N_either_COLON_COLON_SPACED_ : unit nonterminal
  | N_effect_pattern_
      : [ `Effect of Umber__Untyped.Effect_pattern.t
        | `Value of Umber__Untyped.Pattern.t
        ]
        nonterminal
[@@deriving sexp_of]

let sexp_of_production prod =
  let (X symbol) = I.lhs prod in
  let empty_sexp _ = Sexp.List [] in
  match symbol with
  | T terminal -> sexp_of_terminal empty_sexp terminal
  | N nonterminal -> sexp_of_nonterminal empty_sexp nonterminal
;;

let rec lex ~print_tokens_to lexbuf =
  let token = Lexer.read lexbuf in
  sexp_of_token token |> fprint_s ~out:print_tokens_to;
  match token with
  | EOF -> ()
  | _ -> lex ~print_tokens_to lexbuf
;;

let try_lex ~print_tokens_to lexbuf =
  Compilation_error.try_with' (fun () -> lex ~print_tokens_to lexbuf)
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
        | Some prod -> sprintf !". (Last production: %{sexp: production})" prod
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
  Compilation_error.try_with' (fun () -> parse ?print_tokens_to lexbuf)
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
