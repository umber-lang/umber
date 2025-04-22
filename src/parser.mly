%{
  open Import
  open Parser_scope

  (* Disable deprecation warnings caused by Menhir using Not_found when inferring types *)
  [@@@ocaml.warning "-3"]
%}

%token IF
%token THEN
%token ELSE
%token LET
%token LET_NONREC
%token AND
%token IN
%token MATCH
%token HANDLE
%token RESUME
%token WITH
%token AS
%token TYPE
%token VAL
%token EXTERN
%token INFIX
%token INFIXL
%token INFIXR
%token MODULE
%token FILE_MODULE
%token TRAIT
%token IMPL
%token IMPORT
%token EFFECT

%token EQUALS
%token PIPE
%token AMPERSAND
%token COLON
%token COLON_SPACED
%token COMMA
%token BACKSLASH
%token ARROW
%token L_PAREN
%token R_PAREN
%token L_BRACKET
%token R_BRACKET
%token L_BRACE
%token R_BRACE
%token LESS_THAN
%token GREATER_THAN
%token PERIOD

%token <int> N_PERIODS
%token <int> INT
%token <float> FLOAT
%token <Uchar.t> CHAR
%token <Ustring.t> STRING

%token UNDERSCORE
%token <Ustring.t> LOWER_NAME
%token <Ustring.t> UPPER_NAME
%token <Ustring.t> OPERATOR

%token EOF

%start <Untyped.Module.t> prog
%start <unit> check_operator
%%

val_operator:
  | op = OPERATOR { op }
  | LESS_THAN { Ustring.of_uchar (Uchar.of_char '<') }
  | GREATER_THAN { Ustring.of_uchar (Uchar.of_char '>') }
  | LESS_THAN; GREATER_THAN { Ustring.of_string_exn "<>" }
  | PERIOD { Ustring.of_uchar (Uchar.of_char '.') }
  | n = N_PERIODS { Ustring.make n (Uchar.of_char '.') }

check_operator:
  | _op = val_operator; EOF { () }

operator:
  | COLON; name = qualified(val_name); colon { name }
  | COLON; name = qualified(UPPER_NAME); colon { name }
  | op = val_operator { [], op }

%inline colon: either(COLON, COLON_SPACED) { }

literal:
  | i = INT { Literal.Int i }
  | f = FLOAT { Literal.Float f }
  | c = CHAR { Literal.Char c }
  | s = STRING { Literal.String s }

pattern_name:
  | name = val_name { Some (Value_name.of_ustring_unchecked name) }
  | UNDERSCORE { None }

(* TODO: Make parens optional in tuple patterns only: do after adding sequence literal patterns *)
(* TODO: Allow constant value names? Prefixed with '.'? ':'? '='? *)
(* TODO: Also, allow infix constructors which start with : (or just any operator?) *)
pattern_term:
  | p = tuple(pattern) { single_or_list Pattern.tuple p }
  | l = literal { Pattern.Constant l }
  | name = pattern_name { Pattern.Catch_all name }
  | name = qualified(UPPER_NAME)
    { Pattern.Cnstr_appl (Cnstr_name.Relative.of_ustrings_unchecked name, []) }
  | fields = braces(record_literal_fields(pattern)) { Pattern.Record fields }

pattern:
  | p = pattern_term { p }
  | cnstr = qualified(UPPER_NAME); args = nonempty(pattern_term)
    { Pattern.Cnstr_appl (
        Cnstr_name.Relative.of_ustrings_unchecked cnstr, Nonempty.to_list args) }
  | left = pattern; PIPE; right = pattern { Pattern.Union (left, right) }
  | pat = pattern; AS; name = val_name
    { Pattern.As (pat, Value_name.of_ustring_unchecked name) }
  | annot = type_annot_constrained(pattern)
    { Pattern.Type_annotation (fst annot, Node.with_value (snd annot) ~f:Fn.id) }

op_section:
  | op = with_loc(operator); expr = with_loc(expr_op_term)
    { let op = Node.map op ~f:Value_name.Relative.of_ustrings_unchecked in
      Expr.Op_section { op_side = `Left; op; expr } }
  | expr = with_loc(expr_op_term); op = with_loc(operator)
    { let op = Node.map op ~f:Value_name.Relative.of_ustrings_unchecked in
      Expr.Op_section { op_side = `Right; op; expr } }

expr_term:
  | e = qualified(with_loc(tuple(expr)))
    { let path, exprs = e in
      let exprs, exprs_span = Node.with_value exprs ~f:Fn.id, Node.span exprs in
      Expr.qualified
        path
        (single_or_list (fun es -> Node.create (Expr.Tuple es) exprs_span) exprs) }
  | op = qualified(parens(operator))
    { let path1, (path2, op) = op in
      Expr.Name (Value_name.Relative.of_ustrings_unchecked (path1 @ path2, op)) }
  | op_section = qualified(parens(with_loc(op_section)))
    { let path, expr = op_section in
      Expr.qualified path expr }
  | name = qualified(either(LOWER_NAME, UPPER_NAME))
    { Expr.Name (Value_name.Relative.of_ustrings_unchecked name) }
  | RESUME { Expr.Name (Module_path.Relative.empty, Value_name.resume_keyword) }
  | l = literal { Expr.Literal l }
  | items = brackets(flexible_list(COMMA, expr)) { Expr.Seq_literal items }
  | fields = braces(record_literal_fields(expr)) { Expr.Record_literal fields }
  (* TODO: add more advanced syntax like Idris'
    http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html
     e.g. access nested fields like a->b->c
          map over fields with $= (or maybe := ?)
          also maybe allow multiple fields with the same type to share the
          type in the declaration e.g. type Point = { a, b, c : Int } 
    Maybe this could be covered instead (or accompanying) a good lenses/accessor library. *)
  | L_BRACE; record = expr; WITH; fields = record_literal_fields(expr); R_BRACE
    { Expr.Record_update (record, fields) }
  | record = with_loc(expr_term); PERIOD; field = with_loc(LOWER_NAME)
    { Expr.Record_field_access (record, Node.map ~f:Value_name.of_ustring_unchecked field) }

expr_op_term:
  | f = with_loc(expr_term); args = nonempty(with_loc(expr_term))
    { Expr.Fun_call (f, args) }
  | e = expr_term { e }

(* Expressions with binary operators are first parsed as if all operators were
   left-associative with the same precedence. This operator tree is later re-associated. *)
expr_op_tree:
  | left = with_loc(expr_op_term); op = with_loc(operator); right = with_loc(expr_op_term)
    { Btree.Node (
        Node.map op ~f:Value_name.Relative.of_ustrings_unchecked,
        Leaf left,
        Leaf right) }
  | left = expr_op_tree; op = with_loc(operator); right = with_loc(expr_op_term)
    { Btree.Node (
        Node.map op ~f:Value_name.Relative.of_ustrings_unchecked,
        left,
        Leaf right) }

match_branch:
  | branch = separated_pair(with_loc(pattern), ARROW, expr) { branch }
  | left = pattern; PIPE; right = match_branch
    { let right, expr = right in 
      let right, right_span = Node.with_value right ~f:Fn.id, Node.span right in
      let span = Span.combine (Span.of_loc $loc(left)) right_span in
      Node.create (Pattern.union left right) span, expr }

match_branches:
  | PIPE; branches = separated_nonempty(PIPE, match_branch) { branches }

effect_pattern_:
  | LESS_THAN; operation = qualified(LOWER_NAME); args = nonempty(with_loc(pattern));
    GREATER_THAN
    { let operation =  Value_name.Relative.of_ustrings_unchecked operation in
      `Effect { Effect_pattern.operation; args } }
  | pattern = pattern { `Value pattern }

%inline effect_pattern: ep = with_loc(effect_pattern_) { ep }

%inline handle_branch:
  | branch = separated_pair(effect_pattern, ARROW, expr) { branch }

%inline handle_branches:
  | PIPE; branches = separated_nonempty(PIPE, handle_branch) { branches }

let_rec:
  | LET { true }
  | LET_NONREC { false }

let_binding:
  | fixity = fixity?; pat = with_loc(pattern); EQUALS; expr = expr { pat, fixity, expr }
  | fixity = fixity?; fun_name = with_loc(pattern_name); 
    args = nonempty(with_loc(pattern_term)); EQUALS; body = expr
    { Node.map fun_name ~f:Pattern.catch_all,
      fixity,
      Node.create (Expr.Lambda (args, body)) (Span.of_loc ($startpos(args), $endpos(body))) }

expr_:
  | op_tree = expr_op_tree { Expr.Op_tree op_tree }
  | e = expr_op_term { e }
  | BACKSLASH; args = nonempty(with_loc(pattern_term)); ARROW; body = expr
    { Expr.Lambda (args, body) }
  | IF; cond = expr; THEN; e1 = expr; ELSE; e2 = expr { Expr.If (cond, e1, e2) }
  (* TODO: Allow the OCaml-like `match ... with` syntax to reduce surprise. *)
  | MATCH; e = expr; branches = match_branches { Expr.Match (e, branches) }
  | MATCH; branches = match_branches { Expr.Match_function branches }
  | HANDLE; e = expr; branches = handle_branches { Expr.Handle (e, branches) }
  | rec_ = let_rec; bindings = separated_nonempty(AND, let_binding); IN; body = expr
    { Expr.Let { rec_; bindings; body } }
  | annot = type_annot_constrained(expr) { Expr.Type_annotation (fst annot, snd annot) }

%inline expr: e = with_loc(expr_) { e }

type_record:
  (* TODO Support function types directly as record fields *)
  | fields = braces(flexible_nonempty(COMMA, type_annot_non_fun(LOWER_NAME)))
    { Type_decl.Record (Nonempty.map fields ~f:(fun (name, typ) ->
        Value_name.of_ustring_unchecked name, typ)) }

type_tuple_or_fun:
  | t = tuple(type_non_fun) { single_or_list Type_scheme.tuple t }
  | t = parens(type_fun) { t }

type_term:
  | t = type_tuple_or_fun { t }
  | cnstr = qualified(UPPER_NAME)
    { Type_scheme.Type_app (Type_name.Relative.of_ustrings_unchecked cnstr, []) }
  | param = LOWER_NAME { Type_scheme.Var (Type_param_name.of_ustring_unchecked param) }
  (* Require parentheses around unions/intersections besides otherwise, variant type
     arguments or type annotations on match expressions would always require parenthese
     to avoid confusion between union types and the pipe separator. *)
  | types = parens(separated_at_least_two(PIPE, type_non_fun))
    { Type_scheme.Union types }
  | types = parens(separated_at_least_two(AMPERSAND, type_non_fun))
    { Type_scheme.Intersection types }

type_non_fun:
  | t = type_term { t }
  | cnstr = qualified(UPPER_NAME); args = nonempty(type_term)
    { Type_scheme.Type_app (
        Type_name.Relative.of_ustrings_unchecked cnstr, Nonempty.to_list args) }

(* Writing this out like this instead of just using 
   `separated_nonempty(COMMA, type_non_fun)` prevents conflicts for some reason. *)
type_fun_args:
  | arg = type_non_fun { Nonempty.singleton arg }
  | arg = type_non_fun; COMMA; args = type_fun_args { Nonempty.cons arg args }

type_effect:
  | param = LOWER_NAME
    { Type_scheme.Effect_var (Type_param_name.of_ustring_unchecked param) }
  | cnstr = qualified(UPPER_NAME); args = list(type_term)
    { Type_scheme.Effect (Effect_name.Relative.of_ustrings_unchecked cnstr, args) }

%inline type_effects:
  | LESS_THAN; effects = separated_list(COMMA, type_effect); GREATER_THAN
    { Type_scheme.effect_union_list effects }

%inline type_fun:
  | args = type_fun_args; ARROW; effects = type_effects?; body = type_non_fun
    { Type_scheme.Function
        (args, Option.value effects ~default:(Type_scheme.effect_union []), body) }

type_expr:
  | t = type_non_fun { t }
  | t = type_fun { t }

(* TODO: Parse constraints using "where". Use these for trait bounds too. *)
%inline type_expr_constrained:
  | type_ = type_expr { type_, [] }

type_cnstr_decl:
  | cnstr = UPPER_NAME; args = list(type_term)
    { Cnstr_name.of_ustring_unchecked cnstr, args }

type_decl:
  | expr = type_expr { Type_decl.Alias expr }
  | PIPE; variants = separated_list(PIPE, type_cnstr_decl) { Type_decl.Variants variants }
  | record = type_record { record }

%inline type_params:
  | params = list(LOWER_NAME) { List.map ~f:Type_param_name.of_ustring_unchecked params }

%inline type_params_nonempty:
  | params = nonempty(LOWER_NAME)
    { Nonempty.map ~f:Type_param_name.of_ustring_unchecked params }

val_name:
  | name = LOWER_NAME { name }
  | op = parens(val_operator) { op }

fixity:
  | INFIX; n = INT { Fixity.of_decl_exn Non_assoc n }
  | INFIXL; n = INT { Fixity.of_decl_exn Left n }
  | INFIXR; n = INT { Fixity.of_decl_exn Right n }

n_periods:
  | { 0 }
  | PERIOD { 1 }
  | n = N_PERIODS { n }

%inline unidentified_name:
  | name = either(val_name, UPPER_NAME) { Unidentified_name.of_ustring name }

import_paths_after_module:
  | paths = import_paths { [ paths ] }
  | paths = parens(separated_nonempty(COMMA, import_paths)) { paths }

import_paths:
  | module_name = UPPER_NAME; PERIOD; paths = import_paths_after_module
    { Module.Import.Paths.Module (Module_name.of_ustring_unchecked module_name, paths) }
  | name = unidentified_name { Name name }
  | UNDERSCORE { All }
  | name = unidentified_name; AS; as_name = unidentified_name { Name_as (name, as_name) }
  | name = unidentified_name; AS; UNDERSCORE { Name_excluded name }

import_stmt:
  | IMPORT; n_periods = n_periods; paths = import_paths
    { { Module.Import.kind = Module.Import.Kind.of_n_periods n_periods ; paths } }

stmt_common:
  | EXTERN; fix = fixity?; name = val_name; colon; t = type_expr_constrained;
    EQUALS; s = STRING
    { Module.Extern (
        Value_name.of_ustring_unchecked name, fix, t, Extern_name.of_ustring s) }
  | TYPE; name = UPPER_NAME; params = type_params; decl = preceded(EQUALS, type_decl)?
    { Module.Type_decl (
        Type_name.of_ustring_unchecked name,
        (Type_decl.params_of_list params, Option.value decl ~default:Abstract)) }
  | EFFECT; name = UPPER_NAME; params = type_params;
    sigs = option(preceded(EQUALS, braces(list(stmt_sig))))
    { Module.Effect (
        Effect_name.of_ustring_unchecked name,
        Untyped.create_effect (Type_decl.params_of_list params) sigs) }
  | import = import_stmt { Module.Import import }

stmt_sig_:
  | s = stmt_common { Module.Common_sig s }
  | VAL; fix = fixity?; name = val_name; colon; t = type_expr_constrained
    { Module.Val (Value_name.of_ustring_unchecked name, fix, t) }
  (* TODO: support trait bounds (inheritance) on traits *)
  | TRAIT; name = UPPER_NAME; params = type_params_nonempty; sigs = colon_sigs 
    { Module.Trait_sig (Trait_name.of_ustring_unchecked name, params, sigs) }
  | MODULE; name = UPPER_NAME; colon; stmts = braces(list(stmt_sig))
    { Module.Module_sig (Module_name.of_ustring_unchecked name, stmts) }

%inline stmt_sig: s = with_loc(stmt_sig_) { s }

%inline colon_sigs: colon; sigs = braces(list(stmt_sig)) { sigs }
%inline equals_defs: EQUALS; defs = braces(list(stmt)) { defs }

stmt_:
  | s = stmt_common { Module.Common_def s }
  | TRAIT; name = UPPER_NAME; params = type_params_nonempty; sigs = colon_sigs;
    defs = loption(equals_defs)
    { Module.Trait (Trait_name.of_ustring_unchecked name, params, sigs, defs) }
  | LET; bindings = separated_nonempty(AND, let_binding)
    { Module.Let bindings }
  | MODULE; name = UPPER_NAME; sigs = loption(colon_sigs); defs = equals_defs 
    { Module.Module (Module_name.of_ustring_unchecked name, sigs, defs) }
  | IMPL; trait = UPPER_NAME; args = nonempty(type_term);
    defs = equals_defs
    { Module.Impl ([], Trait_name.of_ustring_unchecked trait, args, defs) }

%inline stmt: s = with_loc(stmt_) { s }

%inline file_module_sig:
  | sig_ = loption(preceded(FILE_MODULE, braces(list(stmt_sig)))) { sig_ }

prog:
  | def1 = list(stmt); sig_ = file_module_sig; def2 = list(stmt); EOF
    { Module_name.empty, sig_, def1 @ def2 }

%inline with_loc(X): value = X { Node.create value (Span.of_loc $loc) }

%inline type_annot(X): a = separated_pair(X, colon, type_expr) { a }
%inline type_annot_non_fun(X): a = separated_pair(X, colon, type_non_fun) { a }
%inline type_annot_constrained(X):
  | a = separated_pair(X, colon, with_loc(type_expr_constrained)) { a }

%inline record_field_EQUALS(X):
  | field = LOWER_NAME; x = preceded(EQUALS, X)?
    { (Value_name.of_ustring_unchecked field, x) }

%inline record_literal_fields(X):
  | fields = flexible_nonempty(COMMA, record_field_EQUALS(X)) { fields }

qualified(X):
  | name = UPPER_NAME; PERIOD; rest = qualified(X)
    { name :: fst rest, snd rest }
  | x = X { [], x }

%inline tuple(X): items = parens(flexible_list(COMMA, X)) { items }

%inline parens(X): x = delimited(L_PAREN, X, R_PAREN) { x }
%inline brackets(X): x = delimited(L_BRACKET, X, R_BRACKET) { x }
%inline braces(X): x = delimited(L_BRACE, X, R_BRACE) { x }

%inline separated_at_least_two(separator, X):
  | x = X; separator; xs = separated_nonempty(separator, X) { Non_single_list.(x :: xs) }

flexible_list(separator, X):
  | { [] }
  | xs = flexible_nonempty(separator, X) { Nonempty.to_list xs }

flexible_nonempty(separator, X):
  | x = X { Nonempty.singleton x }
  | x = X; separator; xs = flexible_list(separator, X) { Nonempty.(x :: xs) }

%inline nonempty(X): xs = nonempty_list(X) { Nonempty.of_list_exn xs }

%inline separated_nonempty(separator, X):
  | xs = separated_nonempty_list(separator, X) { Nonempty.of_list_exn xs }

%inline maybe_delimited(opening, X, closing):
  | x = either(X, delimited(opening, X, closing)) { x }

either(X, Y):
  | x = X { x }
  | y = Y { y }