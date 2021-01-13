%{
  open Import
  open Untyped
  open Ast

  (* Disable deprecation warnings caused by Menhir using Not_found when inferring types *)
  [@@@ocaml.warning "-3"]
%}

%token IF
%token THEN
%token ELSE
%token LET
%token LET_NONREC
%token MATCH
%token WITH
%token WITHOUT
%token TYPE
%token ALIAS
%token VAL
%token INFIX
%token INFIXL
%token INFIXR
%token MODULE
%token FILE_MODULE
%token TRAIT
%token IMPL
%token IMPORT

%token EQUALS
%token EQUALS_ONLY_LINE
%token PIPE
%token COLON
%token COLON_SPACED
%token COMMA
%token BACKSLASH
%token ASTERISK
%token PERIOD
%token ARROW
%token FAT_ARROW
%token L_PAREN
%token R_PAREN
%token L_BRACKET
%token R_BRACKET
%token L_BRACE
%token R_BRACE

%token <int> INT
%token <float> FLOAT
%token <Uchar.t> CHAR
%token <Ustring.t> STRING

%token UNDERSCORE
(* TODO: prevent underscore being used as a variable with a name_error, and create patterns
   using a new catch_all creator. Want to be able to parse it as a LOWER_NAME to use as 
   the previous evaluated thing in the interpreter (as seen in Python, or similar to
   `it` in ghci)
   Another option: could just use `it` for the previous purpose and use `_` for gaps (partial application) *)
%token <Ustring.t> LOWER_NAME
%token <Ustring.t> UPPER_NAME
%token <Ustring.t> OPERATOR

%token INDENT
%token DEDENT
%token LINE_SEP
%token EOF

(* TODO: Make sure all desirable forms of indentation are supported. 
   e.g. some from http://people.csail.mit.edu/mikelin/ocaml+twt/quick_reference.pdf
   
   Actually, what if we do more of the haskell thing and just allow INDENTs
   at basically any point in expressions?
   - Could have an error handler that just moves on if in an expression context
     and erroring on an INDENT (yeah nah)
   - exprs can then end in any number of DEDENTs (or is that going to cause ambiguity?)
   
   Here's an option: enforce a specific kind of indentation (even in tuples, etc.)
   and just use that for now -- once this is self-hosting, I'll re-write the parser by hand
   
   can look into indentation-sensitive parsing:
     https://michaeldadams.org/papers/layout_parsing/LayoutParsing.pdf 
     - probably relevant for the parser re-write which will happen once we are self-hosting
     - for now, let's just get something basic working (doesn't need to support every case) *)

%start <Untyped.Module.t> prog
%%

(* TODO: Idea: Apply a handler to all statements in the module.
  effect UPPER_NAME with VALUE_NAME
  e.g.
  [effect Random with System.random]
  
  This would be a good place to use a [handle] keyword.
  Could also do [by] instead of [with].
  
  I like having handle be the same as match but with a default branch that says
  | x -> x (so it's like try/except basically) *)

val_operator:
  | op = OPERATOR { op }
  | ASTERISK { Ustring.of_string_exn "*" }

operator:
  | COLON; name = qualified(val_name); colon { name }
  | COLON; name = qualified(UPPER_NAME); colon { name }
  | op = val_operator { [], op }

%inline equals: either(EQUALS, EQUALS_ONLY_LINE) { }
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
    { Pattern.Cnstr_appl (Cnstr_name.Qualified.of_ustrings_unchecked name, []) }
  | fields = braces(record_literal_fields(pattern)) { Pattern.Record fields }

pattern:
  | p = pattern_term { p }
  | cnstr = qualified(UPPER_NAME); args = nonempty_list(pattern_term)
    { Pattern.Cnstr_appl (Cnstr_name.Qualified.of_ustrings_unchecked cnstr, args) }
  | left = pattern; PIPE; right = pattern { Pattern.Union (left, right) }
  | annot = type_annot_bounded(pattern)
    { Pattern.Type_annotation (fst annot, snd annot) }

op_section:
  | op = operator; e = expr_op_term { Expr.op_section_right op e }
  | e = expr_op_term; op = operator { Expr.op_section_left e op }

expr_term:
  | e = qualified(tuple(expr))
    { Expr.qualified (fst e, single_or_list Expr.tuple (snd e)) }
  | op = qualified(parens(operator))
    { let path1, (path2, op) = op in
      Expr.Name (Value_name.Qualified.of_ustrings_unchecked (path1 @ path2, op)) }
  | op_section = qualified(parens(op_section)) { Expr.qualified op_section }
  | name = qualified(either(LOWER_NAME, UPPER_NAME))
    { Expr.Name (Value_name.Qualified.of_ustrings_unchecked name) }
  | l = literal { Expr.Literal l }
  | items = brackets(block_items(expr)) { Expr.Seq_literal items }
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
  | record = expr_term; PERIOD; field = LOWER_NAME
    { Expr.Record_field_access (record, Value_name.of_ustring_unchecked field) }

expr_fun_call:
  | f = expr_term; arg = expr_term { Expr.Fun_call (f, arg) }
  | f = expr_fun_call; arg = expr_term { Expr.Fun_call (f, arg) }

expr_op_term:
  | e = expr_fun_call { e }
  | e = expr_term { e }

%inline match_branches:
  | branches = pipe_branches(separated_pair(pattern, ARROW, expr)) { branches }

(* Expressions with binary operators are first parsed as if all operators were
   left-associative with the same precedence. This operator tree is later re-associated. *)
expr_op_tree:
  | left = expr_op_term; op = operator; right = expr_op_term
    { Btree.Node (Value_name.Qualified.of_ustrings_unchecked op, Leaf left, Leaf right) }
  | left = expr_op_tree; op = operator; right = expr_op_term
    { Btree.Node (Value_name.Qualified.of_ustrings_unchecked op, left, Leaf right) }

let_rec:
  | LET { true }
  | LET_NONREC { false }

let_binding:
  | pat = pattern; equals; expr = expr { pat, expr }
  | fun_name = pattern_name; args = nonempty_list(pattern_term); equals; expr = expr
    { Pattern.Catch_all fun_name, List.fold_right args ~init:expr ~f:Expr.lambda }

expr:
  | e = delimited(INDENT, expr, DEDENT) { e }
  (* TODO: ^ need better handling of stuff like this
     What it should really be is that you just have to have the same number of INDENTs &
     DEDENTs by the time you finish the expression - they shouldn't have to perfectly
     wrap it *)
  | op_tree = expr_op_tree { Expr.Op_tree op_tree }
  | e = expr_op_term { e }
  | BACKSLASH; args = nonempty_list(pattern_term); ARROW; body = expr
    { List.fold_right args ~init:body ~f:Expr.lambda }
  | IF; cond = expr; THEN; e1 = expr; ELSE; e2 = expr { Expr.If (cond, e1, e2) }
  | MATCH; e = expr; LINE_SEP?; branches = match_branches { Expr.Match (e, branches) }
  | MATCH; branches = match_branches { Expr.match_function branches }
  | rec_ = let_rec; binding = let_binding; LINE_SEP?; body = expr
    { Expr.Let { rec_; bindings = [binding]; body } }
  | rec_ = let_rec; INDENT; bindings = separated_nonempty_list(LINE_SEP, let_binding);
    DEDENT; body = expr
    { Expr.Let { rec_; bindings; body } }
  | annot = type_annot_bounded(expr) { Expr.Type_annotation (fst annot, snd annot) }

type_record:
  | fields = braces(block_items_nonempty(type_annot(LOWER_NAME)))
    { Type.Decl.Record (List.map fields ~f:(fun (name, typ) ->
        Value_name.of_ustring_unchecked name, typ)) }

type_term:
  | e = tuple(type_expr) { single_or_list Type.Expr.tuple e }
  | cnstr = qualified(UPPER_NAME)
    { Type.Expr.Type_app (Type_name.Qualified.of_ustrings_unchecked cnstr, []) }
  | param = LOWER_NAME { Type.Expr.Var (Type_param_name.of_ustring_unchecked param) }

type_non_func:
  | t = type_term { t }
  | cnstr = qualified(UPPER_NAME); args = nonempty_list(type_term)
    { Type.Expr.Type_app (Type_name.Qualified.of_ustrings_unchecked cnstr, args) }

type_expr:
  | t = type_non_func { t }
  | arg = type_non_func; ARROW; body = type_expr
    { Type.Expr.Function (arg, body) }

%inline trait_bound:
  | bounds = parens(block_items(pair(UPPER_NAME, nonempty_list(LOWER_NAME))));
    FAT_ARROW
    { List.map bounds ~f:(fun (trait, params) ->
        Trait_name.of_ustring_unchecked trait,
        List.map params ~f:Type_param_name.of_ustring_unchecked) }

%inline type_expr_bounded:
  | bound = loption(trait_bound); t = type_expr { (bound, t) }

type_cnstr_decl:
  | cnstr = UPPER_NAME; args = list(type_term)
    { Cnstr_name.of_ustring_unchecked cnstr, args }

type_decl:
  | PIPE { Type.Decl.Variants [] }
  | variants = pipe_branches(type_cnstr_decl) { Type.Decl.Variants variants }
  | r = block(type_record) { r }

%inline type_param_list:
  | params = list(LOWER_NAME) { List.map ~f:Type_param_name.of_ustring_unchecked params }

val_name:
  | name = LOWER_NAME { name }
  | op = parens(val_operator) { op }

fixity:
  | INFIX; n = INT { Fixity.(of_decl_exn Non_assoc n) }
  | INFIXL; n = INT { Fixity.(of_decl_exn Left n) }
  | INFIXR; n = INT { Fixity.(of_decl_exn Right n) }

%inline import_module_path:
  | IMPORT; path = separated_nonempty_list(PERIOD, UPPER_NAME)
    { Module_path.of_ustrings_unchecked path }

%inline import_item:
  | name = either(val_name, UPPER_NAME) { Unidentified_name.of_ustring name }

import_stmt:
  | IMPORT; name = UPPER_NAME { Module.Import (Module_name.of_ustring_unchecked name) }
  | path = import_module_path; WITH; ASTERISK { Module.Import_with (path, []) }
  | path = import_module_path; WITH; items = nonempty_list(import_item)
    { Module.Import_with (path, items) }
  | path = import_module_path; WITHOUT; items = nonempty_list(import_item)
    { Module.Import_without (path, items) }

stmt_common:
  | VAL; name = val_name; fix = parens(fixity)?; colon;
    t = block(type_expr_bounded)
    { Module.Val (Value_name.of_ustring_unchecked name, fix, t) }
  | TYPE; name = UPPER_NAME; params = type_param_list; decl = preceded(equals, type_decl)?
    { Module.Type_decl (
        Type_name.of_ustring_unchecked name,
        (params, Option.value decl ~default:Abstract)) }
  | TYPE; ALIAS; name = UPPER_NAME; params = type_param_list; equals; e = type_expr
    { Module.Type_decl (
        Type_name.of_ustring_unchecked name, (params, Type.Decl.Alias e)) }
  | TRAIT; name = UPPER_NAME; params = type_param_list; colon;
    sig_ = block(nonempty_lines(stmt_sig))
    { Module.Trait_sig (Trait_name.of_ustring_unchecked name, params, sig_) }
  | import = import_stmt { import }

stmt_sig_:
  | s = stmt_common { Module.Common_sig s }
  | MODULE; name = UPPER_NAME; colon; stmts = block(lines(stmt_sig))
    { Module.Module_sig (Module_name.of_ustring_unchecked name, stmts) }

%inline stmt_sig: s = with_loc(stmt_sig_) { s }

%inline def:
  (* TODO: try removing EQUALS_ONLY_LINE: allow sig to end in LINE_SEP? *)
  | equals; def = block(nonempty_lines(stmt)) { def }

optional_sig_def:
  | colon; body = block(pair(lines(stmt_sig), def)) { body }
  | def = def { [], def }

(*function_def(X):
  | INDENT; args = flexible_nonempty_list(LINE_SEP, X); equals; body = expr; DEDENT
    { args, body }
  | args = nonempty_list(X); equals; body = expr { args, body }*)

stmt_:
  | s = stmt_common { Module.Common_def s }
  | LET; binding = let_binding { Module.Let [binding] }
  | LET; INDENT; bindings = separated_nonempty_list(LINE_SEP, let_binding); DEDENT
    { Module.Let bindings }
  | MODULE; name = UPPER_NAME; body = optional_sig_def
    { Module.Module (Module_name.of_ustring_unchecked name, fst body, snd body) }
  | TRAIT; name = UPPER_NAME; params = type_param_list; body = optional_sig_def
    { Module.Trait (Trait_name.of_ustring_unchecked name, params, fst body, snd body) }
  (* TODO: support multiple type parameters in trait impls
     Should probably change `type_expr` to `nonempty_list(type_term)` *)
  | IMPL; bound = loption(trait_bound); trait = UPPER_NAME; typ = type_expr;
    def = def
    { Module.Impl (bound, Trait_name.of_ustring_unchecked trait, typ, def) }

%inline stmt: s = with_loc(stmt_) { s }

%inline file_module_sig:
  | sig_ = loption(preceded(FILE_MODULE, block(lines(stmt_sig)))) { sig_ }

prog:
  | def1 = flexible_optional_list(LINE_SEP, stmt); sig_ = file_module_sig;
    def2 = lines(stmt); EOF
    { Module_name.of_string_unchecked "", sig_, def1 @ def2 }

%inline with_loc(X): node = X { { Node.node ; span = Span.of_loc $loc } }

%inline lines(X): x = separated_list(LINE_SEP?, X) { x }
%inline nonempty_lines(X): x = separated_nonempty_list(LINE_SEP?, X) { x }

%inline type_annot(X): a = separated_pair(X, colon, type_expr) { a }
%inline type_annot_bounded(X):
  | a = separated_pair(X, colon, type_expr_bounded) { a }

%inline line_sep_pipe: LINE_SEP?; PIPE { }

pipe_branches(X):
  | PIPE?; branches = separated_nonempty_list(PIPE, X) { branches }
  | INDENT; PIPE?; branches = separated_nonempty_list(line_sep_pipe, X); DEDENT
    { branches }

%inline record_field_equals(X):
  | field = LOWER_NAME; x = preceded(EQUALS, X)?
    { (Value_name.of_ustring_unchecked field, x) }

%inline record_literal_fields(X):
  | fields = block_items_nonempty(record_field_equals(X)) { fields }

qualified(X):
  | name = UPPER_NAME; PERIOD; rest = qualified(X)
    { name :: fst rest, snd rest }
  | x = X { [], x }

%inline tuple(X): items = parens(block_items(X)) { items }

%inline parens(X): x = delimited(L_PAREN, X, R_PAREN) { x }
%inline brackets(X): x = delimited(L_BRACKET, X, R_BRACKET) { x }
%inline braces(X): x = delimited(L_BRACE, X, R_BRACE) { x }

%inline block(X): b = maybe_delimited(INDENT, X, DEDENT) { b }

%inline block_items(X): items = flexible_list(COMMA, X); DEDENT* { items }
%inline block_items_nonempty(X):
  | items = flexible_nonempty_list(COMMA, X); DEDENT* { items }

%inline flexible_list(separator, X):
  | xs = loption(flexible_nonempty_list(separator, X)) { xs }

flexible_nonempty_list(separator, X):
  | x = X { [x] }
  | x = X; separator; xs = flexible_list(separator, X) { x :: xs }

flexible_optional_list(separator, X):
  | separator? { [] }
  | x = X; separator?; xs = flexible_optional_list(separator, X) { x :: xs }

%inline maybe_delimited(opening, X, closing):
  | x = either(X, delimited(opening, X, closing)) { x }

either(X, Y):
  | x = X { x }
  | y = Y { y }