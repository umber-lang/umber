open Import
open Names

(* TODO: add location references to the file/place each token came from *)

module Literal = struct
  type t =
    | Int of int
    | Float of float
    | Char of Uchar.t
    | String of Ustring.t
  [@@deriving sexp, variants]

  let typ = function
    | Int _ -> Core.Int.typ
    | Float _ -> Core.Float.typ
    | Char _ -> Core.Char.typ
    | String _ -> Core.String.typ
  ;;
end

module Pattern = struct
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | Cnstr_appl of Cnstr_name.Qualified.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) list
    | Union of t * t
    | Type_annotation of t * Type.Expr.Bounded.t
  [@@deriving sexp, variants]
end

module Expr = struct
  type t =
    | Literal of Literal.t
    | Name of Value_name.Qualified.t
    | Qualified of Module_path.t * t
    | Fun_call of t * t
    | Op_tree of (Value_name.Qualified.t, t) Btree.t
    | Lambda of Pattern.t * t
    | If of t * t * t
    | Match of t * (Pattern.t * t) list
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t list
    | Seq_literal of t list
    | Record_literal of (Value_name.t * t option) list
    | Record_update of t * (Value_name.t * t option) list
    | Record_field_access of t * Value_name.t
    | Type_annotation of t * Type.Expr.Bounded.t
  [@@deriving sexp_of, variants]

  let match_function branches =
    (* FIXME: this should probably create a new unused name
       Another option is the empty string gets picked up and added later,
       when we have a Name_bindings.t as context *)
    let name = Value_name.of_string_unchecked "" in
    Lambda (Pattern.Catch_all (Some name), Match (Name ([], name), branches))
  ;;

  let qualified (path, expr) =
    match path with
    | [] -> expr
    | _ -> Qualified (Module_path.of_ustrings_unchecked path, expr)
  ;;

  let op_section_right op expr =
    let op = Value_name.Qualified.of_ustrings_unchecked op in
    (* TODO: Refer uses of the empty string like to a value like Value_name.empty *)
    let left_var = Value_name.of_string_unchecked "" in
    let left_var_qualified = Value_name.Qualified.with_path [] left_var in
    Lambda
      ( Pattern.Catch_all (Some left_var)
      , Fun_call (Fun_call (Name op, Name left_var_qualified), expr) )
  ;;

  let op_section_left expr op =
    Fun_call (Name (Value_name.Qualified.of_ustrings_unchecked op), expr)
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.t) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.t) def [@@deriving sexp_of]
end
