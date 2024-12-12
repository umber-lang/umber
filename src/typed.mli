open Import
open Names

module Pattern : sig
  (* FIXME: Add type annotations inside here *)
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr_name.Absolute.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) Nonempty.t
    | Union of t * t
  [@@deriving equal, sexp, variants]

  val fold_names : t -> init:'acc -> f:('acc -> Value_name.t -> 'acc) -> 'acc
end

module Effect_pattern : sig
  type t =
    { operation : Value_name.Absolute.t
    ; args : Pattern.t Nonempty.t
    }
  [@@deriving equal, sexp]
end

module Expr : sig
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Absolute.t
    | Fun_call of 'typ t Node.t * 'typ * ('typ t Node.t * 'typ) Nonempty.t
    | Lambda of Pattern.t Node.t Nonempty.t * 'typ t Node.t
    | Match of 'typ t Node.t * 'typ * (Pattern.t Node.t * 'typ t Node.t) Nonempty.t
    | Handle of
        { expr : 'typ t Node.t
        ; expr_type : 'typ
        ; value_branch : (Pattern.t Node.t * 'typ t Node.t) option
        ; effect_branches : (Effect_pattern.t Node.t * 'typ t Node.t) list
        }
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t Node.t list
    | Record_literal of (Value_name.t * 'typ t Node.t option) list
    | Record_update of 'typ t Node.t * (Value_name.t * 'typ t Node.t option) list
    | Record_field_access of 'typ t Node.t * Value_name.t Node.t
  [@@deriving sexp]

  type generalized =
    Module_path.absolute Type_scheme.t t * Module_path.absolute Type_scheme.t
  [@@deriving sexp_of]
end

module Module : sig
  include module type of Module

  type nonrec t = (Pattern.t, Expr.generalized, Module_path.absolute) t
  [@@deriving sexp_of]

  type nonrec def = (Pattern.t, Expr.generalized, Module_path.absolute) def
  [@@deriving sexp_of]

  val of_untyped
    :  names:Name_bindings.t
    -> include_std:bool
    -> Untyped.Module.t
    -> (Name_bindings.t * t, Compilation_error.t) Result.t
end
