open Import
open Names

module Pattern : sig
  type t =
    | Constant of Untyped.Literal.t
    | Catch_all of Value_name.t option
    | Cnstr_appl of Cnstr_name.Qualified.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) list
    | Union of t * t
  [@@deriving sexp]

  val of_untyped_with_names
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Name_bindings.Name_entry.t Value_name.Map.t * (t * Type.t)

  val of_untyped_into
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Name_bindings.t * (t * Type.t)
end

module Expr : sig
  type t =
    | Literal of Untyped.Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of t * t
    | Lambda of Pattern.t * t
    | Match of t * (Pattern.t * t) list
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t list
    | Record_literal of (Value_name.t * t option) list
    | Record_update of t * (Value_name.t * t option) list
    | Record_field_access of t * Value_name.t
  [@@deriving sexp]

  val of_untyped
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Expr.t
    -> t * Type.t
end

module Module : sig
  include module type of Module

  type nonrec t = (Pattern.t, Expr.t * Type.Scheme.t) t [@@deriving sexp]
  type nonrec def = (Pattern.t, Expr.t * Type.Scheme.t) def [@@deriving sexp]

  val of_untyped
    :  ?backtrace:bool
    -> ?names:Name_bindings.t
    -> ?types:Type_bindings.t
    -> Untyped.Module.t
    -> (Name_bindings.t * t, Ustring.t) Result.t
end
