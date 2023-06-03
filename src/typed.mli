open Import
open Names

module Pattern : sig
  include module type of Pattern

  type nonrec t = (Nothing.t, Module_path.absolute) t [@@deriving sexp]

  val of_untyped_with_names
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Names.t * (t * Type.t)

  val of_untyped_into
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Name_bindings.t * (Names.t * (t * Type.t))
end

module Expr : sig
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Absolute.t
    | Fun_call of 'typ t Node.t * ('typ t Node.t * 'typ) Nonempty.t
    | Lambda of Pattern.t Node.t Nonempty.t * 'typ t Node.t
    | Match of 'typ t Node.t * 'typ * (Pattern.t Node.t * 'typ t Node.t) Nonempty.t
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    (* TODO: consider replacing Tuple with some kind of built-in constructor
       e.g. _Tuple2, _Tuple3, depending on the length *)
    | Tuple of 'typ t Node.t list
    | Record_literal of (Value_name.t * 'typ t Node.t option) list
    | Record_update of 'typ t Node.t * (Value_name.t * 'typ t Node.t option) list
    | Record_field_access of 'typ t Node.t * Value_name.t Node.t
  [@@deriving sexp]

  type generalized =
    Module_path.absolute Type.Scheme.t t * Module_path.absolute Type.Scheme.t
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
    -> types:Type_bindings.t
    -> include_std:bool
    -> Untyped.Module.t
    -> (Name_bindings.t * t, Compilation_error.t) Result.t
end
