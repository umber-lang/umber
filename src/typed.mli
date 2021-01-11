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
  type 'typ expr =
    | Literal of Untyped.Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of 'typ expr * 'typ expr
    | Lambda of Pattern.t * 'typ expr
    | Match of 'typ expr * (Pattern.t * 'typ expr) list
    | Let of (Pattern.t * 'typ, 'typ expr) Let_binding.t
    | Tuple of 'typ expr list
    | Record_literal of (Value_name.t * 'typ expr option) list
    | Record_update of 'typ expr * (Value_name.t * 'typ expr option) list
    | Record_field_access of 'typ expr * Value_name.t
  [@@deriving sexp]

  type t = Type.Scheme.t expr [@@deriving sexp]

  val of_untyped
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Expr.t
    -> Type.t expr * Type.t

  module Op_tree : sig
    (** Re-associate the operator tree through tree rotations.
        These constraints hold on the finished tree:
        1. Every node has precedence less than or equal to both its childrens' precedence.
        2. When a node has precedence equal to that some of its children,
           it and those children all share the same associativity:
          a. In the case of left associativity, rotating anticlockwise results in a tree
             which violates constraint 1.
          b. In the case of right associativity, rotating clockwise results in a tree
             which violates constraint 1.
          c. The case of no associativity is not allowed. *)
    val to_untyped_expr
      :  names:Name_bindings.t
      -> (Value_name.Qualified.t, Untyped.Expr.t) Btree.t
      -> Untyped.Expr.t
  end
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
