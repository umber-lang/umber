open Import
open Names

module Pattern : sig
  type t =
    | Constant of Untyped.Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr_name.Qualified.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) list
    | Union of t * t
  [@@deriving sexp]

  val of_untyped_with_names
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Untyped.Pattern.Names.t * (t * Type.t)

  val of_untyped_into
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Pattern.t
    -> Name_bindings.t * (Untyped.Pattern.Names.t * (t * Type.t))
end

module Expr : sig
  type 'typ t =
    | Literal of Untyped.Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of 'typ t * 'typ t
    | Lambda of Pattern.t * 'typ t
    | Match of 'typ t * (Pattern.t * 'typ t) list
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t list
    | Record_literal of (Value_name.t * 'typ t option) list
    | Record_update of 'typ t * (Value_name.t * 'typ t option) list
    | Record_field_access of 'typ t * Value_name.t
  [@@deriving sexp]

  val of_untyped
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Expr.t
    -> (Type.t * Untyped.Pattern.Names.t) t * Type.t
end

module Module : sig
  include module type of Module

  type nonrec t = (Pattern.t, Type.Scheme.t Expr.t * Type.Scheme.t) t [@@deriving sexp]

  type nonrec def = (Pattern.t, Type.Scheme.t Expr.t * Type.Scheme.t) def
  [@@deriving sexp]

  val of_untyped
    :  ?backtrace:bool
    -> ?names:Name_bindings.t
    -> ?types:Type_bindings.t
    -> Untyped.Module.t
    -> (Name_bindings.t * t, Ustring.t) Result.t

  (* TODO: should probably keep all these functions internal and move all this to the ml *)

  (** Gather placeholders for all declared names and types.
      (Needed for imports of submodules to work.) *)
  val gather_name_placeholders
    :  names:Name_bindings.t
    -> Untyped.Module.sig_ Node.t list
    -> Untyped.Module.def Node.t list
    -> Name_bindings.t

  (** Gather all imported names and local type/trait declarations. *)
  val gather_imports_and_type_decls
    :  names:Name_bindings.t
    -> Untyped.Module.sig_ Node.t list
    -> Untyped.Module.def Node.t list
    -> Name_bindings.t

  type intermediate_def =
    ((Pattern.t * Type.t) * Untyped.Pattern.Names.t, Untyped.Expr.t) Module.def
  [@@deriving sexp]

  (** Handle all `val` and `let` statements (value bindings/type annotations).
      Also type the patterns in each let binding and assign the names fresh type
      variables. *)
  val handle_value_bindings
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> Untyped.Module.sig_ Node.t list
    -> Untyped.Module.def Node.t list
    -> Name_bindings.t * intermediate_def Node.t list

  (** Type-check the expressions (definitions) for each let binding and trait implementation.
      Performs let-generalization on free variables in let statement bindings.
      (Let-generalization is not currently done for local let bindings within expressions.)
      This is the final step in the type checking process. *)
  val type_defs
    :  names:Name_bindings.t
    -> types:Type_bindings.t
    -> intermediate_def Node.t list
    -> Name_bindings.t * def Node.t list
end
