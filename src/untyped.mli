open! Import
open Names

module Pattern : sig
  include module type of Pattern

  type nonrec t = Type.Scheme.Bounded.t t [@@deriving sexp]
end

module Expr : sig
  type t =
    | Literal of Literal.t
    | Name of Value_name.Qualified.t
    | Qualified of Module_path.t * t
    | Fun_call of t * t Nonempty.t
    | Op_tree of (Value_name.Qualified.t, t) Btree.t
    | Lambda of Pattern.t Nonempty.t * t
    | If of t * t * t
    | Match of t * (Pattern.t * t) Nonempty.t
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t list
    | Seq_literal of t list
    | Record_literal of (Value_name.t * t option) Nonempty.t
    | Record_update of t * (Value_name.t * t option) Nonempty.t
    | Record_field_access of t * Value_name.t
    | Type_annotation of t * Type.Scheme.Bounded.t
  [@@deriving sexp, variants]

  val match_function : (Pattern.t * t) Nonempty.t -> t
  val qualified : Ustring.t list * t -> t
  val op_section_left : t -> Ustring.t list * Ustring.t -> t
  val op_section_right : Ustring.t list * Ustring.t -> t -> t

  (** Get all the external names referenced by an expression. Names local to the
      expression (e.g. those bound by match expressions or lambdas) are not included. *)
  val names_used : names:Name_bindings.t -> t -> Value_name.Qualified.Set.t
end

val create_effect : Type_param_name.t list -> Module.sig_ Node.t list option -> Effect.t

module Module : sig
  include module type of Module

  type nonrec t = (Pattern.t, Expr.t) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.t) def [@@deriving sexp_of]
end
