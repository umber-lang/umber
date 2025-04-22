open! Import
open Names

module Pattern : sig
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr_name.Relative.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) Nonempty.t
    | Union of t * t
    | Type_annotation of t * Module_path.relative Type_scheme.t
  [@@deriving equal, sexp, variants]

  val fold_names : t -> init:'acc -> f:('acc -> Value_name.t -> 'acc) -> 'acc
end

module Effect_pattern : sig
  type t =
    { operation : Value_name.Relative.t
    ; args : Pattern.t Node.t Nonempty.t
    }
  [@@deriving equal, sexp]
end

module Expr : sig
  type t =
    | Literal of Literal.t
    | Name of Value_name.Relative.t
    | Qualified of Module_path.Relative.t * t Node.t
    | Fun_call of t Node.t * t Node.t Nonempty.t
    | Op_tree of (Value_name.Relative.t Node.t, t Node.t) Btree.t
    | Op_section of
        { op_side : [ `Left | `Right ]
        ; op : Value_name.Relative.t Node.t
        ; expr : t Node.t
        }
    | Lambda of Pattern.t Node.t Nonempty.t * t Node.t
    | If of t Node.t * t Node.t * t Node.t
    | Match of t Node.t * (Pattern.t Node.t * t Node.t) Nonempty.t
    | Match_function of (Pattern.t Node.t * t Node.t) Nonempty.t
    | Handle of
        t Node.t
        * ([ `Effect of Effect_pattern.t | `Value of Pattern.t ] Node.t * t Node.t)
          Nonempty.t
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t Node.t list
    | Seq_literal of t Node.t list
    | Record_literal of (Value_name.t * t Node.t option) Nonempty.t
    | Record_update of t Node.t * (Value_name.t * t Node.t option) Nonempty.t
    | Record_field_access of t Node.t * Value_name.t Node.t
    | Type_annotation of t Node.t * Module_path.relative Type_scheme.t Node.t
  [@@deriving equal, sexp, variants]

  (** Get all the external names referenced by an expression. Names local to the
      expression (e.g. those bound by match expressions or lambdas) are not included. *)
  val names_used : names:Name_bindings.t -> t -> Value_name.Absolute.Set.t

  val qualified : Ustring.t list -> t Node.t -> t
end

val create_effect
  :  Type_param_name.t Unique_list.t
  -> 'name Module.sig_ Node.t list option
  -> 'name Effect.t

module Let_binding_group : sig
  type t = (Pattern.t Node.t * Fixity.t option * Expr.t Node.t) Nonempty.t
  [@@deriving sexp_of]
end

module Module : sig
  include module type of Module

  type nonrec t = (Let_binding_group.t, Module_path.relative) t [@@deriving sexp_of]
  type nonrec sig_ = Module_path.relative sig_ [@@deriving sexp_of]
  type nonrec def = (Let_binding_group.t, Module_path.relative) def [@@deriving sexp_of]
end
