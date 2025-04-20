open Import
open Names

module Pattern : sig
  type 'typ t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of 'typ t * Value_name.t
    | Cnstr_appl of Cnstr_name.Absolute.t * ('typ t * 'typ) list
    | Tuple of ('typ t * 'typ) list
    | Record of (Value_name.t * 'typ t option) Nonempty.t
    | Union of 'typ t * 'typ t
  [@@deriving equal, sexp, variants]

  type generalized = Module_path.absolute Type_scheme.t t [@@deriving sexp_of]

  val fold_names : _ t -> init:'acc -> f:('acc -> Value_name.t -> 'acc) -> 'acc
end

module Effect_pattern : sig
  type 'typ t =
    { operation : Value_name.Absolute.t
    ; args : 'typ Pattern.t Nonempty.t
    }
  [@@deriving equal, sexp]
end

module Effect_branch : sig
  type 'typ t =
    { effect_pattern : 'typ Effect_pattern.t
    ; arg_types : 'typ Nonempty.t
    ; resume_type : 'typ
    }
  [@@deriving sexp]
end

module Expr : sig
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Absolute.t
    | Fun_call of 'typ t Node.t * 'typ * ('typ t Node.t * 'typ) Nonempty.t
    | Lambda of 'typ Pattern.t Node.t Nonempty.t * 'typ t Node.t
    | Match of 'typ t Node.t * 'typ * ('typ Pattern.t Node.t * 'typ t Node.t) Nonempty.t
    | Handle of
        { expr : 'typ t Node.t
        ; expr_type : 'typ
        ; value_branch : (('typ Pattern.t * 'typ) Node.t * 'typ t Node.t) option
        ; effect_branches : ('typ Effect_branch.t Node.t * 'typ t Node.t) list
        }
    | Let of ('typ Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t Node.t list
    | Record_literal of (Value_name.t * 'typ t Node.t option) list
    | Record_update of 'typ t Node.t * (Value_name.t * 'typ t Node.t option) list
    | Record_field_access of 'typ t Node.t * Value_name.t Node.t
  [@@deriving sexp]

  type generalized =
    Module_path.absolute Type_scheme.t t * Module_path.absolute Type_scheme.t
  [@@deriving sexp_of]
end

module Let_binding_group : sig
  module Index : sig
    (* TODO: It might be nice if we could sort bindings in dependency order but have that
       sort be stable according to the original source ordering - that would make MIR test
       output easier to read. As it stands we are getting the topological sort from a
       graph library though, so it's hard to make this happen. *)
    (** Specifies the order of a group of bindings relative to other bindings in the same
        file. This is a topological ordering of the bindings based on the dependencies
        between them. *)
    type t [@@deriving compare, sexp_of]
  end

  type t =
    { rec_ : bool
    ; bindings :
        (Pattern.generalized Node.t * Fixity.t option * Expr.generalized Node.t)
        Nonempty.t
    ; index : Index.t
    }
  [@@deriving sexp_of, fields]
end

module Module : sig
  include module type of Module

  type nonrec t = (Let_binding_group.t, Module_path.absolute) t [@@deriving sexp_of]
  type nonrec def = (Let_binding_group.t, Module_path.absolute) def [@@deriving sexp_of]

  val of_untyped
    :  names:Name_bindings.t
    -> include_std:bool
    -> Untyped.Module.t
    -> (Name_bindings.t * t, Compilation_error.t) Result.t
end
