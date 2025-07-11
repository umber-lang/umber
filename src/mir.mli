open! Import
open Names

module Block_index : sig
  type t [@@deriving sexp]

  val of_int : int -> t
  val to_int : t -> int
end

module Effect_op_id : sig
  type t

  val to_int : t -> int
end

module Expr : sig
  type t =
    | Primitive of Literal.t
    | Name of Mir_name.t
    | Let of Mir_name.t * t * t
    | Fun_call of Mir_name.t * t Nonempty.t
    | Make_block of
        { tag : Cnstr_tag.t
        ; fields : t list
        }
    | Get_block_field of Block_index.t * t
    | Cond_assign of
        { vars : Mir_name.t list
        ; conds : (cond * t list) Nonempty.t
        ; body : t
        ; if_none_matched : cond_if_none_matched
        }
    | Handle_effects of
        (* FIXME: I think vars might just have to be the vars used by the handled expr,
           not including the branches. Make that clear. *)
        
        { vars : (Mir_name.t * Mir_name.t) list
        ; value_handler : (Mir_name.t * t) option
        ; effect_handlers : effect_handler Nonempty.t
        ; expr : t
        }
    | Perform_effect of
        { effect_op : Effect_op_id.t
        ; args : t Nonempty.t
        }

  and effect_handler =
    { effect_op : Effect_op_id.t
    ; args : Mir_name.t Nonempty.t
    ; resume : Mir_name.t
    ; handler : t
    }

  and cond =
    | Equals of t * Literal.t
    | Constant_tag_equals of t * Cnstr_tag.t
    | Non_constant_tag_equals of t * Cnstr_tag.t
    | And of cond * cond

  and cond_if_none_matched =
    | Otherwise of t
    | Use_bindings of t list
  [@@deriving sexp_of]

  module Fun_def : sig
    type nonrec t =
      { fun_name : Mir_name.t
      ; args : Mir_name.t Nonempty.t
      ; body : t
      }
    [@@deriving sexp_of]
  end
end

module Fun_decl : sig
  type t =
    { name : Mir_name.t
    ; arity : int
    }
  [@@deriving sexp_of]
end

module Extern_decl : sig
  type t =
    { name : Mir_name.t
    ; extern_name : Extern_name.t
    ; arity : int
    }
  [@@deriving sexp_of]
end

module Stmt : sig
  type t =
    | Value_def of Mir_name.t * Expr.t
    | Fun_def of Expr.Fun_def.t
    | Fun_decl of Fun_decl.t
    | Extern_decl of Extern_decl.t
  [@@deriving sexp_of, variants]
end

type t = Stmt.t list [@@deriving sexp_of]

val of_typed_module
  :  names:Name_bindings.t
  -> Typed.Module.t
  -> (t, Compilation_error.t) result
