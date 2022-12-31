open! Import
open Names

module Cnstr_tag : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
  val default : t
end

module Block_index : sig
  type t [@@deriving sexp]

  val of_int : int -> t
  val to_int : t -> int
end

module Expr : sig
  type t =
    | Primitive of Literal.t
    | Name of Name_id.t
    | Let of Name_id.t * t * t
    | Fun_call of Name_id.t * t Nonempty.t
    | Make_block of
        { tag : Cnstr_tag.t
        ; fields : t list
        }
    | Get_block_field of Block_index.t * t
    | Cond_assign of
        { vars : Name_id.t list
        ; conds : (cond * t list) Nonempty.t
        ; body : t
        ; if_none_matched : cond_if_none_matched
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
      { fun_name : Name_id.t
      ; closed_over : Name_id.Set.t
      ; args : Name_id.t Nonempty.t
      ; body : t
      }
    [@@deriving sexp_of]
  end
end

module Extern_decl : sig
  type t =
    { name : Name_id.t
    ; arity : int
    }
  [@@deriving sexp]
end

module Stmt : sig
  type t =
    | Value_def of Name_id.t * Expr.t
    | Fun_def of Expr.Fun_def.t
    | Extern_decl of Extern_decl.t
  [@@deriving sexp_of, variants]
end

type t = Stmt.t list [@@deriving sexp_of]

val of_typed_module
  :  names:Name_bindings.t
  -> name_table:Name_id.Name_table.t
  -> Typed.Module.t
  -> (t, Compilation_error.t) result
