(* See https://github.com/grin-compiler/grin#grin-ir for the GRIN IR specification *)
(*
type prog = binding list

and binding =
  { fun_name : var
  ; fun_args : var list
  ; body : exp
  }

and exp =
  | Sequence of sexp * (lpat * exp)
  | Case of val_ * (cpat * exp) list
  | Operation of sexp

and sexp =
  | Application of var * sval list
  | Unit of val_
  | Store of val_
  | Fetch of var * int option
  | Update of var * val_
  | Exp_parens of exp

and val_ =
  | Constant_tag of tag * sval list
  | Variable_tag of var * sval list
  | Single_tag of tag
  | Empty
  | Sval of sval

and sval =
  | Literal of literal
  | Variable of var

and lpat = val_

and cpat =
  | Constant_node_pattern of tag * var list
  | Tag_pattern of tag
  | Literal_pattern of literal

and var = private string

and tag_type =
  | C_tag
  | F_tag
  | P_tag

and tag_name = private string

and tag = tag_type * tag_name

and literal =
  | Int of int
  | Char of char*)

module Literal : sig
  (* TODO: check if they support more literal types
     Also check sizes for int, char representation, etc.
     What about floats, strings? *)
  type t =
    | Int of int
    | Char of char
end

module Tag : sig
  type kind =
    | C_tag
    | F_tag
    | P_tag

  type name = private string
  type t = kind * name
end

module Var : sig
  type t = private string
end

module Pat : sig
  type t =
    | Constant_node_pattern of Tag.t * Var.t list
    | Tag_pattern of Tag.t
    | Literal_pattern of Literal.t
end

module Val : sig
  type t =
    | Constant_tag of Tag.t * simple list
    | Variable_tag of Var.t * simple list
    | Single_tag of Tag.t
    | Empty
    | Simple of simple

  and simple =
    | Literal of Literal.t
    | Variable of Var.t
end

module Exp : sig
  type t =
    | Sequence of simple * (Val.t * t)
    | Case of Val.t * (Pat.t * t) Nonempty_list.t
    | Operation of simple

  and simple =
    | Application of Var.t * Val.simple Nonempty_list.t
    | Unit of Val.t
    | Store of Val.t
    | Fetch of Var.t * int option
    | Update of Var.t * Val.t
    | Exp_parens of t
end

module Binding : sig
  type t =
    { fun_name : Var.t
    ; fun_args : Var.t Nonempty_list.t
    ; body : Exp.t
    }
end

module Prog : sig
  type t = Binding.t Nonempty_list.t
end
