(* See https://github.com/grin-compiler/grin#grin-ir for the GRIN IR specification *)
open Base

module Literal = struct
  type t =
    | Int of int
    | Char of char
end

module Tag = struct
  type kind =
    | C_tag
    | F_tag
    | P_tag

  type name = private string
  type t = kind * name
end

module Var = struct
  type t = private string
end

module Pat = struct
  type t =
    | Constant_node_pattern of Tag.t * Var.t list
    | Tag_pattern of Tag.t
    | Literal_pattern of Literal.t
end

module Val = struct
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

module Exp = struct
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

module Binding = struct
  type t =
    { fun_name : Var.t
    ; fun_args : Var.t Nonempty_list.t
    ; body : Exp.t
    }
end

module Prog = struct
  type t = Binding.t Nonempty_list.t

  (* TODO: to_bigstring, or something like that: write to an existing bigstring?
     What did I do for Ocaml Pickle? *)
end
