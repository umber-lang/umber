open! Import
open Names

type t [@@deriving sexp_of]

val create : unit -> t

val constrain
  :  names:Name_bindings.t
  -> types:t
  -> subtype:Type.t
  -> supertype:Type.t
  -> unit

val constrain_effects
  :  names:Name_bindings.t
  -> types:t
  -> subtype:(Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effects
  -> supertype:(Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effects
  -> unit

val constrain_effects_to_be_total
  :  names:Name_bindings.t
  -> types:t
  -> (Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effects
  -> unit

val substitute : t -> Type.t -> Type.t
val generalize : t -> Type.t -> Module_path.absolute Type.Scheme.t

(* Want a message like "this expression has type ... but an expression was
   expected with type ..."
   TODO: look to Elm, Rust, etc. for error message inspiration
   Not all errors fit this mold exactly - probably need a proper dedicated type. *)
val type_error : string -> Type.t -> Type.t -> _
