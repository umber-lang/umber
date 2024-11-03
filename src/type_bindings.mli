open! Import
open Names

type t [@@deriving sexp_of]

val create : unit -> t

(* FIXME: Instantiating schemes is used to bind vars for variables
   The other place is patterns *)

val instantiate_type_scheme
  :  ?params:Type_param.Env_to_vars.t
  -> names:Name_bindings.t
  -> types:t
  -> Module_path.absolute Type_scheme.t
  -> Internal_type.t

val instantiate_type_or_scheme
  :  ?params:Type_param.Env_to_vars.t
  -> names:Name_bindings.t
  -> types:t
  -> Name_bindings.Name_entry.Type_or_scheme.t
  -> Internal_type.t

val constrain
  :  names:Name_bindings.t
  -> types:t
  -> subtype:Internal_type.t
  -> supertype:Internal_type.t
  -> unit

val constrain'
  :  names:Name_bindings.t
  -> types:t
  -> subtype:Name_bindings.Name_entry.Type_or_scheme.t
  -> supertype:Name_bindings.Name_entry.Type_or_scheme.t
  -> unit

val constrain_effects
  :  names:Name_bindings.t
  -> types:t
  -> subtype:Internal_type.effects
  -> supertype:Internal_type.effects
  -> unit

val constrain_effects_to_be_total
  :  names:Name_bindings.t
  -> types:t
  -> Internal_type.effects
  -> unit

val generalize : t -> Internal_type.t -> Module_path.absolute Type_scheme.t

(* Want a message like "this expression has type ... but an expression was
   expected with type ..."
   TODO: look to Elm, Rust, etc. for error message inspiration
   Not all errors fit this mold exactly - probably need a proper dedicated type. *)
val type_error : string -> Internal_type.t -> Internal_type.t -> _
