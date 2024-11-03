open! Core
open! Names

val simplify_type
  :  Module_path.absolute Type_scheme.t
  -> Module_path.absolute Type_scheme.type_

(* FIXME: Remove this hack, and probably make the above function explicitly return an
   empty list of constraints *)
val simplify_types
  :  positive_types:Module_path.absolute Type_scheme.t list
  -> negative_types:Module_path.absolute Type_scheme.t list
  -> Module_path.absolute Type_scheme.t list * Module_path.absolute Type_scheme.t list
