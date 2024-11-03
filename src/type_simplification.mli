open! Core
open! Names

val simplify_type
  :  Module_path.absolute Type_scheme.t
  -> context_vars:Type_param.Set.t By_polarity.t
  -> Module_path.absolute Type_scheme.t
