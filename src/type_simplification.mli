open! Core
open! Names

val simplify_type
  :  Module_path.absolute Type_scheme.t
  -> context_vars:int Type_param.Map.t By_polarity.t
  -> Module_path.absolute Type_scheme.t

val get_positive_and_negative_vars
  :  _ Type_scheme.type_
  -> context_vars:int Type_param.Map.t By_polarity.t
  -> int Type_param.Map.t By_polarity.t
