open! Import
open Names

val check : names:Name_bindings.t -> Module_name.t -> unit

val check_val_scheme_vs_inferred_scheme
  :  names:Name_bindings.t
  -> val_scheme:Module_path.absolute Type_scheme.t
  -> inferred_scheme:Module_path.absolute Type_scheme.t
  -> unit
