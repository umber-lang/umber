open! Core
open! Import

(* TODO: This doesn't handle polymorphic types particularly smartly. Should think about
   whether that matters. Actually, since this is only used for extern declarations, we
   don't need to be super clever about this. *)
val arity_of_type : names:Name_bindings.t -> Module_path.absolute Type_scheme.type_ -> int
