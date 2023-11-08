open Import
open Names

(* FIXME: Unify trait constraints with other type constraints. *)
type t = (Trait_name.t * Type_param_name.t Nonempty.t) list
[@@deriving compare, equal, hash, sexp]
