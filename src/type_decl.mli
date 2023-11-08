open! Import
open Names

type 'n decl =
  | Abstract
  | Alias of 'n Type_scheme.type_
  | Variants of (Cnstr_name.t * 'n Type_scheme.type_ list) list
  | Record of (Value_name.t * 'n Type_scheme.type_) Nonempty.t
[@@deriving compare, equal, hash, sexp]

type 'n t = Type_param_name.t Unique_list.t * 'n decl
[@@deriving compare, equal, hash, sexp]

val arity : 'n t -> int
val map_exprs : 'n1 t -> f:('n1 Type_scheme.type_ -> 'n2 Type_scheme.type_) -> 'n2 t
val fold_exprs : 'n t -> init:'acc -> f:('acc -> 'n Type_scheme.type_ -> 'acc) -> 'acc
val iter_exprs : 'n t -> f:('n Type_scheme.type_ -> unit) -> unit
val no_free_params : 'n t -> bool
val params_of_list : Type_param_name.t list -> Type_param_name.t Unique_list.t
