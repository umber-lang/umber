open! Import
open Names

type 'n decl =
  | Abstract
  | Alias of 'n Type_scheme.t
  | Variants of (Cnstr_name.t * 'n Type_scheme.t list) list
  (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record. One problem with this is you
       can no longer define recursive record types, which is a bit annoying. *)
  | Record of (Value_name.t * 'n Type_scheme.t) Nonempty.t
[@@deriving compare, equal, hash, sexp]

type 'n t = Type_param_name.t Unique_list.t * 'n decl
[@@deriving compare, equal, hash, sexp]

val arity : 'n t -> int
val map_exprs : 'n1 t -> f:('n1 Type_scheme.t -> 'n2 Type_scheme.t) -> 'n2 t
val fold_exprs : 'n t -> init:'acc -> f:('acc -> 'n Type_scheme.t -> 'acc) -> 'acc
val iter_exprs : 'n t -> f:('n Type_scheme.t -> unit) -> unit
val no_free_params : 'n t -> bool
val params_of_list : Type_param_name.t list -> Type_param_name.t Unique_list.t
