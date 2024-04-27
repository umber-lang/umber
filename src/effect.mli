open! Import
open Names

module Operation : sig
  type 'n t =
    { name : Value_name.t
    ; args : 'n Type_scheme.type_ Nonempty.t
    ; result : 'n Type_scheme.type_
    }
  [@@deriving sexp, fields]
end

type 'n t =
  { params : Type_param_name.t Unique_list.t
  ; operations : 'n Operation.t list option
  }
[@@deriving sexp, fields]

val map_exprs : 'n1 t -> f:('n1 Type_scheme.type_ -> 'n2 Type_scheme.type_) -> 'n2 t
val fold_operations : 'n t -> init:'acc -> f:('acc -> 'n Operation.t -> 'acc) -> 'acc
