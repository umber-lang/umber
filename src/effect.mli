open! Import
open Names

module Operation : sig
  type 'n t =
    { name : Value_name.t
    ; args : 'n Type.Scheme.t Nonempty.t
    ; result : 'n Type.Scheme.t
    }
  [@@deriving sexp]
end

type 'n t =
  { params : Type_param_name.t list
  ; operations : 'n Operation.t list option
  }
[@@deriving sexp]

val map_exprs : 'n1 t -> f:('n1 Type.Scheme.t -> 'n2 Type.Scheme.t) -> 'n2 t
val fold_operations : 'n t -> init:'acc -> f:('acc -> 'n Operation.t -> 'acc) -> 'acc
