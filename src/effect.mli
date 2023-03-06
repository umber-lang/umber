open! Import
open Names

module Operation : sig
  type t =
    { name : Value_name.t
    ; args : Type.Scheme.t Nonempty.t
    ; result : Type.Scheme.t
    }
  [@@deriving sexp]
end

type t =
  { params : Type_param_name.t list
  ; operations : Operation.t list option
  }
[@@deriving sexp]

val map_exprs : t -> f:(Type.Scheme.t -> Type.Scheme.t) -> t
val fold_operations : t -> init:'acc -> f:('acc -> Operation.t -> 'acc) -> 'acc
