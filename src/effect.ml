open! Import
open Names

module Operation = struct
  type t =
    { name : Value_name.t
    ; args : Type.Scheme.t Nonempty.t
    ; result : Type.Scheme.t
    }
  [@@deriving sexp]

  let map_exprs { name; args; result } ~f =
    { name; args = Nonempty.map args ~f; result = f result }
  ;;
end

type t =
  { params : Type_param_name.t list
  ; operations : Operation.t list option
  }
[@@deriving sexp]

let map_exprs { params; operations } ~f =
  { params; operations = Option.map operations ~f:(List.map ~f:(Operation.map_exprs ~f)) }
;;

let fold_operations t ~init ~f =
  Option.fold t.operations ~init ~f:(fun init operations -> List.fold operations ~init ~f)
;;
