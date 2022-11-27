open Import

module T = struct
  type t =
    | Int of int
    | Float of float
    | Char of Uchar.t
    | String of Ustring.t
  [@@deriving compare, equal, hash, sexp, variants]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let typ = function
  | Int _ -> Intrinsics.Int.typ
  | Float _ -> Intrinsics.Float.typ
  | Char _ -> Intrinsics.Char.typ
  | String _ -> Intrinsics.String.typ
;;
