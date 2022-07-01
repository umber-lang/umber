open Import

type t =
  | Int of int
  | Float of float
  | Char of Uchar.t
  | String of Ustring.t
[@@deriving sexp, variants]

let typ = function
  | Int _ -> Intrinsics.Int.typ
  | Float _ -> Intrinsics.Float.typ
  | Char _ -> Intrinsics.Char.typ
  | String _ -> Intrinsics.String.typ
;;
