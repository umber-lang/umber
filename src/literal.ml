open Import

type t =
  | Int of int
  | Float of float
  | Char of Uchar.t
  | String of Ustring.t
[@@deriving sexp, variants]

let typ = function
  | Int _ -> Core.Int.typ
  | Float _ -> Core.Float.typ
  | Char _ -> Core.Char.typ
  | String _ -> Core.String.typ
;;
