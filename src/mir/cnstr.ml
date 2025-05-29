open! Core
open! Import

(* TODO: decide what to do with records *)
module T = struct
  type t =
    | Named of Cnstr_name.t
    | Tuple
  [@@deriving compare, equal, sexp, variants]
end

include T
include Comparable.Make (T)
