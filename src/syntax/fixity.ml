open! Core
open! Import

module Level = struct
  module T = struct
    type t = int [@@deriving compare, equal, hash, sexp]
  end

  include T
  module O = Comparable.Infix (T)
  include O

  let min = 0
  let max = 9
  let all = List.range ~start:`inclusive ~stop:`inclusive min max

  let of_int_exn n =
    if min <= n && n <= max
    then n
    else raise_s [%message "Invalid precedence level" (n : int)]
  ;;

  let pred n = if n = min then None else Some (n - 1)
  let succ n = if n = max then None else Some (n + 1)
end

module Assoc = struct
  type t =
    | Non_assoc
    | Left
    | Right
  [@@deriving compare, equal, hash, sexp, enumerate]

  let compatible t1 t2 =
    match t1, t2 with
    | Left, Left | Right, Right -> true
    | _ -> false
  ;;
end

type t = Assoc.t * Level.t [@@deriving compare, equal, hash, sexp, enumerate]

(* TODO: maybe the default should be no associativity and unknown precedence *)
let default = Assoc.Non_assoc, 9
let of_decl_exn fixity n = fixity, Level.of_int_exn n
