open! Core

type 'a t = 'a list [@@deriving compare, equal, hash, sexp_of]

let of_list list ~compare =
  match List.find_a_dup list ~compare with
  | None -> Ok list
  | Some duplicate -> Error duplicate
;;

let of_list_exn list ~compare =
  List.exn_if_dup list ~compare ~to_sexp:[%sexp_of: _];
  list
;;

let t_of_sexp a_of_sexp (sexp : Sexp.t) =
  match sexp with
  | List sexps ->
    (* Sexp comparison is not perfect, but should be better than no validation of
       duplicates in the sexp. *)
    List.exn_if_dup sexps ~compare:Sexp.compare ~to_sexp:Fn.id;
    List.map sexps ~f:a_of_sexp
  | Atom _ ->
    raise_s [%message "Unique_list.t_of_sexp: expected list but got atom" (sexp : Sexp.t)]
;;

let empty = []
let singleton x = [ x ]
let quickcheck_observer = List.quickcheck_observer
