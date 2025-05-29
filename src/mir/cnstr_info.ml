open! Core
open! Import

type t =
  | Variants of
      { constant_cnstrs : Cnstr_name.t list
      ; non_constant_cnstrs : (Cnstr_name.t * int) list
      }
  | Tuple of { arg_count : int }
[@@deriving sexp_of]

let of_variants variants =
  let constant_cnstrs, non_constant_cnstrs =
    List.partition_map variants ~f:(fun (cnstr_name, arg_count) ->
      if arg_count = 0 then First cnstr_name else Second (cnstr_name, arg_count))
  in
  Variants { constant_cnstrs; non_constant_cnstrs }
;;

let of_tuple arg_count = Tuple { arg_count }

let fold t ~init:acc ~f =
  match t with
  | Variants { constant_cnstrs; non_constant_cnstrs } ->
    let acc =
      List.foldi constant_cnstrs ~init:acc ~f:(fun i acc cnstr_name ->
        f acc (Cnstr.Named cnstr_name) (Cnstr_tag.of_int i) ~arg_count:0)
    in
    List.foldi non_constant_cnstrs ~init:acc ~f:(fun i acc (cnstr_name, arg_count) ->
      f acc (Named cnstr_name) (Cnstr_tag.of_int i) ~arg_count)
  | Tuple { arg_count } -> f acc Tuple Cnstr_tag.default ~arg_count
;;

let cnstrs t =
  List.rev (fold t ~init:[] ~f:(fun acc cnstr _tag ~arg_count:_ -> cnstr :: acc))
;;

let tag t cnstr =
  match t, (cnstr : Cnstr.t) with
  | Variants { constant_cnstrs; non_constant_cnstrs }, Named cnstr_name ->
    (match
       List.findi constant_cnstrs ~f:(fun (_ : int) -> Cnstr_name.( = ) cnstr_name)
     with
     | Some (i, (_ : Cnstr_name.t)) -> Cnstr_tag.of_int i
     | None ->
       (match
          List.findi non_constant_cnstrs ~f:(fun _ -> Cnstr_name.( = ) cnstr_name << fst)
        with
        | Some (i, _) -> Cnstr_tag.of_int i
        | None ->
          compiler_bug
            [%message
              "Constructor name lookup failed"
                (cnstr_name : Cnstr_name.t)
                ~cnstr_info:(t : t)]))
  | Tuple _, Tuple -> Cnstr_tag.default
  | Variants _, Tuple | Tuple _, Named _ ->
    compiler_bug
      [%message "Incompatible cnstr info" (cnstr : Cnstr.t) ~cnstr_info:(t : t)]
;;
