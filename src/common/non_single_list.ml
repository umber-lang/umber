open! Core
open! Import

module T = struct
  type 'a t =
    | []
    | ( :: ) of 'a * 'a Nonempty.t
  [@@deriving hash, compare, equal]
end

include T

let to_list : _ t -> _ list = function
  | [] -> []
  | x :: xs -> x :: Nonempty.to_list xs
;;

let of_list_convert (list : _ list) ~make ~singleton =
  match list with
  | [] -> make []
  | x :: y :: xs -> make (x :: y :: xs)
  | [ x ] -> singleton x
;;

let of_list_exn =
  of_list_convert ~make:Fn.id ~singleton:(fun _ ->
    failwith "Non_single_list.of_list_exn: got singleton list")
;;

include
  Sexpable.Of_sexpable1
    (List)
    (struct
      include T

      let to_sexpable = to_list
      let of_sexpable = of_list_exn
    end)

include Container.Make (struct
  include T

  let fold t = List.fold (to_list t)
  let iter = `Custom (fun t -> List.iter (to_list t))

  let length =
    `Custom
      (function
       | [] -> 0
       | _ :: xs -> 1 + Nonempty.length xs)
  ;;
end)

let map t ~f =
  match t with
  | [] -> []
  | x :: xs -> f x :: Nonempty.map xs ~f
;;

let fold_until t = List.fold_until (to_list t)
