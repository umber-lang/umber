open! Core
open! Import

type 'a t = ( :: ) of 'a * 'a list [@@deriving compare, equal, hash]

let singleton x = [ x ]
let hd (x :: _) = x
let tl (_ :: xs) = xs

let of_list : 'a list -> 'a t option = function
  | x :: xs -> Some (x :: xs)
  | [] -> None
;;

let of_list_exn : 'a list -> 'a t = function
  | x :: xs -> x :: xs
  | [] -> failwith "Nonempty.of_list_exn: empty list"
;;

let init n ~f = of_list_exn (List.init n ~f)

let cons x = function
  | y :: ys -> x :: y :: ys
;;

let to_list (x :: xs) : _ list = x :: xs

let rev (x :: xs) =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (x :: to_list acc) xs
  in
  loop [ x ] xs
;;

let append (x :: xs) (y :: ys) = x :: (xs @ (y :: ys))
let append_list (x :: xs) ys = x :: (xs @ ys)

let rec rev_append_list xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> rev_append_list xs (x :: to_list ys)
;;

let rev_append xs = rev_append_list (to_list xs)

module C = Container.Make (struct
  type nonrec 'a t = 'a t

  let fold xs = List.fold (to_list xs)
  let length = `Define_using_fold
  let iter = `Define_using_fold
end)

(* Enumerate the values we want to include so we don't unintentionally shadow things like
   [to_list]. *)
let fold = C.fold
let length = C.length
let iter = C.iter
let fold_result = C.fold_result
let exists = C.exists
let for_all = C.for_all
let count = C.count
let sum = C.sum
let find = C.find
let find_map = C.find_map
let to_array = C.to_array

let map (x :: xs) ~f =
  let rec loop acc = function
    | [] -> rev acc
    | x :: xs -> loop (f x :: to_list acc) xs
  in
  loop [ f x ] xs
;;

let concat_map (x :: xs) ~f =
  let rec loop acc = function
    | [] -> rev acc
    | x :: xs -> loop (rev_append (f x) acc) xs
  in
  loop (rev (f x)) xs
;;

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return x = [ x ]
  let bind = concat_map
  let map = `Custom map
end)

let mem xs = List.mem (to_list xs)

let zip_strict xs ys : _ List.Or_unequal_lengths.t =
  match List.zip (to_list xs) (to_list ys) with
  | Ok list -> Ok (of_list_exn list)
  | Unequal_lengths -> Unequal_lengths
;;

let zip_exn xs ys = of_list_exn (List.zip_exn (to_list xs) (to_list ys))

let mapi (x :: xs) ~f =
  f 0 x :: snd (List.fold_map xs ~init:1 ~f:(fun i x -> i + 1, f i x))
;;

(* TODO: This should probably be called map2_lenient or something *)
let map2 (x :: xs) (y :: ys) ~f =
  let rec loop xs ys acc =
    match xs, ys with
    | [], _ | _, [] -> acc
    | x :: xs, y :: ys -> loop xs ys (cons (f x y) acc)
  in
  loop xs ys [ f x y ] |> rev
;;

let unzip ((x, y) :: xys) =
  let rec loop xys xs ys =
    match xys with
    | [] -> rev xs, rev ys
    | (x, y) :: xys -> loop xys (x :: to_list xs) (y :: to_list ys)
  in
  loop xys [ x ] [ y ]
;;

let reduce xs ~f =
  match xs with
  | [ x ] -> x
  | x :: x' :: xs -> List.fold ~init:(f x x') xs ~f
;;

let fold' (x :: xs) ~init ~f = List.fold xs ~init:(init x) ~f
let fold_right xs ~init ~f = List.fold_right (to_list xs) ~init ~f

let fold_map xs ~init ~f =
  List.fold_map (to_list xs) ~init ~f |> Tuple2.map_snd ~f:of_list_exn
;;

let foldi t ~init ~f =
  fold t ~init:(0, init) ~f:(fun (i, acc) x -> i + 1, f i acc x) |> snd
;;

let fold_until t = List.fold_until (to_list t)

let fold_map_until (hd :: tl) ~init ~f : _ Fold_action.t =
  match (f init hd : _ Fold_action.t) with
  | Stop final -> Stop (final, [])
  | Continue (acc, hd) ->
    (match List.fold_map_until tl ~init:acc ~f with
     | Stop (final, tl) -> Stop (final, hd :: tl)
     | Continue (acc, tl) -> Continue (acc, hd :: tl))
;;

module Fold2_result = struct
  type nonrec ('a, 'b) t =
    | Left_trailing of 'a t
    | Right_trailing of 'b t
    | Same_length
end

let rec fold2_list xs ys ~init:acc ~f =
  match xs, ys with
  | [], [] -> acc, Fold2_result.Same_length
  | [], y :: ys -> acc, Right_trailing (y :: ys)
  | x :: xs, [] -> acc, Left_trailing (x :: xs)
  | x :: xs, y :: ys -> fold2_list xs ys ~init:(f acc x y) ~f
;;

let fold2 xs ys ~init ~f = fold2_list (to_list xs) (to_list ys) ~init ~f

let zip (x :: xs) (y :: ys) =
  fold2_list ~init:[ x, y ] xs ys ~f:(fun xs_and_ys x y -> cons (x, y) xs_and_ys)
;;

let fold2_exn xs ys ~init ~f = List.fold2_exn (to_list xs) (to_list ys) ~init ~f
let iteri t ~f = foldi t ~init:() ~f:(fun i () x -> f i x)
let iter2 xs ys ~f = fold2 xs ys ~init:() ~f:(fun () x y -> f x y) |> snd
let iter2_exn xs ys ~f = List.iter2_exn (to_list xs) (to_list ys) ~f

let split_last xs =
  let (last :: rest) = rev xs in
  List.rev rest, last
;;

let ( @ ) = append
let is_empty _ = false
let min_elt xs ~compare = List.min_elt (to_list xs) ~compare |> Option.value_exn
let max_elt xs ~compare = List.max_elt (to_list xs) ~compare |> Option.value_exn
let sexp_of_t sexp_of (x :: xs) = sexp_of_list sexp_of (x :: xs)
let t_of_sexp of_sexp sexp = of_list_exn (list_of_sexp of_sexp sexp)

let rec cartesian_product_all (xs :: xss) =
  match of_list xss with
  | None -> map xs ~f:(fun x -> [ x ])
  | Some xss ->
    let xss' = cartesian_product_all xss in
    concat_map xs ~f:(fun x -> map xss' ~f:(cons x))
;;

let%expect_test "cartesian product" =
  print_s [%sexp (cartesian_product_all [ [ 1; 2 ]; [ 3 ]; [ 4; 5 ] ] : int t t)];
  [%expect {| ((1 3 4) (1 3 5) (2 3 4) (2 3 5)) |}]
;;

let sort xs ~compare = of_list_exn (List.sort (to_list xs) ~compare)
