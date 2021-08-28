open Import

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

let cons x = function
  | y :: ys -> x :: y :: ys
;;

let to_list (x :: xs) : 'a list = x :: xs

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

include Container.Make (struct
  type nonrec 'a t = 'a t

  let fold xs = List.fold (to_list xs)
  let length = `Define_using_fold
  let iter = `Define_using_fold
end)

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
  let map = `Define_using_bind
end)

let zip (x :: xs) (y :: ys) =
  let rec loop xs ys acc =
    match xs, ys with
    | [], _ | _, [] -> acc
    | x :: xs, y :: ys -> loop xs ys (cons (x, y) acc)
  in
  loop xs ys [ x, y ] |> rev
;;

let mapi (x :: xs) ~f =
  f 0 x :: snd (List.fold_map xs ~init:1 ~f:(fun i x -> i + 1, f i x))
;;

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

let fold_right xs ~init ~f = List.fold_right (to_list xs) ~init ~f

let fold_map xs ~init ~f =
  List.fold_map (to_list xs) ~init ~f |> Tuple2.map_snd ~f:of_list_exn
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
