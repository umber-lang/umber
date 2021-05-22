open Import

type 'a t = ( :: ) of 'a * 'a list

let of_list : 'a list -> 'a t option = function
  | x :: xs -> Some (x :: xs)
  | [] -> None
;;

let of_list_exn : 'a list -> 'a t = function
  | x :: xs -> x :: xs
  | [] -> failwith "Non_empty.of_list_exn: empty list"
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
let rev_append xs ys = List.rev_append (to_list xs) (to_list ys) |> of_list_exn

include Container.Make (struct
  type nonrec 'a t = 'a t

  let fold xs = List.fold (to_list xs)
  let length = `Define_using_fold
  let iter = `Define_using_fold
end)

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return x = [ x ]

  let bind (x :: xs) ~f =
    let rec loop xs ~f acc =
      match (xs : _ list) with
      | [] -> acc
      | x :: xs -> loop xs ~f (rev_append (f x) acc)
    in
    loop xs ~f (f x) |> rev
  ;;

  let map = `Define_using_bind
end)

let zip (x :: xs) (y :: ys) =
  let rec loop xs ys acc =
    match xs, ys with
    | [], _ | _, [] -> acc
    | x :: xs, y :: ys -> loop xs ys ((x, y) :: to_list acc)
  in
  loop xs ys [ x, y ] |> rev
;;

let unzip ((x, y) :: xys) =
  let rec loop xys xs ys =
    match xys with
    | [] -> xs, ys
    | (x, y) :: xys -> loop xys (x :: to_list xs) (y :: to_list ys)
  in
  loop xys [ x ] [ y ]
;;

let is_empty _ = false
let min_elt xs ~compare = List.min_elt (to_list xs) ~compare |> Option.value_exn
let max_elt xs ~compare = List.max_elt (to_list xs) ~compare |> Option.value_exn
let sexp_of_t sexp_of (x :: xs) = sexp_of_list sexp_of (x :: xs)
let t_of_sexp of_sexp sexp = of_list_exn (list_of_sexp of_sexp sexp)
