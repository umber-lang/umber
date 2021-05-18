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

let sexp_of_t sexp_of (x :: xs) = sexp_of_list sexp_of (x :: xs)
let t_of_sexp of_sexp sexp = of_list_exn (list_of_sexp of_sexp sexp)
