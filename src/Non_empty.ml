type 'a t = ( :: ) of 'a * 'a list

let of_list : 'a list -> 'a t option = function
  | x :: xs -> Some (x :: xs)
  | [] -> None
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
