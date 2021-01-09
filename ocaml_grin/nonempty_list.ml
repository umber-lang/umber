type 'a t = 'a list

let of_list = function
  | [] -> None
  | xs -> Some xs
;;

let of_list_exn = function
  | [] -> raise (Invalid_argument "Empty list")
  | xs -> xs
;;

let to_list l = l
