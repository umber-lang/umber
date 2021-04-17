open Core_kernel

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let rec iter_pairs list ~f =
  match list with
  | [] | [ _ ] -> ()
  | a :: (b :: _ as rest) ->
    f a b;
    iter_pairs rest ~f
;;

let single_or_list wrapper = function
  | [ x ] -> x
  | xs -> wrapper xs
;;

let list_split_last list =
  let rec loop acc = function
    | [] -> None
    | [ last ] -> Some (List.rev acc, last)
    | x :: xs -> loop (x :: acc) xs
  in
  loop [] list
;;

let option_or_default x ~f =
  match x with
  | Some x -> x
  | None -> f ()
;;

let compiler_bug msg = failwith ("COMPILER BUG: " ^ Sexp.to_string msg)
