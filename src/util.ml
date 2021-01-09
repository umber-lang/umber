open Core_kernel

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let rec iter_pairs lst ~f =
  match lst with
  | [] | [ _ ] -> ()
  | a :: (b :: _ as rest) ->
    f a b;
    iter_pairs rest ~f
;;

let single_or_list wrapper = function
  | [ x ] -> x
  | xs -> wrapper xs
;;

let option_or_default x ~f =
  match x with
  | Some x -> x
  | None -> f ()
;;

let compiler_bug msg = failwith ("COMPILER BUG: " ^ Sexp.to_string msg)
