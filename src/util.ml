open Core_kernel

module Map_action = struct
  type ('a, 'b) t =
    | Defer of 'a
    | Halt of 'b
    | Retry of 'a
  [@@deriving variants]
end

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let on f g x y = f (g x) (g y)

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

let compiler_bug msg = raise_s [%message "COMPILER BUG" (msg : Sexp.t)]

let assert_or_compiler_bug ~here cond =
  if not cond
  then compiler_bug [%message "assertion failed" (here : Source_code_position.t)]
;;

let never_happens here _ : Nothing.t =
  compiler_bug
    [%message "this is never supposed to happen" (here : Source_code_position.t)]
;;
