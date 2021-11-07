open Core_kernel

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let on f g x y = f (g x) (g y)

module Map_action = struct
  type ('a, 'b) t =
    | Defer of 'a
    | Halt of 'b
    | Retry of 'a
  [@@deriving variants]
end

module Fold_action = struct
  module T = struct
    type ('acc, 'final) t =
      | Continue of 'acc
      | Stop of 'final
    [@@deriving variants]

    let bind t ~f =
      match t with
      | Continue acc -> f acc
      | Stop _ as t -> t
    ;;

    let map = `Define_using_bind
    let return = continue
  end

  include T
  include Monad.Make2 (T)

  let finish t ~f =
    match t with
    | Continue acc -> acc
    | Stop acc -> f acc
  ;;
end

module List : sig
  include module type of List

  val iter_pairs : 'a t -> f:('a -> 'a -> unit) -> unit
  val split_last : 'a t -> ('a t * 'a) option

  val fold_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t
end = struct
  include List

  let rec iter_pairs list ~f =
    match list with
    | [] | [ _ ] -> ()
    | a :: (b :: _ as rest) ->
      f a b;
      iter_pairs rest ~f
  ;;

  let split_last list =
    let rec loop acc = function
      | [] -> None
      | [ last ] -> Some (List.rev acc, last)
      | x :: xs -> loop (x :: acc) xs
    in
    loop [] list
  ;;

  let rec fold_until list ~init ~f : _ Fold_action.t =
    match list with
    | [] -> Continue init
    | x :: xs ->
      (match (f init x : _ Fold_action.t) with
      | Stop _ as stop -> stop
      | Continue init -> fold_until xs ~init ~f)
  ;;
end

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
