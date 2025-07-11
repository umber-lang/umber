open Core

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

  let id = finish ~f:Fn.id

  let result = function
    | Continue x -> Ok x
    | Stop err -> Error err
  ;;
end

module Option : sig
  include module type of Option

  val fold_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t
end = struct
  include Option

  let fold_until t ~init ~f : _ Fold_action.t =
    match t with
    | None -> Continue init
    | Some x -> f init x
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

  val fold_map_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc * 'b, 'final) Fold_action.t)
    -> ('acc * 'b t, 'final * 'b t) Fold_action.t
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
      let%bind.Fold_action init = f init x in
      fold_until xs ~init ~f
  ;;

  let fold_map_until list ~init ~f : _ Fold_action.t =
    match
      fold_until list ~init:(init, []) ~f:(fun (acc, xs) x ->
        match (f acc x : _ Fold_action.t) with
        | Continue (acc, x) -> Continue (acc, x :: xs)
        | Stop final -> Stop (final, xs))
    with
    | Continue (acc, xs) -> Continue (acc, List.rev xs)
    | Stop (final, xs) -> Stop (final, List.rev xs)
  ;;
end

module Map : sig
  include module type of Map

  val fold_until
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t
end = struct
  include Map

  let fold_until t ~init ~f =
    Map.fold_until
      t
      ~init
      ~f:(fun ~key ~data init ->
        match (f ~key ~data init : _ Fold_action.t) with
        | Continue x -> Continue x
        | Stop _ as stop -> Stop stop)
      ~finish:Fold_action.continue
  ;;
end

exception Compiler_bug of Sexp.t [@@deriving sexp_of]

let compiler_bug msg = raise (Compiler_bug msg)

let assert_or_compiler_bug ~here cond =
  if not cond
  then compiler_bug [%message "assertion failed" (here : Source_code_position.t)]
;;

let ok_or_compiler_bug ~here = function
  | Ok x -> x
  | Error error ->
    compiler_bug [%message (error : Error.t) (here : Source_code_position.t)]
;;

let never_happens here _ : Nothing.t =
  compiler_bug
    [%message "this is never supposed to happen" (here : Source_code_position.t)]
;;

(* TODO: Rename to something a little clearer like [debug_print] *)
let eprint_s (here : Source_code_position.t) msg =
  let file = Filename.basename here.pos_fname in
  match file with
  | "" -> eprint_s [%sexp (Info.tag ~tag:file (Info.of_lazy_sexp msg) : Info.t)]
  | _ -> ()
;;
