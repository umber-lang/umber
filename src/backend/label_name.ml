open! Core
open! Import

module T = struct
  type t = string [@@deriving sexp, compare, equal, hash]
end

include T
include Hashable.Make (T)

let of_string s =
  (* See the NASM for docs for what constitutes a valid identifier:
       https://www.nasm.us/xdoc/2.16.03/html/nasmdoc3.html#section-3.1 *)
  let buffer = Buffer.create (String.length s) in
  let escape_used = ref false in
  let escaping_now = ref false in
  String.iteri s ~f:(fun i c ->
    let needs_escape =
      if i = 0
      then (
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' | '?' -> false
        | _ -> true)
      else (
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' | '@' | '~' | '.' | '?'
          -> false
        | _ -> true)
    in
    match !escaping_now, needs_escape with
    | false, false -> (* No escaping needed. *) Buffer.add_char buffer c
    | false, true ->
      (* Start escaping. *)
      escape_used := true;
      escaping_now := true;
      Buffer.add_string buffer "__"
    | true, false ->
      (* Stop escaping.*)
      escaping_now := false;
      Buffer.add_char buffer c
    | true, true -> (* Keep escaping. *) ());
  if !escape_used
  then (
    Buffer.add_char buffer '#';
    Buffer.add_string buffer (Int.to_string (String.hash s)));
  Buffer.contents buffer
;;

let to_string = Fn.id
let of_mir_name = Mir_name.to_string >> of_string
let of_extern_name = Extern_name.to_string >> of_string
