open Core
include Uchar
include Comparable.Make (Uchar)

exception Malformed

(* Code "liberated" from:
   https://github.com/ocaml-community/sedlex/blob/master/src/lib/sedlexing.ml *)
let add_to_buffer buf t =
  let p = to_scalar t in
  let open Int in
  if p <= 0x7f
  then Buffer.add_char buf (Char.of_int_exn p)
  else if p <= 0x7ff
  then (
    Buffer.add_char buf (Char.of_int_exn (0xc0 lor (p lsr 6)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor (p land 0x3f))))
  else if p <= 0xffff
  then (
    assert (p < 0xd800 || p >= 0xe000);
    Buffer.add_char buf (Char.of_int_exn (0xe0 lor (p lsr 12)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor ((p lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor (p land 0x3f))))
  else if p <= 0x10ffff
  then (
    Buffer.add_char buf (Char.of_int_exn (0xf0 lor (p lsr 18)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor ((p lsr 12) land 0x3f)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor ((p lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.of_int_exn (0x80 lor (p land 0x3f))))
  else assert false
;;

let of_gen_exn s =
  let open Option.Let_syntax in
  let open Int in
  let get s =
    match Gen.get s with
    | Some c -> Char.to_int c
    | None -> raise Malformed
  in
  Gen.get s
  >>| function
  | '\000' .. '\127' as c -> Uchar.of_char c
  | '\192' .. '\223' as c ->
    let n1 = Char.to_int c in
    let n2 = get s in
    if n2 lsr 6 <> 0b10 then raise Malformed;
    Uchar.of_scalar_exn (((n1 land 0x1f) lsl 6) lor (n2 land 0x3f))
  | '\224' .. '\239' as c ->
    let n1 = Char.to_int c in
    let n2 = get s in
    let n3 = get s in
    if n2 lsr 6 <> 0b10 || n3 lsr 6 <> 0b10 then raise Malformed;
    Uchar.of_scalar_exn
      (((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f))
  | '\240' .. '\247' as c ->
    let n1 = Char.to_int c in
    let n2 = get s in
    let n3 = get s in
    let n4 = get s in
    if n2 lsr 6 <> 0b10 || n3 lsr 6 <> 0b10 || n4 lsr 6 <> 0b10 then raise Malformed;
    Uchar.of_scalar_exn
      (((n1 land 0x07) lsl 18)
      lor ((n2 land 0x3f) lsl 12)
      lor ((n3 land 0x3f) lsl 6)
      lor (n4 land 0x3f))
  | _ -> raise Malformed
;;

let to_string t =
  let buf = Buffer.create 4 in
  add_to_buffer buf t;
  Buffer.contents buf
;;

let sexp_of_t t = Sexp.Atom (to_string t)
let of_string_exn s = Gen.of_string s |> of_gen_exn |> Option.value_exn

let t_of_sexp = function
  | Sexp.Atom s -> of_string_exn s
  | sexp -> raise_s [%message "Cannot parse Uchar.t" (sexp : Sexp.t)]
;;

let to_int = to_scalar
