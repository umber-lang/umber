open Import
include Array

module T = struct
  type t = Uchar.t array [@@deriving compare, equal, quickcheck]
  type elt = Uchar.t

  let add_substring_to_buffer buf t ~pos ~len =
    let rec loop t buf i =
      if i < len
      then (
        Uchar.add_to_buffer buf t.(i);
        loop t buf (i + 1))
    in
    loop t buf pos
  ;;

  let add_to_buffer buf t = add_substring_to_buffer buf t ~pos:0 ~len:(length t)

  let to_string t =
    let buf = Buffer.create (length t * 4) in
    add_to_buffer buf t;
    Buffer.contents buf
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let of_gen_exn s =
    let rec loop s q =
      match Uchar.of_gen_exn s with
      | Some c ->
        Queue.enqueue q c;
        loop s q
      | None -> ()
    in
    let q = Queue.create () in
    loop s q;
    Queue.to_array q
  ;;

  let of_string_exn s = of_gen_exn (Gen.of_string s)

  let t_of_sexp = function
    | Sexp.Atom s -> of_string_exn s
    | sexp -> raise_s [%message "Cannot parse Ustring.t" (sexp : Sexp.t)]
  ;;

  let hash_fold_t init = fold ~init ~f:Uchar.hash_fold_t
  let hash = hash_fold_t (Hash.create ()) >> Hash.get_hash_value
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let empty = [||]
let ( ^ ) = append
let mem t c = Array.mem t c ~equal:Uchar.( = )
let index t uchar = Array.findi t ~f:(fun _ c -> Uchar.(c = uchar)) |> Option.map ~f:fst
let index_exn t uchar = index t uchar |> Option.value_exn
let make len x = Array.create ~len x
let of_array_unchecked = Fn.id
let of_ustring = Fn.id
let to_ustring = Fn.id
let print ?(out = stdout) s = Out_channel.output_string out (to_string s)

let print_endline ?(out = stdout) s =
  print ~out s;
  Out_channel.newline out
;;
