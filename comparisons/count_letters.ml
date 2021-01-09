open Base

let count_letters str =
  let freqs = Hashtbl.create (module Char) in
  String.iter str ~f:(fun c ->
    Hashtbl.update freqs c ~f:(function
      | None -> 1
      | Some n -> n + 1));
  freqs