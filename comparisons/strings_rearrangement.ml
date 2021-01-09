open Base

let rec permutations = function
  | [] -> []
  | [_] as single -> [single]
  | list -> List.foldi list ~init:[] ~f:(fun i acc x ->
    let without_i = List.filteri list ~f:(fun j _ -> i <> j) in
    acc @ List.map ~f:(fun p -> x :: p) (permutations without_i))

let rec pairs = function
  | [] | [_] -> []
  | a :: ((b :: _) as tail) -> (a, b) :: pairs tail

let differ_by n s1 s2 =
  let rec loop n s1 s2 pos =
    if pos >= String.length s1 then n = 0
    else
      let n' = if Char.(s1.[pos] = s2.[pos]) then n else n - 1 in
      loop n' s1 s2 (pos + 1)
  in
  if String.length s1 <> String.length s2 then false
  else loop n s1 s2 0

let strings_rearrangement list =
  List.exists (permutations list) ~f:(fun list ->
    pairs list |> List.for_all ~f:(fun (s1, s2) -> differ_by 1 s1 s2))