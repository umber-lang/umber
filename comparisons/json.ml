open Base

type t =
  | Object of (string * t) list
  | Array of t list
  | String of string
  | Bool of bool
  | Int of int
  | Float of float
  | Null

exception Parse_error of string

let parse =
  let rec eat_whitespace str i =
    if Char.(str.[i] = ' ' || str.[i] = '\n') then eat_whitespace str (i + 1)
    else i
  and parse_value str i =
    match str.(i) with
    | 
  and parse_array str i =
    match str.(i) with
    | 
  and parse_object str i =
    match str.(i) with
    | 
  and parse str i =
    match str.(i) with
    | '{' -> parse_object str (eat_whitespace str i)
    | '[' -> parse_array str (eat_whitespace str i)
  in
  fun str -> parse str 0