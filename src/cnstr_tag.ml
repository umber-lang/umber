open! Import
include Int

let of_int t = t
let to_int t = t
let default = 0
let closure = 0x3001

(* let no_scan = 0x8000 *)
let int = 0x8001
let char = 0x8002
let float = 0x8003
let string = 0x8004
let continuation = 0x8010
