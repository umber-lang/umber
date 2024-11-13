open! Import
open Names

(* Importantly, none of these can be valid value names a user could enter. *)

let binding = Value_name.of_string_unchecked "*binding"
let fun_ = Value_name.of_string_unchecked "*fun"
let match_ = Value_name.of_string_unchecked "match"
let lambda_arg = Value_name.of_string_unchecked "*lambda_arg"
let underscore = Value_name.of_string_unchecked "_"
let closure_env = Value_name.of_string_unchecked "*closure_env"
let synthetic_arg i = Value_name.of_string_unchecked [%string "*arg%{i#Int}"]
let cps_arg = Value_name.of_string_unchecked "*cps"
