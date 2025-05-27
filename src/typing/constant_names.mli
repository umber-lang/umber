open! Core
open! Import
open Names

val binding : Value_name.t
val fun_ : Value_name.t
val match_ : Value_name.t
val lambda_arg : Value_name.t
val underscore : Value_name.t
val closure_env : Value_name.t
val synthetic_arg : int -> Value_name.t
