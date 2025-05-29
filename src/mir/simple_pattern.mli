open! Core
open! Import

type t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of t * Value_name.t
  | Cnstr_appl of Cnstr.t * (t * Module_path.absolute Type_scheme.type_) list
[@@deriving sexp, variants]

val flatten_typed_pattern : Typed_ast.Pattern.generalized -> t Nonempty.t
val flatten_typed_pattern_no_unions : Typed_ast.Pattern.generalized -> label:string -> t
val names : t -> Value_name.Set.t

module Coverage : sig
  type simple_pattern = t
  type t

  val of_pattern : simple_pattern -> t
  val of_patterns : simple_pattern Nonempty.t -> t
  val combine : t -> t -> t

  val missing_cases
    :  t
    -> ctx:Context.t
    -> input_type:Module_path.absolute Type_scheme.type_
    -> simple_pattern list
end
