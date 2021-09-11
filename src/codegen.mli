open Import
open Names

type t

val of_mir : module_name:Module_name.t -> Mir.t -> t
val to_string : t -> string
val print : t -> to_:Filename.t -> unit
