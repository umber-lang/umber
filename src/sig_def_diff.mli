open Names

type t

val create : names:Name_bindings.t -> Module_name.t -> t
val raise_if_nonempty : t -> unit
