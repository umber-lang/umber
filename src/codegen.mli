open Import

module Value_table : sig
  type t

  val create : unit -> t
  val parse : Llvm.llcontext -> string -> t
end

type t

val of_mir
  :  context:Llvm.llcontext
  -> source_filename:Filename.t
  -> values:Value_table.t
  -> Mir.t
  -> (t, Compilation_error.t) result

val of_mir_exn
  :  context:Llvm.llcontext
  -> source_filename:Filename.t
  -> values:Value_table.t
  -> Mir.t
  -> t

val to_string : t -> string
val print : t -> to_:Filename.t -> unit
