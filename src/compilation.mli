open! Import

type 'a t

include Monad.S with type 'a t := 'a t

val fatal_error : Compilation_error.t -> _ t
