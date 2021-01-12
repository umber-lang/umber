open Import
open Names

module Binding : sig
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Qualified.Set.t
    ; info : 'a
    }
end

type 'a t

val create : unit -> 'a t
val add_binding : 'a t -> 'a Binding.t -> Module_path.t -> unit
val of_bindings : ('a Binding.t * Module_path.t) Sequence.t -> 'a t

(* TODO: what's this? *)
(*val of_bindings : 'seq -> iter:('seq -> f:('a Binding.t -> unit) -> unit) -> 'a t*)
val to_regrouped_bindings : 'a t -> ('a Binding.t list * Module_path.t) Sequence.t
