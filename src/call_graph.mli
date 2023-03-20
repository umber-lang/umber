open Import
open Names

(* TODO: probably shouldn't overload terminology between [Name_bindings], [Type_bindings],
   and this (let bindings) - maybe rename the others to [Name_context] and [Type_context] *)
module Binding : sig
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Relative.Set.t
    ; info : 'a
    }
  [@@deriving fields, sexp]
end

type 'a t

val create : unit -> 'a t
val add_binding : 'a t -> 'a Binding.t -> Name_bindings.Path.t -> unit
val of_bindings : ('a Binding.t * Name_bindings.Path.t) Sequence.t -> 'a t

val to_regrouped_bindings
  :  'a t
  -> ('a Binding.t Nonempty.t * Name_bindings.Path.t) Sequence.t
