open Import
open Names

(* TODO: probably shouldn't overload terminology between [Name_bindings], [Type_bindings],
   and this (let bindings) - maybe rename the others to [Name_context] and [Type_context] *)
module Binding : sig
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Absolute.Set.t
    ; info : 'a
    }
  [@@deriving fields, sexp]
end

type 'a t

val of_bindings : ('a Binding.t * Module_path.Absolute.t) Sequence.t -> 'a t

val to_regrouped_bindings
  :  'a t
  -> ('a Binding.t Nonempty.t * Module_path.Absolute.t) Sequence.t
