open! Import
open Names

(* FIXME: Cleanup *)
(* val map
  :  ('typ, 'name) t
  -> f:(('typ, 'name) t -> (('typ, 'name) t, ('typ, 'name) t) Map_action.t)
  -> ('typ, 'name) t

val fold_until
  :  ('typ, 'name) t
  -> init:'acc
  -> f:
       ('acc
        -> ('typ, 'name) t
        -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_types : ('typ, 'name) t -> init:'acc -> f:('acc -> 'typ -> 'acc) -> 'acc *)

(* TODO: consider abstracting this. It would help out with the verbosity of type errors
   for some of the complex types in typed.ml. *)
type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

val empty : t

val add_name
  :  t
  -> Value_name.t
  -> Internal_type.t
  -> type_source:Name_bindings.Name_entry.Type_source.t
  -> fixity:Fixity.t option
  -> t

val add_fresh_name
  :  t
  -> Value_name.t
  -> type_source:Name_bindings.Name_entry.Type_source.t
  -> fixity:Fixity.t option
  -> t * Internal_type.t

val gather
  :  'pattern
  -> type_source:Name_bindings.Name_entry.Type_source.t
  -> fixity:Fixity.t option
  -> fold:('pattern -> init:t -> f:(t -> Value_name.t -> t) -> t)
  -> Name_bindings.Name_entry.t Value_name.Map.t

val find : t -> Value_name.t -> Name_bindings.Name_entry.t option
val mem : t -> Value_name.t -> bool

val merge
  :  t
  -> t
  -> combine:
       (key:Value_name.t
        -> Name_bindings.Name_entry.t
        -> Name_bindings.Name_entry.t
        -> Name_bindings.Name_entry.t)
  -> t
