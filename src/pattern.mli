open Import
open Names

type ('typ, 'name) t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of ('typ, 'name) t * Value_name.t
  | Cnstr_appl of 'name Cnstr_name.Qualified.t * ('typ, 'name) t list
  | Tuple of ('typ, 'name) t list
  | Record of (Value_name.t * ('typ, 'name) t option) Nonempty.t
  | Union of ('typ, 'name) t * ('typ, 'name) t
  | Type_annotation of ('typ, 'name) t * 'typ
[@@deriving equal, sexp, variants]

val fold_until
  :  ('typ, 'name) t
  -> init:'acc
  -> f:
       ('acc
        -> ('typ, 'name) t
        -> ([< `Defer of 'acc | `Halt of 'acc ], 'final) Fold_action.t)
  -> ('acc, 'final) Fold_action.t

val fold_types : ('typ, 'name) t -> init:'acc -> f:('acc -> 'typ -> 'acc) -> 'acc

module Names : sig
  type ('a, 'b) pattern := ('a, 'b) t
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

  val fold
    :  ('typ, 'name) pattern
    -> init:'acc
    -> f:('acc -> Value_name.t -> 'acc)
    -> 'acc

  val gather
    :  ('a, 'b) pattern
    -> type_source:Name_bindings.Name_entry.Type_source.t
    -> fixity:Fixity.t option
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
end
