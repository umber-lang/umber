open! Core
open! Import

type t [@@deriving sexp_of]

val create : names:Name_bindings.t -> name_table:Mir_name.Name_table.t -> t
val add_value_name : t -> Value_name.t -> t * Mir_name.t
val copy_name : t -> Mir_name.t -> Mir_name.t

module Name_kind : sig
  (* TODO: Should we unify this with cnstr information? It seems to have a similar
       flavour, and having one place to store "what this binding is" seems helpful. *)
  type t =
    | Local
    | External of { arity : int }
    | Effect_op of Effect_op_id.t
    | Bool_intrinsic of { tag : Cnstr_tag.t }
  [@@deriving sexp_of]
end

val find_value_name : t -> Value_name.Absolute.t -> Mir_name.t * Name_kind.t
val find_value_name_assert_local : t -> Value_name.t -> Mir_name.t
val find_value_name_assert_external : t -> Value_name.t -> Mir_name.t
val peek_value_name : t -> Value_name.Absolute.t -> Mir_name.t option

type find_override := Value_name.Absolute.t -> Mir_name.t -> Mir_name.t option

val with_find_override : t -> f:find_override -> t
val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
val with_path_into_defs : t -> Module_path.Absolute.t -> f:(t -> t * 'a) -> t * 'a
val current_path : t -> Module_path.Absolute.t
val find_cnstr_info : t -> Module_path.absolute Type_scheme.type_ -> Cnstr_info.t

val find_cnstr_info_from_decl
  :  t
  -> Module_path.absolute Type_decl.decl
  -> follow_aliases:bool
  -> Cnstr_info.t option
