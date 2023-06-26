open Import
open Names

module Name_entry : sig
  type t [@@deriving equal, sexp]

  module Type_source : sig
    type t =
      | Placeholder
      | Let_inferred
      | Val_declared
      | Val_and_let
      | Extern_declared
    [@@deriving compare, enumerate, equal, sexp, variants]
  end

  val typ : t -> Type.t
  val scheme : t -> Module_path.absolute Type.Scheme.t option
  val type_source : t -> Type_source.t
  val fixity : t -> Fixity.t option
  val extern_name : t -> Extern_name.t option
  val create : ?fixity:Fixity.t -> type_source:Type_source.t -> Type.t -> t
  val merge : t -> t -> t
  val identical : t -> t -> bool
end

module Type_entry : sig
  type t

  module Id : sig
    type t [@@deriving equal, compare, hash, sexp_of]

    include Comparable.S_plain with type t := t
  end

  val decl : t -> Module_path.absolute Type.Decl.t
  val identical : t -> t -> bool
  val id : t -> Id.t
end

type t [@@deriving sexp]

val name_error : msg:string -> Ustring.t -> 'a

(* Handling of default values *)
val empty : t
val core : t
val prelude : t Lazy.t
val without_std : t -> t

(* Querying/updating names *)
val find_entry_with_path
  :  t
  -> Value_name.Relative.t
  -> Value_name.Absolute.t * Name_entry.t

val find_type : t -> Value_name.Relative.t -> Type.t
val find_cnstr_type : t -> Cnstr_name.Relative.t -> Type.t
val find_fixity : t -> Value_name.Relative.t -> Fixity.t

val set_inferred_scheme
  :  t
  -> Value_name.t
  -> Module_path.absolute Type.Scheme.t
  -> shadowing_allowed:bool
  -> check_existing:(Name_entry.t -> unit)
  -> t

val add_name_placeholder : t -> Value_name.t -> t
val add_type_decl_placeholder : t -> Type_name.t -> Module_path.relative Type.Decl.t -> t
val add_effect_placeholder : t -> Effect_name.t -> Effect.t -> t

(** Fold over all the local (non-imported) names bound. *)
val fold_local_names
  :  t
  -> init:'a
  -> f:('a -> Value_name.Absolute.t -> Name_entry.t -> 'a)
  -> 'a

val merge_names
  :  t
  -> Name_entry.t Value_name.Map.t
  -> combine:(Value_name.t -> Name_entry.t -> Name_entry.t -> Name_entry.t)
  -> t

val find_absolute_entry : t -> Value_name.Absolute.t -> Name_entry.t

(** Find a type declaration given an absolute qualified name *)
val find_absolute_type_decl
  :  ?defs_only:bool
  -> t
  -> Type_name.Absolute.t
  -> Module_path.absolute Type.Decl.t

val find_absolute_type_entry
  :  ?defs_only:bool
  -> t
  -> Type_name.Absolute.t
  -> Type_entry.t

val absolutify_type_name : t -> Type_name.Relative.t -> Type_name.Absolute.t
val absolutify_value_name : t -> Value_name.Relative.t -> Value_name.Absolute.t

val absolutify_type_expr
  :  t
  -> ('v, 'pf, Module_path.relative) Type.Expr.t
  -> ('v, 'pf, Module_path.absolute) Type.Expr.t

val absolutify_type_decl
  :  t
  -> Module_path.relative Type.Decl.t
  -> Module_path.absolute Type.Decl.t

(* Scope handling *)
val current_path : t -> Module_path.Absolute.t
val into_module : t -> place:[ `Sig | `Def ] -> Module_name.t -> t
val into_parent : t -> t
val with_submodule : t -> place:[ `Sig | `Def ] -> Module_name.t -> f:(t -> t) -> t

val with_submodule'
  :  t
  -> place:[ `Sig | `Def ]
  -> Module_name.t
  -> f:(t -> t * 'a)
  -> t * 'a

val with_path_into_defs : t -> Module_path.Absolute.t -> f:(t -> t * 'a) -> t * 'a

(* AST handling *)
val import : t -> Module.Import.t -> t
val import_all : t -> Module_path.Relative.t -> t
val import_all_absolute : t -> Module_path.Absolute.t -> t

val add_val
  :  t
  -> Value_name.t
  -> Fixity.t option
  -> Module_path.absolute Type.Scheme.Bounded.t
  -> unify:(Type.t -> Type.t -> unit)
  -> t

val add_extern
  :  t
  -> Value_name.t
  -> Fixity.t option
  -> Module_path.absolute Type.Scheme.Bounded.t
  -> Extern_name.t
  -> unify:(Type.t -> Type.t -> unit)
  -> t

val add_type_decl : t -> Type_name.t -> Module_path.absolute Type.Decl.t -> t
val add_effect : t -> Effect_name.t -> Effect.t -> unify:(Type.t -> Type.t -> unit) -> t

module Sigs_or_defs : sig
  type name_bindings = t
  type t

  (* TODO: instead of having sets of keys + lookup functions, could we not just return maps? *)
  val value_names : t -> Value_name.Set.t
  val type_names : t -> Type_name.Set.t
  val module_names : t -> Module_name.Set.t
  val find_entry : name_bindings -> t -> Value_name.t -> Name_entry.t

  val find_type_decl
    :  name_bindings
    -> t
    -> Type_name.t
    -> Module_path.absolute Type.Decl.t

  val find_module : name_bindings -> t -> Module_name.t -> t option
end

(* TODO: Maybe this should have a return type more like [Sigs.t option * Defs.t]. This
   would be clearer to use.*)
val find_sigs_and_defs
  :  t
  -> Module_path.Relative.t
  -> Module_name.t
  -> Sigs_or_defs.t option * Sigs_or_defs.t

module For_testing : sig
  val create : names:Name_entry.t Value_name.Map.t -> t
end
