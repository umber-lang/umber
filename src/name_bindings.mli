open Import
open Names

module Name_entry : sig
  module Type_source : sig
    type t =
      | Val_declared
      | Let_inferred
    [@@deriving equal, sexp]
  end

  type t [@@deriving sexp]

  val typ : t -> Type.t
  val scheme : t -> Type.Scheme.t option
  val type_source : t -> Type_source.t
  val fixity : t -> Fixity.t option
  val extern_name : t -> Extern_name.t option
  val let_inferred : ?fixity:Fixity.t -> ?extern_name:Extern_name.t -> Type.t -> t
end

module Path : sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val to_module_path : t -> Module_path.t
  val append : t -> Module_name.t -> place:[ `Sig | `Def ] -> t
end

type t [@@deriving sexp]

exception Name_error of Ustring.t [@@deriving sexp]

val name_error_msg : string -> Ustring.t -> 'a

(* Handling of default values *)
val empty : t
val core : t
val std_prelude : t Lazy.t
val without_std : t -> t

(* Querying/updating names *)
val find_entry : t -> Value_name.Qualified.t -> Name_entry.t
val find_type : t -> Value_name.Qualified.t -> Type.t
val find_cnstr_type : t -> Cnstr_name.Qualified.t -> Type.t
val find_fixity : t -> Value_name.Qualified.t -> Fixity.t
val set_scheme : t -> Value_name.t -> Type.Scheme.t -> t
val add_name_placeholder : t -> Value_name.t -> t
val add_type_placeholder : t -> Type_name.t -> t

val merge_names
  :  t
  -> Name_entry.t Value_name.Map.t
  -> combine:(Value_name.t -> Name_entry.t -> Name_entry.t -> Name_entry.t)
  -> t

(** Find a type declaration given a qualified name *)
val find_type_decl : t -> Type_name.Qualified.t -> Type.Decl.t

(** Find a type declaration given an absolute qualified name *)
val find_absolute_type_decl : t -> Type_name.Qualified.t -> Type.Decl.t

(** Convert a qualified type name to one with an absolute module path *)
val absolutify_type_name : t -> Type_name.Qualified.t -> Type_name.Qualified.t

(** Convert a qualified value name to one with an absolute module path *)
val absolutify_value_name : t -> Value_name.Qualified.t -> Value_name.Qualified.t

(* Scope handling *)
val current_path : t -> Path.t
val into_module : t -> place:[ `Sig | `Def ] -> Module_name.t -> t
val into_parent : t -> t

(* TODO: maybe the interface should be ~f_sigs ~f_defs ? *)
val with_submodule : t -> place:[ `Sig | `Def ] -> Module_name.t -> f:(t -> t) -> t

val with_submodule'
  :  t
  -> place:[ `Sig | `Def ]
  -> Module_name.t
  -> f:(t -> t * 'a)
  -> t * 'a

val with_path : t -> Path.t -> f:(t -> t * 'a) -> t * 'a

(* AST handling *)
(* TODO: rename the existing module Import to Common, and split into Import.t with:
   `val import : t -> Import.t -> t` *)
val import : t -> Module_name.t -> t
val import_with : t -> Module_path.t -> Unidentified_name.t list -> t
val import_all : t -> Module_path.t -> t
val import_without : t -> Module_path.t -> Unidentified_name.t list -> t

val add_val
  :  ?extern_name:Extern_name.t
  -> t
  -> Value_name.t
  -> Fixity.t option
  -> Type.Expr.Bounded.t
  -> unify:(Type.t -> Type.t -> unit)
  -> t

val add_type_decl : t -> Type_name.t -> Type.Decl.t -> t

module Sigs_or_defs : sig
  type name_bindings = t
  type t

  val value_names : t -> Value_name.Set.t
  val type_names : t -> Type_name.Set.t
  val module_names : t -> Module_name.Set.t
  val find_entry : name_bindings -> t -> Value_name.t -> Name_entry.t
  val find_type_decl : name_bindings -> t -> Type_name.t -> Type.Decl.t
  val find_module : name_bindings -> t -> Module_name.t -> t option
end

val find_sigs_and_defs
  :  t
  -> Module_path.t
  -> Module_name.t
  -> Sigs_or_defs.t option * Sigs_or_defs.t
