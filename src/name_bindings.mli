open Names

module Name_entry : sig
  module Type_source : sig
    type t =
      | Placeholder
      | Val_declared
      | Let_inferred
    [@@deriving sexp]
  end

  module Type_or_scheme : sig
    type t =
      | Type of Type.t
      | Scheme of Type.Scheme.t
    [@@deriving sexp]
  end

  type t =
    { typ : Type_or_scheme.t
    ; type_source : Type_source.t
    ; fixity : Fixity.t option
    ; extern_name : Extern_name.t option
    }
  [@@deriving sexp]

  val typ : t -> Type.t
  val let_inferred : ?fixity:Fixity.t -> ?extern_name:Extern_name.t -> Type.t -> t
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
val set_scheme : t -> place:[ `Sig | `Def ] -> Value_name.t -> Type.Scheme.t -> t
val add_fresh_var : t -> place:[ `Sig | `Def ] -> Value_name.t -> t * Type.t
val add_type_placeholder : t -> place:[ `Sig | `Def ] -> Type_name.t -> t

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
val current_path : t -> Module_path.t
val into_module : t -> place:[ `Sig | `Def ] -> Module_name.t -> t
val into_parent : t -> t
val with_submodule : t -> place:[ `Sig | `Def ] -> Module_name.t -> f:(t -> t) -> t

val with_submodule'
  :  t
  -> place:[ `Sig | `Def ]
  -> Module_name.t
  -> f:(t -> t * 'a)
  -> t * 'a

val with_path : t -> Module_path.t -> f:(t -> t * 'a) -> t * 'a

(* AST handling *)
(* TODO: rename the existing module Import to Common, and split into Import.t with:
   `val import : t -> Import.t -> t` *)
val import : t -> place:[ `Sig | `Def ] -> Module_name.t -> t

val import_with
  :  t
  -> place:[ `Sig | `Def ]
  -> Module_path.t
  -> Unidentified_name.t list
  -> t

val import_all : t -> place:[ `Sig | `Def ] -> Module_path.t -> t

val import_without
  :  t
  -> place:[ `Sig | `Def ]
  -> Module_path.t
  -> Unidentified_name.t list
  -> t

val add_val
  :  ?extern_name:Extern_name.t
  -> t
  -> place:[ `Sig | `Def ]
  -> Value_name.t
  -> Fixity.t option
  -> Type.Expr.Bounded.t
  -> unify:(Type.t -> Type.t -> unit)
  -> t

val add_type_decl : t -> place:[ `Sig | `Def ] -> Type_name.t -> Type.Decl.t -> t
