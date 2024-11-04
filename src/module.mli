open! Import
open Names

module Import : sig
  module Kind : sig
    type t =
      | Absolute
      | Relative of { nth_parent : int (** 0 means relative to current *) }
    [@@deriving sexp_of]

    val of_n_periods : int -> t
    val to_n_periods : t -> int
  end

  module Paths : sig
    type t =
      | All
      | Module of Module_name.t * t Nonempty.t
      | Name of Unidentified_name.t
      | Name_as of Unidentified_name.t * Unidentified_name.t
      | Name_excluded of Unidentified_name.t
    [@@deriving compare, sexp_of, variants]
  end

  type t =
    { kind : Kind.t
    ; paths : Paths.t
    }
  [@@deriving sexp_of]
end

type ('pat, 'expr, 'name) t =
  Module_name.t * 'name sig_ Node.t list * ('pat, 'expr, 'name) def Node.t list

and 'name common =
  | Val of Value_name.t * Fixity.t option * 'name Type_scheme.t
  | Extern of Value_name.t * Fixity.t option * 'name Type_scheme.t * Extern_name.t
  | Type_decl of Type_name.t * 'name Type_decl.t
  | Effect of Effect_name.t * 'name Effect.t
  | Trait_sig of Trait_name.t * Type_param_name.t Nonempty.t * 'name sig_ Node.t list
  | Import of Import.t

and 'name sig_ =
  | Common_sig of 'name common
  | Module_sig of Module_name.t * 'name sig_ Node.t list

and ('pat, 'expr, 'name) def =
  | Common_def of 'name common
  | Module of ('pat, 'expr, 'name) t
  | Let of
      { rec_ : bool
      ; bindings : ('pat Node.t * Fixity.t option * 'expr Node.t) Nonempty.t
      }
  | Trait of
      Trait_name.t
      * Type_param_name.t Nonempty.t
      * 'name sig_ Node.t list
      * ('pat, 'expr, 'name) def Node.t list
  | Impl of
      Trait_bound.t
      * Trait_name.t
      * 'name Type_scheme.type_ Nonempty.t
      * ('pat, 'expr, 'name) def Node.t list
[@@deriving sexp_of]

val with_filename : ('pat, 'expr, 'name) t -> string -> ('pat, 'expr, 'name) t
val module_name : _ t -> Module_name.t
