open! Import
open Names

type ('pat, 'expr) t = Module_name.t * sig_ Node.t list * ('pat, 'expr) def Node.t list

and common =
  | Val of Value_name.t * Fixity.t option * Type.Scheme.Bounded.t
  | Extern of Value_name.t * Fixity.t option * Type.Scheme.t * Extern_name.t
  | Type_decl of Type_name.t * Type.Decl.t
  | Effect of Effect_name.t * Effect.t
  | Trait_sig of Trait_name.t * Type_param_name.t Nonempty.t * sig_ Node.t list
  | Import of Module_name.t
  | Import_with of Module_path.t * Unidentified_name.t list
  | Import_without of Module_path.t * Unidentified_name.t Nonempty.t

and sig_ =
  | Common_sig of common
  | Module_sig of Module_name.t * sig_ Node.t list

and ('pat, 'expr) def =
  | Common_def of common
  | Module of ('pat, 'expr) t
  | Let of
      { rec_ : bool
      ; bindings : ('pat * 'expr) Node.t Nonempty.t
      }
  | Trait of
      Trait_name.t
      * Type_param_name.t Nonempty.t
      * sig_ Node.t list
      * ('pat, 'expr) def Node.t list
  | Impl of
      Trait_bound.t
      * Trait_name.t
      * Type.Scheme.t Nonempty.t
      * ('pat, 'expr) def Node.t list
[@@deriving sexp_of]

val with_filename : ('pat, 'expr) t -> string -> ('pat, 'expr) t
val module_name : _ t -> Module_name.t
