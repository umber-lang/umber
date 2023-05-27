open Import
open Names

module Import = struct
  module Kind = struct
    type t =
      | Absolute
      | Relative of { nth_parent : int (** 0 means relative to current *) }
    [@@deriving sexp_of]

    let of_n_periods = function
      | 0 -> Absolute
      | n -> Relative { nth_parent = n - 1 }
    ;;
  end

  module Paths = struct
    type t =
      | Module of Module_name.t * t Nonempty.t
      | Name of Unidentified_name.t
      | All
      | Name_as of Unidentified_name.t * Unidentified_name.t
    [@@deriving sexp_of]
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
  (* TODO: Consider making [Val] sig-only. *)
  | Val of Value_name.t * Fixity.t option * 'name Type.Scheme.Bounded.t
  | Extern of Value_name.t * Fixity.t option * 'name Type.Scheme.t * Extern_name.t
  | Type_decl of Type_name.t * 'name Type.Decl.t
  (* TODO: [Trait_sig] actually can't appear in defs as it is just parsed as [Trait].
     There should probably be a sig-only type. *)
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
      ; bindings : ('pat * 'expr) Node.t Nonempty.t
      }
  | Trait of
      Trait_name.t
      * Type_param_name.t Nonempty.t
      * 'name sig_ Node.t list
      * ('pat, 'expr, 'name) def Node.t list
  | Impl of
      Trait_bound.t
      * Trait_name.t
      * 'name Type.Scheme.t Nonempty.t
      * ('pat, 'expr, 'name) def Node.t list
[@@deriving sexp_of]

(* TODO: probably move this somewhere else, like Parsing *)
let with_filename (_, sigs, defs) filename =
  let basename = Filename.basename filename in
  let module_name =
    try Filename.split_extension basename |> fst |> Module_name.of_string_lenient_exn with
    | exn ->
      (* TODO: This is a bit silly/hacky. Maybe we should have an interpreter that can
         compile things on the fly instead. *)
      (* Handle stuff like /dev/fd/N for debugging *)
      if String.equal (Filename.dirname filename) "/dev/fd"
      then Module_name.of_string_lenient_exn [%string "Devfd%{basename}"]
      else raise exn
  in
  module_name, sigs, defs
;;

let module_name : _ t -> Module_name.t = Tuple3.get1
