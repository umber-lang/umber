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
    (* TODO: Consider making a ['a Or_underscore.t] type. Then we could rephrase this type
       in terms of that. *)
    type t =
      | All
      | Module of Module_name.t * t Nonempty.t
      | Name of Unidentified_name.t
      | Name_as of Unidentified_name.t * Unidentified_name.t
      | Name_excluded of Unidentified_name.t
    [@@deriving compare, sexp_of, variants]

    (* We need `Name_excluded` to come last in sorted order so that when imports are
       processed in this sorted order, names are always added before being removed.  *)
    let%expect_test "Name_excluded comes last in sorted order" =
      let module_name = Module_name.of_string_exn "ModuleName" in
      let name = Unidentified_name.of_string_exn "name" in
      let examples =
        Variants.fold
          ~init:[]
          ~all:(fun acc variant -> variant.constructor :: acc)
          ~module_:(fun acc variant -> variant.constructor module_name [ All ] :: acc)
          ~name:(fun acc variant -> variant.constructor name :: acc)
          ~name_as:(fun acc variant -> variant.constructor name name :: acc)
          ~name_excluded:(fun acc variant -> variant.constructor name :: acc)
      in
      print_s [%sexp (List.sort examples ~compare : t list)];
      [%expect
        {|
        (All (Module ModuleName (All)) (Name name) (Name_as name name)
         (Name_excluded name)) |}]
    ;;
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
  | Val of Value_name.t * Fixity.t option * 'name Type_scheme.Bounded.t
  | Extern of Value_name.t * Fixity.t option * 'name Type_scheme.t * Extern_name.t
  | Type_decl of Type_name.t * 'name Type_decl.t
  | Effect of Effect_name.t * 'name Effect.t
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
      ; bindings : ('pat Node.t * 'expr Node.t) Nonempty.t
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
