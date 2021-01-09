open Import
open Names

type ('pat, 'expr) t = Module_name.t * sig_ list * ('pat, 'expr) def list

and common =
  | Val of Value_name.t * Fixity.t option * Type.Expr.Bounded.t
  | Type_decl of Type_name.t * Type.Decl.t
  | Trait_sig of Trait_name.t * Type.Param.t list * sig_ list
  (* TODO: Allow importing paths all at once
     e.g. `import A.B` instead of `import A with B`
     Related: allow `import A with B.C` or `import A.B.C` instead of multiple imports *)
  | Import of Module_name.t
  | Import_with of Module_path.t * Unidentified_name.t list
  | Import_without of Module_path.t * Unidentified_name.t list

and sig_ =
  | Common_sig of common
  | Module_sig of Module_name.t * sig_ list

and ('pat, 'expr) def =
  | Common_def of common
  | Module of ('pat, 'expr) t
  | Let of 'pat * 'expr
  | Trait of Trait_name.t * Type.Param.t list * sig_ list * ('pat, 'expr) def list
  | Impl of
      Trait_bound.t * Trait_name.t * Type.Param.t Type.Expr.t * ('pat, 'expr) def list
(* TODO: couldn't ^this be simplified with Type.Expr.Bounded.t ? *)
[@@deriving sexp]

let with_filename m filename =
  Tuple3.map_fst m ~f:(fun _ ->
    (* FIXME: this probably breaks for some unicode cases e.g. capitalization 
       and it doesn't handle symbolic characters.
       Should add a proper conversion check to a valid UPPER_NAME *)
    let filename =
      Filename.basename filename |> Filename.split_extension |> fst |> String.capitalize
    in
    Module_name.of_string_exn filename)
;;
