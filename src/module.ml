open Import
open Names

type ('pat, 'expr) t = Module_name.t * sig_ Node.t list * ('pat, 'expr) def Node.t list

and common =
  | Val of Value_name.t * Fixity.t option * Type.Expr.Bounded.t
  | Type_decl of Type_name.t * Type.Decl.t
  | Trait_sig of Trait_name.t * Type.Param.t list * sig_ Node.t list
  (* TODO: Allow importing paths all at once
     e.g. `import A.B` instead of `import A with B`
     Related: allow `import A with B.C` or `import A.B.C` instead of multiple imports *)
  | Import of Module_name.t
  | Import_with of Module_path.t * Unidentified_name.t list
  | Import_without of Module_path.t * Unidentified_name.t list

and sig_ =
  | Common_sig of common
  | Module_sig of Module_name.t * sig_ Node.t list

and ('pat, 'expr) def =
  | Common_def of common
  | Module of ('pat, 'expr) t
  | Let of ('pat * 'expr) list
  | Trait of
      Trait_name.t * Type.Param.t list * sig_ Node.t list * ('pat, 'expr) def Node.t list
  | Impl of
      Trait_bound.t
      * Trait_name.t
      * Type.Param.t Type.Expr.t
      * ('pat, 'expr) def Node.t list
(* TODO: couldn't ^this be simplified with Type.Expr.Bounded.t ? *)
[@@deriving sexp]

(* TODO: probably move this somewhere else, like Parsing *)
let with_filename (_, sigs, defs) filename =
  let module_name =
    Filename.basename filename
    |> Filename.split_extension
    |> fst
    |> Module_name.of_string_lenient_exn
  in
  module_name, sigs, defs
;;

let sigs_defs_span span (sigs_spans, defs_spans) =
  let sigs = List.map ~f:fst sigs_spans in
  let defs, def_spans = List.unzip defs_spans in
  sigs, defs, Span.first_to_last span def_spans
;;

let common_def (common, span) = { Node.node = Common_def common; span }

let module_ span name body =
  let sigs, defs, span = sigs_defs_span span body in
  Module (Module_name.of_ustring_unchecked name, sigs, defs), span
;;

let let_ span bindings_spans =
  let bindings, spans = List.unzip bindings_spans in
  Let bindings, Span.first_to_last span spans
;;

let trait span name params_spans body =
  let sigs, defs, span = sigs_defs_span span body in
  Trait (Trait_name.of_ustring_unchecked name, params, fst body, snd body), span
;;

let impl span (bound, _) name typ defs =
  let defs, spans = List.unzip defs in
  let span = Span.first_to_last span spans in
  Impl (bound, Trait_name.of_ustring_unchecked name, typ, defs), span
;;
