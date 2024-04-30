open! Import
open Names

(* FIXME: Define functions to:
   1) convert from typed ast to untyped ast with annotations
   2) pretty-print the untyped ast back into code (like a formatter would). 
   
   This is basically just implementing a code formatter, but shouldn't be too hard. *)

let typed_ast_to_untyped_annotated_module ((module_name, sigs, defs) : Typed.Module.t)
  : Untyped.Module.t
  =
  (* NOTE: It isn't semantics-preserving to interpret an absolute path as a relative path
     due to shadowing. This is kinda fine for the purposes of pretty-printing the AST
     for test output or error messages. *)
  let relativize_name =
    Tuple2.map_fst ~f:(fun (path : Module_path.Absolute.t) ->
      Module_path.Relative.of_module_names (path :> Module_name.t list))
  in
  let relativize_type type_ =
    Type_scheme.map type_ ~type_name:relativize_name ~effect_name:relativize_name
  in
  let relativize_type' type_ =
    Type_scheme.map' type_ ~type_name:relativize_name ~effect_name:relativize_name
  in
  let rec relativize_pattern : Typed.Pattern.t -> Untyped.Pattern.t = function
    | (Constant _ | Catch_all _) as pattern -> pattern
    | As (pattern, name) -> As (relativize_pattern pattern, name)
    | Cnstr_appl (cnstr, args) ->
      Cnstr_appl (relativize_name cnstr, List.map args ~f:relativize_pattern)
    | Tuple fields -> Tuple (List.map fields ~f:relativize_pattern)
    | Record fields ->
      Record
        (Nonempty.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:relativize_pattern)))
    | Union (left, right) -> Union (relativize_pattern left, relativize_pattern right)
    | Type_annotation _ -> .
  in
  let rec handle_common
    : Module_path.absolute Module.common -> Module_path.relative Module.common
    = function
    | Val (name, fixity, type_) -> Val (name, fixity, relativize_type' type_)
    | Extern (name, fixity, type_, extern_name) ->
      Extern (name, fixity, relativize_type' type_, extern_name)
    | Type_decl (type_name, decl) ->
      Type_decl (type_name, Type_decl.map_exprs decl ~f:relativize_type)
    | Effect (effect_name, effect) ->
      Effect (effect_name, Effect.map_exprs effect ~f:relativize_type)
    | Trait_sig (trait_name, params, sigs) ->
      Trait_sig (trait_name, params, List.map sigs ~f:(Node.map ~f:handle_sig))
    | Import import -> Import import
  and handle_sig : Module_path.absolute Module.sig_ -> Module_path.relative Module.sig_
    = function
    | Common_sig common -> Common_sig (handle_common common)
    | Module_sig (module_name, sigs) ->
      Module_sig (module_name, List.map sigs ~f:(Node.map ~f:handle_sig))
  and handle_def : Typed.Module.def -> Untyped.Module.def = function
    | Common_def common -> Common_def (handle_common common)
    | Let { rec_; bindings } ->
      Let
        { rec_
        ; bindings =
            Nonempty.map bindings ~f:(fun (pat, expr) ->
              Node.map pat ~f:relativize_pattern, Node.map expr ~f:handle_expr)
        }
    | Module (module_name, sigs, defs) ->
      Module
        ( module_name
        , List.map sigs ~f:(Node.map ~f:handle_sig)
        , List.map defs ~f:(Node.map ~f:handle_def) )
    | Trait (trait_name, params, sigs, defs) ->
      Trait
        ( trait_name
        , params
        , List.map sigs ~f:(Node.map ~f:handle_sig)
        , List.map defs ~f:(Node.map ~f:handle_def) )
    | Impl (trait_bound, trait_name, args, defs) ->
      Impl
        ( trait_bound
        , trait_name
        , Nonempty.map args ~f:relativize_type
        , List.map defs ~f:(Node.map ~f:handle_def) )
  in
  ( module_name
  , List.map sigs ~f:(Node.map ~f:handle_sig)
  , List.map defs ~f:(Node.map ~f:handle_def) )
;;
