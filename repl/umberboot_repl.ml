open! Core
open Umber

let evaluate_line line =
  (* FIXME: Should parse either a single def or a single expression (not a whole module). *)
  let%bind.Result ast = Parsing.try_parse (Sedlexing.Utf8.from_string line) in
  let%bind.Result names, ast =
    Ast.Typed.Module.of_untyped
      ~names:(force Name_bindings.prelude)
      ~types:(Type_bindings.create ())
      ast
  in
  let%bind.Result mir = Mir.of_typed_module ~names ast in
  let codegen = Codegen.create ~source_filename:"<repl>" in
  let%bind.Result () = Codegen.add_mir codegen mir in
  Codegen.run_jit
;;
