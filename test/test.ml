open Core
open Umber

let concat_current dir = Filename.(concat (concat current_dir_name dir))

(* These tests are currently for parsing only as their related type system features have
   not yet been implemented. *)
(* TODO: enable these tests for type-checking. *)
let parse_only_tests =
  [ "Modules" (* traits *); "Traits" (* traits *); "Types" (* records *) ]
;;

let should_type_check test = not (List.mem ~equal:String.equal parse_only_tests test)

(* These tests are just for type-checking as they are not ready to be converted to MIR. *)
(* TODO: enable these for mir. *)
let type_only_tests =
  [ "LetPattern" (* unions in toplevel let bindings *)
  ; "Effects" (* effects *)
  ; "ReraiseExn" (* effects *)
  ; "State" (* effects *)
  ; "Iter" (* effects *)
  ]
;;

let should_make_mir test =
  should_type_check test && not (List.mem ~equal:String.equal type_only_tests test)
;;

let should_make_llvm test = should_make_mir test

let no_asm_tests =
  [ "AdventOfCode2024_1" (* TODO: Hanging, most likely register allocation too slow. *) ]
;;

let should_make_asm test =
  should_make_mir test && not (List.mem ~equal:String.equal no_asm_tests test)
;;

let no_exe_tests = []

let should_make_exe test =
  should_make_asm test && not (List.mem ~equal:String.equal no_exe_tests test)
;;

let print_compilation_error ~out ~filename (error : Compilation_error.t) =
  let exn =
    match error.kind with
    | Codegen_error | Other -> error.exn
    | _ -> None
  in
  Parsing.fprint_s
    ~out
    [%sexp
      "Compilation error"
      , ({ error with filename = Some filename; exn; backtrace = None }
          : Compilation_error.t)]
;;

type target = Umberboot.Target.t * Umberboot.File_or_stdout.t

let test ~in_file =
  let filename = Filename.basename in_file in
  let bare_filename = Filename.chop_extension filename in
  let out_filename = bare_filename ^ ".out" in
  let tokens_file = concat_current "tokens" out_filename in
  let ast_file = concat_current "ast" out_filename in
  let mir_file = concat_current "mir" out_filename in
  let asm_file = concat_current "asm" out_filename in
  let llvm_file = concat_current "llvm" out_filename in
  let output_file = concat_current "output" out_filename in
  let input_file = concat_current "input" (bare_filename ^ ".txt") in
  let tmp_exe_file = Filename_unix.temp_file ("umber_test." ^ filename) "" in
  List.iter
    [ tokens_file; ast_file; mir_file; asm_file; llvm_file; output_file ]
    ~f:(fun file ->
    (* Touch each file so that we always end up with at least an empty file for every
       target, even if we error out on an earlier case or otherwise don't generate some
       outputs. *)
    Out_channel.write_all file ~data:"");
  let base_targets : target list =
    [ Tokens, File tokens_file
    ; ( (if should_type_check bare_filename then Type_annotated_code else Untyped_ast)
      , File ast_file )
    ]
  in
  let targets =
    List.fold_until
      ~init:base_targets
      [ should_make_mir, (Mir, File mir_file : target)
      ; should_make_llvm, (Llvm, File llvm_file)
      ; should_make_asm, (Asm, File asm_file)
      ; should_make_exe, (Exe, File tmp_exe_file)
      ]
      ~finish:Fn.id
      ~f:(fun targets (should_make, target) ->
        if should_make bare_filename then Continue (target :: targets) else Stop targets)
  in
  let encountered_error = ref false in
  Umberboot.compile targets ~filename:in_file ~on_error:(fun stage error ->
    encountered_error := true;
    let file =
      match stage with
      | Lexing -> tokens_file
      | Parsing | Type_checking -> ast_file
      | Generating_mir -> mir_file
      | Generating_llvm -> llvm_file
      | Generating_asm -> asm_file
      | Linking -> output_file
    in
    Out_channel.with_file file ~f:(fun out ->
      print_compilation_error error ~out ~filename));
  if (not !encountered_error) && should_make_exe bare_filename
  then (
    let redirected_stdin =
      if Sys_unix.file_exists_exn input_file then " < " ^ input_file else ""
    in
    Shell.sh "%s > %s 2>&1%s" tmp_exe_file output_file redirected_stdin)
;;

let command =
  Command.basic
    ~summary:"Run tests for a file"
    (let%map_open.Command in_file = anon ("file" %: Filename_unix.arg_type) in
     fun () ->
       try test ~in_file with
       | exn -> Exn.reraise exn [%string "Error compiling file %{in_file}"])
;;

let () = Command_unix.run command
