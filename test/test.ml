open Core
open Umber

let concat_current dir = Filename.(concat (concat current_dir_name dir))

(* These tests are currently for parsing only as their related type system features have
   not yet been implemented. *)
(* TODO: enable these tests for type-checking. *)
let parse_only_tests = [ "Imports"; "Modules"; "Operators"; "Traits"; "Types" ]
let should_type_check test = not (List.mem ~equal:String.equal parse_only_tests test)

(* These tests are just for type-checking as they are not ready to be converted to MIR. *)
(* TODO: enable these for mir. *)
let type_only_tests = [ "LetPattern" (* unions in toplevel let bindings *) ]

let should_make_mir test =
  should_type_check test && not (List.mem ~equal:String.equal type_only_tests test)
;;

(* TODO: Implement closures and enable these tests for LLVM codegen. *)
let no_llvm_tests =
  [ "AsPattern"; "Closures"; "MutualRecursion"; "TypeChecking" (* closures *) ]
;;

let should_make_llvm test =
  should_make_mir test && not (List.mem ~equal:String.equal no_llvm_tests test)
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
      , ({ error with filename = Some filename; exn } : Compilation_error.t)]
;;

type target = Umberboot.Target.t * Umberboot.File_or_stdout.t

(* TODO: This should be able to call into umberboot to do what it needs *)
let run_tests () =
  let test filename =
    let bare_filename = Filename.chop_extension filename in
    let out_filename = bare_filename ^ ".out" in
    let tokens_file = concat_current "tokens" out_filename in
    let ast_file = concat_current "ast" out_filename in
    let mir_file = concat_current "mir" out_filename in
    let llvm_file = concat_current "llvm" out_filename in
    List.iter [ tokens_file; ast_file; mir_file; llvm_file ] ~f:(fun file ->
      (* Touch each file so that we always end up with at least an empty for every target,
         even if we error out on an earlier case or otherwise don't generate some outputs. *)
      Out_channel.write_all file ~data:"");
    let in_file = concat_current "examples" filename in
    let base_targets : target list =
      [ Tokens, File tokens_file
      ; ( (if should_type_check bare_filename then Typed_ast else Untyped_ast)
        , File ast_file )
      ]
    in
    let targets =
      List.fold_until
        ~init:base_targets
        [ should_make_mir, (Mir, File mir_file : target)
        ; should_make_llvm, (Llvm, File llvm_file)
        ]
        ~finish:Fn.id
        ~f:(fun targets (should_make, target) ->
          if should_make bare_filename then Continue (target :: targets) else Stop targets)
    in
    Umberboot.compile
      targets
      ~renumber_mir_ids:true
      ~filename:in_file
      ~on_error:(fun stage error ->
      let file =
        match stage with
        | Lexing -> tokens_file
        | Parsing | Type_checking -> ast_file
        | Generating_mir -> mir_file
        | Generating_llvm -> llvm_file
        | Linking -> raise_s [%message "Linking error" (error : Compilation_error.t)]
      in
      Out_channel.with_file file ~f:(fun out ->
        print_compilation_error error ~out ~filename))
  in
  Array.iter (Util.sorted_files_in_local_dir "examples") ~f:(fun file ->
    try test file with
    | exn -> Exn.reraise exn [%string "Error compiling file %{file}"])
;;

let () = run_tests ()
