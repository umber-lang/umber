open Core
open Umber

module Stage = struct
  module T = struct
    type t =
      | Lexing
      | Parsing
      | Type_checking
      | Generating_mir
      | Generating_asm
      | Generating_llvm
      | Linking
    [@@deriving compare, variants, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Target = struct
  module T = struct
    type t =
      | Tokens
      | Untyped_ast
      | Typed_ast
      | Type_annotated_code
      | Names
      | Mir
      | Llvm
      | Asm
      | Exe
    [@@deriving compare, variants, sexp]
  end

  include T
  include Comparable.Make (T)

  let stage : t -> Stage.t = function
    | Tokens -> Lexing
    | Untyped_ast -> Parsing
    | Typed_ast | Type_annotated_code | Names -> Type_checking
    | Mir -> Generating_mir
    | Llvm -> Generating_llvm
    | Asm -> Generating_asm
    | Exe -> Linking
  ;;
end

module File_or_stdout = struct
  type t =
    | File of Filename.t
    | Stdout

  let with_out_channel t ~f =
    match t with
    | File filename -> Out_channel.with_file filename ~f
    | Stdout -> f stdout
  ;;
end

module Output : sig
  type t

  val create : (Target.t * File_or_stdout.t) list -> t
  val requires : t -> Stage.t -> bool
  val find_target : t -> Target.t -> File_or_stdout.t option
  val is_empty : t -> bool
  val param : t Command.Param.t
end = struct
  type t =
    { targets : File_or_stdout.t Target.Map.t
    ; max_stage : Stage.t option
    }

  let empty = { targets = Target.Map.empty; max_stage = None }
  let is_empty t = Map.is_empty t.targets

  let requires t stage =
    match t.max_stage with
    | Some max_stage -> Stage.(stage <= max_stage)
    | None -> false
  ;;

  let find_target t = Map.find t.targets

  let add_target { targets; max_stage } target file_or_stdout =
    let targets =
      match Map.add targets ~key:target ~data:file_or_stdout with
      | `Ok targets -> targets
      | `Duplicate -> raise_s [%message "Duplicate target" (target : Target.t)]
    in
    let max_stage =
      match max_stage with
      | Some max_stage -> Some (Stage.max max_stage (Target.stage target))
      | None -> Some (Target.stage target)
    in
    { targets; max_stage }
  ;;

  let create =
    List.fold ~init:empty ~f:(fun t (target, file_or_stdout) ->
      add_target t target file_or_stdout)
  ;;

  let param =
    let%map_open.Command tokens = flag "tokens" no_arg ~doc:"Print lexer output (tokens)"
    and untyped_ast = flag "untyped-ast" no_arg ~doc:"Print parser output (untyped AST)"
    and typed_ast = flag "typed-ast" no_arg ~doc:"Print type-checker output (typed AST)"
    and type_annotated_code =
      flag "type-annotated-code" no_arg ~doc:"Pretty-print type-annotated code"
    and names = flag "names" no_arg ~doc:"Print name-resolver output (name bindings)"
    and mir = flag "mir" no_arg ~doc:"Print mid-level IR statements (MIR)"
    and llvm = flag "llvm" no_arg ~doc:"Print LLVM IR"
    and asm = flag "asm" no_arg ~doc:"Print x86-64 assembly"
    and exe_filename =
      flag
        "exe"
        (optional Filename_unix.arg_type)
        ~doc:"FILENAME Output a compiled executable"
    in
    let add present t (variant : Target.t Variant.t) =
      if present then add_target t variant.constructor Stdout else t
    in
    Target.Variants.fold
      ~init:empty
      ~tokens:(add tokens)
      ~untyped_ast:(add untyped_ast)
      ~typed_ast:(add typed_ast)
      ~type_annotated_code:(add type_annotated_code)
      ~names:(add names)
      ~mir:(add mir)
      ~llvm:(add llvm)
      ~asm:(add asm)
      ~exe:(fun t variant ->
      match exe_filename with
      | Some exe_filename -> add_target t variant.constructor (File exe_filename)
      | None -> t)
  ;;
end

let with_tmpdir f =
  let dir = Filename_unix.temp_dir "umberboot" "" in
  Exn.protectx ~f dir ~finally:(Shell.rm ~r:() ~f:())
;;

let compile_internal ~filename ~output ~no_std ~parent ~on_error =
  if Output.is_empty output then failwith "No output arguments supplied";
  let run_stage acc (variant : Stage.t Variant.t) ~f =
    Result.bind acc ~f:(fun acc ->
      let stage = variant.constructor in
      if Output.requires output stage
      then Result.map_error (f acc) ~f:(fun error -> `Compilation_error (stage, error))
      else Error `Done)
  in
  let maybe_output target ~f =
    Option.iter (Output.find_target output target) ~f:(fun file_or_stdout ->
      File_or_stdout.with_out_channel file_or_stdout ~f)
  in
  let result =
    Stage.Variants.fold
      ~init:(Ok ())
      ~lexing:
        (run_stage ~f:(fun () ->
           (* Parsing and lexing are done together, so only lex separately if not parsing. *)
           if not (Output.requires output Parsing)
           then
             maybe_output Tokens ~f:(fun print_tokens_to ->
               Parsing.lex_file filename ~print_tokens_to
               |> Result.iter_error ~f:(fun error ->
                    print_s [%sexp (error : Compilation_error.t)]));
           Ok ()))
      ~parsing:
        (run_stage ~f:(fun () ->
           let get_ast print_tokens_to = Parsing.parse_file ?print_tokens_to filename in
           let%map.Result ast =
             match Output.find_target output Tokens with
             | None -> get_ast None
             | Some print_tokens_to ->
               File_or_stdout.with_out_channel print_tokens_to ~f:(fun print_tokens_to ->
                 get_ast (Some print_tokens_to))
           in
           maybe_output Untyped_ast ~f:(fun out ->
             Parsing.fprint_s [%sexp (ast : Ast.Untyped.Module.t)] ~out);
           ast))
      ~type_checking:
        (run_stage ~f:(fun untyped_ast ->
           let type_check untyped_ast =
             let names =
               if no_std then Name_bindings.core else force Name_bindings.prelude
             in
             let names =
               match parent with
               | Some module_name ->
                 Name_bindings.into_module names module_name ~place:`Def
               | None -> names
             in
             Ast.Typed.Module.of_untyped ~names ~include_std:(not no_std) untyped_ast
           in
           let%map.Result names, typed_ast = type_check untyped_ast in
           maybe_output Typed_ast ~f:(fun out ->
             Parsing.fprint_s [%sexp (typed_ast : Ast.Typed.Module.t)] ~out);
           maybe_output Type_annotated_code ~f:(fun out ->
             let formatted_code =
               let buffer = Buffer.create 100 in
               Pretty_ast.Typed_to_untyped.convert_module ~names typed_ast
               |> Pretty_ast.format
               |> Sequence.iter ~f:(Buffer.add_string buffer);
               Buffer.add_char buffer '\n';
               Buffer.contents buffer
             in
             Out_channel.output_string out formatted_code;
             let lexbuf = Sedlexing.Utf8.from_string formatted_code in
             Sedlexing.set_filename lexbuf filename;
             let roundtrip_result =
               match Parsing.try_parse lexbuf with
               | Error error ->
                 Error
                   [%message
                     "BUG: Failed to parse pretty-printed code"
                       (error : Compilation_error.t)]
               | Ok untyped_ast' ->
                 (match type_check untyped_ast' with
                  | Ok _ -> Ok ()
                  | Error error ->
                    Error
                      [%message
                        "BUG: Failed to type-check annotated code on roundtrip"
                          (error : Compilation_error.t)])
             in
             Result.iter_error roundtrip_result ~f:(Parsing.fprint_s ~out));
           maybe_output Names ~f:(fun out ->
             Parsing.fprint_s [%sexp (names : Name_bindings.t)] ~out);
           typed_ast, names))
      ~generating_mir:
        (run_stage ~f:(fun (ast, names) ->
           let%map.Result mir = Mir.of_typed_module ~names ast in
           maybe_output Mir ~f:(fun out -> Parsing.fprint_s [%sexp (mir : Mir.t)] ~out);
           let module_path =
             Ast.Module_path.of_module_names_unchecked
               (Option.to_list parent @ [ Ast.Module.module_name ast ])
           in
           module_path, mir))
      ~generating_asm:
        (run_stage ~f:(fun (module_path, mir) ->
           let asm = Asm_codegen.convert_mir ~module_path mir in
           maybe_output Asm ~f:(fun out ->
             Asm_program.pp (Stdlib.Format.formatter_of_out_channel out) asm);
           Ok (module_path, mir, asm)))
      ~generating_llvm:
        (run_stage ~f:(fun (module_path, mir, asm) ->
           Codegen.of_mir ~module_path ~source_filename:filename mir
           |> Result.iter ~f:(fun codegen ->
                maybe_output Llvm ~f:(fun out ->
                  fprintf out "%s\n" (Codegen.to_string codegen)));
           Ok (module_path, asm)))
      ~linking:
        (run_stage ~f:(fun (module_path, asm) ->
           match Output.find_target output Exe with
           | None -> Ok ()
           | Some Stdout ->
             Error
               (Compilation_error.create
                  ~filename
                  ~msg:[%sexp "Can't write executable file to stdout"]
                  Other)
           | Some (File output_exe) ->
             let module_name =
               Ast.Module_path.last_exn module_path
               |> Ast.Module_name.to_ustring
               |> Ustring.to_string
             in
             with_tmpdir (fun tmpdir ->
               let object_file = tmpdir ^/ module_name ^ ".o" in
               let entry_file = tmpdir ^/ "_entry.o" in
               Asm_codegen.compile_to_object_file asm ~output_file:object_file;
               (* TODO: Hard-coding the Prelude module path here is a bit janky. We should
                  come up with something better when implementing linking arbitrary files. *)
               let prelude_module_path =
                 Ast.Module_path.of_module_names_unchecked
                   [ Ast.Module_name.of_string_unchecked "Std"
                   ; Ast.Module_name.of_string_unchecked "Prelude"
                   ]
               in
               Asm_codegen.compile_entry_module
                 ~module_paths:[ prelude_module_path; module_path ]
                 ~entry_file;
               Linking.link_with_std_and_runtime
                 ~object_files:[ object_file; entry_file ]
                 ~output_exe)))
  in
  match result with
  | Ok () | Error `Done -> ()
  | Error (`Compilation_error (stage, error)) -> on_error stage error
;;

let on_error_raise ~filename stage error =
  raise_s
    [%message
      "Compilation failed"
        (filename : Filename.t)
        (stage : Stage.t)
        ~error:
          ({ error with backtrace = Some (Backtrace.Exn.most_recent ()) }
            : Compilation_error.t)]
;;

let compile ?(no_std = false) ?parent ?on_error ~filename targets =
  let on_error = Option.value on_error ~default:(on_error_raise ~filename) in
  compile_internal ~no_std ~parent ~on_error ~filename ~output:(Output.create targets)
;;

let command =
  Command.basic
    ~summary:"Compile an Umber file"
    (let%map_open.Command filename = anon ("filename" %: Filename_unix.arg_type)
     and output = Output.param
     and no_std = flag "no-std" no_arg ~doc:"Don't include the standard library"
     and parent =
       flag
         "parent"
         (optional Ast.Module_name.arg_type_lenient)
         ~doc:"MODULE_NAME The name of the parent module"
     in
     fun () ->
       compile_internal
         ~filename
         ~output
         ~no_std
         ~parent
         ~on_error:(on_error_raise ~filename))
;;
