open Core
open Umber

(* TODO: Could make Stage/Target GADTS and put the functions inside them. I think GADTs
   will probably ruin everything but I am kinda sad you still have to think about target/
   stage dependencies a lot when writing this command. *)

module Stage = struct
  module T = struct
    (** Stages of compilation. The order is significant; later stages depend on earlier
        stages. *)
    type t =
      | Lexing
      | Parsing
      | Type_checking
      | Generating_mir
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
      | Names
      | Mir
      | Llvm
      | Exe
    [@@deriving compare, variants, sexp]
  end

  include T
  include Comparable.Make (T)

  let stage : t -> Stage.t = function
    | Tokens -> Lexing
    | Untyped_ast -> Parsing
    | Typed_ast | Names -> Type_checking
    | Mir -> Generating_mir
    | Llvm -> Generating_llvm
    | Exe -> Linking
  ;;
end

module Output : sig
  type t

  val param : t Command.Param.t
  val of_targets : Target.t list -> t
  val requires : t -> Stage.t -> bool
  val targets : t -> Target.t -> bool
  val exe_filename_exn : t -> Filename.t
  val is_empty : t -> bool
end = struct
  type t =
    { targets : Target.Set.t
    ; max_stage : Stage.t option
    ; exe_filename : Filename.t option
    }

  let empty = { targets = Target.Set.empty; max_stage = None; exe_filename = None }
  let is_empty t = Set.is_empty t.targets

  let requires t stage =
    match t.max_stage with
    | Some max_stage -> Stage.(stage <= max_stage)
    | None -> false
  ;;

  let targets t = Set.mem t.targets

  let exe_filename_exn t =
    Option.value_exn t.exe_filename ~message:"Output.exe_filename missing"
  ;;

  let add_target { targets; max_stage; exe_filename } target =
    { targets = Set.add targets target
    ; max_stage =
        (match max_stage with
         | Some max_stage -> Some (Stage.max max_stage (Target.stage target))
         | None -> Some (Target.stage target))
    ; exe_filename
    }
  ;;

  let of_targets = List.fold ~init:empty ~f:add_target

  let param =
    let%map_open.Command tokens = flag "tokens" no_arg ~doc:"Print lexer output (tokens)"
    and untyped_ast = flag "untyped-ast" no_arg ~doc:"Print parser output (untyped AST)"
    and typed_ast = flag "typed-ast" no_arg ~doc:"Print type-checker output (typed AST)"
    and names = flag "names" no_arg ~doc:"Print name-resolver output (name bindings)"
    and mir = flag "mir" no_arg ~doc:"Print mid-level IR statements (MIR)"
    and llvm = flag "llvm" no_arg ~doc:"Print LLVM IR"
    and exe_filename =
      flag "exe" (optional Filename_unix.arg_type) ~doc:"Output a compiled executable"
    in
    let add present t (variant : Target.t Variant.t) =
      if present then add_target t variant.constructor else t
    in
    Target.Variants.fold
      ~init:empty
      ~tokens:(add tokens)
      ~untyped_ast:(add untyped_ast)
      ~typed_ast:(add typed_ast)
      ~names:(add names)
      ~mir:(add mir)
      ~llvm:(add llvm)
      ~exe:(fun t variant ->
      match exe_filename with
      | None -> t
      | Some exe_filename ->
        let t = add_target t variant.constructor in
        { t with exe_filename = Some exe_filename })
  ;;
end

let rec rmrf path =
  if Sys_unix.is_directory_exn path
  then (
    Sys_unix.readdir path |> Array.iter ~f:(fun name -> rmrf (path ^/ name));
    Core_unix.remove path)
  else Sys_unix.remove path
;;

let with_tmpdir f =
  let dir = Filename_unix.temp_dir "umberboot" "" in
  Exn.protectx ~f dir ~finally:rmrf
;;

let or_raise = function
  | Ok x -> x
  | Error err -> raise_s [%sexp (err : Compilation_error.t)]
;;

let compile_and_print_internal ~filename ~output ~no_std ~parent =
  if Output.is_empty output then failwith "No output arguments supplied";
  let run_stage acc (variant : Stage.t Variant.t) ~f =
    Option.bind acc ~f:(fun acc ->
      if Output.requires output variant.constructor then Some (f acc) else None)
  in
  Stage.Variants.fold
    ~init:(Some ())
    ~lexing:
      (run_stage ~f:(fun () ->
         (* Parsing and lexing are done together, so only lex separately if not parsing. *)
         if not (Output.requires output Parsing)
         then
           Parsing.lex_file filename ~print_tokens_to:stdout
           |> Result.iter_error ~f:(fun error ->
                print_s [%sexp (error : Compilation_error.t)])))
    ~parsing:
      (run_stage ~f:(fun () ->
         let print_tokens_to = Option.some_if (Output.targets output Tokens) stdout in
         let ast = or_raise (Parsing.parse_file ?print_tokens_to filename) in
         if Output.targets output Untyped_ast
         then print_s [%sexp (ast : Ast.Untyped.Module.t)];
         ast))
    ~type_checking:
      (run_stage ~f:(fun ast ->
         let names =
           if no_std
           then Name_bindings.core
           else Name_bindings.of_prelude_sexp Umber_std.Prelude.names
         in
         let names =
           match parent with
           | Some module_name -> Name_bindings.into_module names module_name ~place:`Def
           | None -> names
         in
         let names, ast =
           or_raise
             (Ast.Typed.Module.of_untyped ~names ~types:(Type_bindings.create ()) ast)
         in
         if Output.targets output Typed_ast
         then print_s [%sexp (ast : Ast.Typed.Module.t)];
         if Output.targets output Names then print_s [%sexp (names : Name_bindings.t)];
         ast, names))
    ~generating_mir:
      (run_stage ~f:(fun (ast, names) ->
         let mir = or_raise (Mir.of_typed_module ~names ast) in
         if Output.targets output Mir then print_s [%sexp (mir : Mir.t)];
         Ast.Module.module_name ast, mir))
    ~generating_llvm:
      (run_stage ~f:(fun (module_name, mir) ->
         let codegen = Codegen.of_mir_exn ~source_filename:filename mir in
         if Output.targets output Llvm then print_endline (Codegen.to_string codegen);
         module_name, codegen))
    ~linking:
      (run_stage ~f:(fun (module_name, codegen) ->
         let module_name = Ast.Module_name.to_ustring module_name |> Ustring.to_string in
         with_tmpdir (fun tmpdir ->
           let object_file = tmpdir ^/ module_name ^ ".o" in
           Codegen.compile_to_object codegen ~output_file:object_file;
           let output_exe = Output.exe_filename_exn output in
           Linking.link_with_runtime ~object_file ~output_exe)))
  |> Option.iter ~f:(fun () -> ())
;;

let compile_and_print ?(no_std = false) ?parent ~filename targets =
  compile_and_print_internal ~no_std ~parent ~filename ~output:(Output.of_targets targets)
;;

let command =
  Command.basic
    ~summary:"Umberboot is a compiler for Umber written in OCaml."
    (let%map_open.Command () = return ()
     and filename = anon ("filename" %: Filename_unix.arg_type)
     and output = Output.param
     and no_std = flag "no-std" no_arg ~doc:"Don't include the standard library"
     and parent =
       flag
         "parent"
         (optional Ast.Module_name.arg_type_lenient)
         ~doc:"MODULE_NAME The name of the parent module"
     in
     fun () -> compile_and_print_internal ~filename ~output ~no_std ~parent)
;;
