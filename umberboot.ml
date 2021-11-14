open Core
open Umber

(* TODO: Could make Stage/Target GADTS and put the functions inside them *)

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
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Target = struct
  module T = struct
    (** Targets for the compiler to output. Each require up to a certain compilation stage
        to be completed. *)
    type t =
      | Tokens
      | Untyped_ast
      | Typed_ast
      | Names
      | Mir
      | Templates
      | Llvm
    [@@deriving compare, enumerate, sexp]
  end

  include T
  include Comparable.Make (T)

  let stage : t -> Stage.t = function
    | Tokens -> Lexing
    | Untyped_ast -> Parsing
    | Typed_ast | Names -> Type_checking
    | Mir | Templates -> Generating_mir
    | Llvm -> Generating_llvm
  ;;

  let param =
    let open Command.Param in
    function
    | Tokens -> flag "tokens" no_arg ~doc:"Print lexer output (tokens)"
    | Untyped_ast -> flag "untyped-ast" no_arg ~doc:"Print parser output (untyped AST)"
    | Typed_ast -> flag "typed-ast" no_arg ~doc:"Print type-checker output (typed AST)"
    | Names -> flag "names" no_arg ~doc:"Print name-resolver output (name bindings)"
    | Mir -> flag "mir" no_arg ~doc:"Print mid-level IR (MIR)"
    | Templates -> flag "templates" no_arg ~doc:"Print polymorphic function templates"
    | Llvm -> flag "llvm" no_arg ~doc:"Print LLVM IR"
  ;;
end

module Output : sig
  type t

  val param : t Command.Param.t
  val requires : t -> Stage.t -> bool
  val targets : t -> Target.t -> bool
end = struct
  type t =
    { targets : Target.Set.t
    ; max_stage : Stage.t option
    }

  let empty = { targets = Target.Set.empty; max_stage = None }

  let requires t stage =
    match t.max_stage with
    | Some max_stage -> Stage.(stage <= max_stage)
    | None -> false
  ;;

  let targets t = Set.mem t.targets

  let param =
    List.fold Target.all ~init:(Command.Param.return empty) ~f:(fun param target ->
      Command.Param.map2
        param
        (Target.param target)
        ~f:(fun ({ targets; max_stage } as t) target_present ->
        if target_present
        then
          { targets = Set.add targets target
          ; max_stage =
              (match max_stage with
              | Some max_stage -> Some (Stage.max max_stage (Target.stage target))
              | None -> Some (Target.stage target))
          }
        else t))
  ;;
end

let command =
  Command.basic
    ~summary:"Umberboot is a compiler for Umber written in OCaml."
    (let%map_open.Command () = return ()
     and filename = anon ("filename" %: Filename.arg_type)
     and output = Output.param
     and no_std = flag "no-std" no_arg ~doc:"Don't include the standard library"
     and parent =
       flag
         "parent"
         (optional Ast.Module_name.arg_type_lenient)
         ~doc:"MODULE_NAME The name of the parent module"
     in
     fun () ->
       let or_raise = function
         | Ok x -> x
         | Error err -> raise_s [%sexp (err : Compilation_error.t)]
       in
       if Output.requires output Parsing
       then (
         let print_tokens_to = Option.some_if (Output.targets output Tokens) stdout in
         let ast = or_raise (Parsing.parse_file ?print_tokens_to filename) in
         if Output.targets output Untyped_ast
         then print_s [%sexp (ast : Ast.Untyped.Module.t)];
         if Output.requires output Type_checking
         then (
           let names =
             if no_std then Name_bindings.core else Lazy.force Name_bindings.std_prelude
           in
           let names =
             match parent with
             | Some module_name -> Name_bindings.into_module names module_name ~place:`Def
             | None -> names
           in
           let names, ast = or_raise (Ast.Typed.Module.of_untyped ~names ast) in
           if Output.targets output Typed_ast
           then print_s [%sexp (ast : Ast.Typed.Module.t)];
           if Output.targets output Names then print_s [%sexp (names : Name_bindings.t)];
           if Output.requires output Generating_mir
           then (
             let fun_factory, mir = or_raise (Mir.of_typed_module ~names ast) in
             if Output.targets output Mir then print_s [%sexp (mir : Mir.t)];
             if Output.targets output Templates
             then print_s [%sexp (fun_factory : Mir.Function_factory.t)];
             if Output.requires output Generating_llvm
             then
               Codegen.of_mir ~source_filename:filename mir
               |> Codegen.to_string
               |> print_endline)))
       else if Output.targets output Tokens
       then
         Parsing.lex_file filename ~print_tokens_to:stdout
         |> Result.iter_error ~f:(fun error ->
              print_s [%sexp (error : Compilation_error.t)]))
;;

let () = Command.run command
