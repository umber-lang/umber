open! Core
open! Import
module Expr = Mir_expr

module Stmt = struct
  (* TODO: Consider passing through information about whether the values/functions are
     exposed to other files so we can decide on proper LLVM linkage for them. *)
  type t =
    | Value_def of Mir_name.t * Expr.t
    | Fun_def of Expr.Fun_def.t
    | Fun_decl of Fun_decl.t
      (* TODO: [Fun_decl] is used to declare things that aren't functions too, which is a
         bit weird. Maybe rename to just [Decl]? [Extern_decl] also works like this but at
         least the name is less misleading. *)
    | Extern_decl of Extern_decl.t
  [@@deriving sexp_of, variants]
end

type t = Stmt.t list [@@deriving sexp_of]

(* To prevent declaring `extern` functions used in this file, add them to `fun_decls`
   up-front. This is kind of a hack - ideally we'd be able to tell at lookup time that
   these are defined in the same file and therefore don't need declaring. *)
let rec mark_extern_functions_as_declared
  ~ctx
  ~fun_decls
  (defs : Typed_ast.Module.def Node.t list)
  =
  List.iter defs ~f:(fun def ->
    Node.with_value def ~f:(function
      | Common_def (Extern (value_name, _, _, _)) ->
        let name = Context.find_value_name_assert_external ctx value_name in
        Hash_set.add fun_decls name
      | Module (module_name, _sigs, defs) ->
        let (_ : Context.t), () =
          Context.with_module ctx module_name ~f:(fun ctx ->
            ctx, mark_extern_functions_as_declared ~ctx ~fun_decls defs)
        in
        ()
      | _ -> ()))
;;

let handle_let_bindings
  ~ctx
  ~names
  ~stmts
  ~rec_
  ~fun_decls
  (bindings :
    (Typed_ast.Pattern.generalized Node.t
    * Fixity.t option
    * Typed_ast.Expr.generalized Node.t)
    Nonempty.t)
  =
  let process_expr (stmts : Stmt.t list) ~just_bound ~ctx expr typ =
    let stmts = ref stmts in
    let add_fun_def fun_def = stmts := Fun_def fun_def :: !stmts in
    let add_fun_decl (fun_decl : Fun_decl.t) =
      if not (Hash_set.mem fun_decls fun_decl.name)
      then (
        Hash_set.add fun_decls fun_decl.name;
        stmts := Fun_decl fun_decl :: !stmts)
    in
    let expr =
      Node.map expr ~f:(fun expr ->
        Expr.of_typed_expr ~just_bound ~ctx ~add_fun_def ~add_fun_decl expr typ)
    in
    !stmts, expr
  in
  let add_let (stmts : Stmt.t list) name mir_expr typ =
    match (mir_expr : Expr.t) with
    | Name name' when Mir_name.(name = name') ->
      (* Don't make a Value_def in the case where all we did is make a Fun_def *)
      stmts
    | _ ->
      let arity = Type_utils.arity_of_type ~names typ in
      if arity > 0
      then (
        (* For function types, we need to ensure the final definition is also a function.
           Create one which just forwards the call to the expression. Importantly, the
           name used for the function must be the original name, which should be a
           proper name from the source (its id should be 0). This lets code in other
           files link with it properly. *)
        let args =
          Nonempty.init arity ~f:(fun i ->
            snd (Context.add_value_name ctx (Constant_names.synthetic_arg i)))
        in
        match mir_expr with
        | Name name' ->
          (* If the body is just another name, we can call it directly. *)
          let body : Expr.t =
            Fun_call (name', Nonempty.map args ~f:(fun arg -> Expr.Name arg))
          in
          Fun_def { fun_name = name; args; body } :: stmts
        | _ ->
          (* Otherwise, we need to generate a [Value_def] and then call it. *)
          let name' = Context.copy_name ctx name in
          let body : Expr.t =
            Fun_call (name', Nonempty.map args ~f:(fun arg -> Expr.Name arg))
          in
          Fun_def { fun_name = name; args; body } :: Value_def (name', mir_expr) :: stmts)
      else Value_def (name, mir_expr) :: stmts
  in
  let bindings =
    Nonempty.map bindings ~f:(fun ((pattern, (_ : Fixity.t option), _) as binding) ->
      let names_bound =
        Node.with_value pattern ~f:(fun pattern ->
          Typed_ast.Pattern.fold_names
            pattern
            ~init:Mir_name.Set.empty
            ~f:(fun names_bound name ->
            Set.add names_bound (Context.find_value_name_assert_local ctx name)))
      in
      binding, names_bound)
  in
  Expr.generate_let_bindings
    bindings
    ~ctx
    ~ctx_for_body:ctx
    ~rec_
    ~init:stmts
    ~add_let
    ~extract_binding:(fun (pat, (_ : Fixity.t option), expr_and_type) ->
      pat, Node.map expr_and_type ~f:fst, Node.with_value expr_and_type ~f:snd)
    ~process_expr
;;

let generate_variant_constructor_values ~ctx ~stmts decl =
  match Context.find_cnstr_info_from_decl ctx decl ~follow_aliases:false with
  | None -> stmts
  | Some cnstr_info ->
    Cnstr_info.fold cnstr_info ~init:stmts ~f:(fun stmts cnstr tag ~arg_count ->
      match cnstr with
      | Tuple -> stmts
      | Named cnstr_name ->
        let name =
          Context.find_value_name_assert_local ctx (Value_name.of_cnstr_name cnstr_name)
        in
        let stmt : Stmt.t =
          if arg_count = 0
          then Value_def (name, Make_block { tag; fields = [] })
          else (
            let arg_names =
              List.init arg_count ~f:(fun i ->
                snd (Context.add_value_name ctx (Constant_names.synthetic_arg i)))
            in
            Fun_def
              { fun_name = name
              ; args = arg_names |> Nonempty.of_list_exn
              ; body =
                  Make_block
                    { tag; fields = List.map arg_names ~f:(fun name -> Expr.Name name) }
              })
        in
        stmt :: stmts)
;;

let generate_effect_operation_values ~ctx ~stmts ({ operations; params = _ } : _ Effect.t)
  =
  Option.fold operations ~init:stmts ~f:(fun stmts operations ->
    List.fold operations ~init:stmts ~f:(fun stmts { name; args; result = _ } ->
      let fun_name, name_kind =
        Context.find_value_name ctx (Context.current_path ctx, name)
      in
      let effect_op =
        match name_kind with
        | Effect_op id -> id
        | _ ->
          compiler_bug
            [%message
              "Unexpected name kind for effect op"
                (name : Value_name.t)
                (name_kind : Context.Name_kind.t)]
      in
      let args =
        Nonempty.mapi args ~f:(fun i (_ : _ Type_scheme.type_) ->
          snd (Context.add_value_name ctx (Constant_names.synthetic_arg i)))
      in
      let body : Expr.t =
        Perform_effect
          { effect_op; args = Nonempty.map args ~f:(fun arg -> Expr.Name arg) }
      in
      let stmt : Stmt.t = Fun_def { fun_name; args; body } in
      stmt :: stmts))
;;

let of_typed_module =
  let rec loop
    ~ctx
    ~names
    ~stmts
    ~let_bindings
    ~fun_decls
    (defs : Typed_ast.Module.def Node.t list)
    =
    List.fold
      defs
      ~init:(ctx, (stmts, let_bindings))
      ~f:(fun (ctx, (stmts, let_bindings)) def ->
        Node.with_value def ~f:(function
          | Let let_binding_group ->
            ctx, (stmts, (let_binding_group, Context.current_path ctx) :: let_bindings)
          | Module (module_name, _sigs, defs) ->
            Context.with_module ctx module_name ~f:(fun ctx ->
              loop ~ctx ~names ~stmts ~let_bindings ~fun_decls defs)
          | Trait _ | Impl _ -> failwith "TODO: MIR traits/impls"
          | Common_def (Type_decl ((_ : Type_name.t), ((_, decl) : _ Type_decl.t))) ->
            ctx, (generate_variant_constructor_values ~ctx ~stmts decl, let_bindings)
          | Common_def (Extern (value_name, (_ : Fixity.t option), type_, extern_name)) ->
            let name = Context.find_value_name_assert_external ctx value_name in
            let arity = Type_utils.arity_of_type ~names (fst type_) in
            ctx, (Extern_decl { name; extern_name; arity } :: stmts, let_bindings)
          | Common_def (Effect ((_ : Effect_name.t), effect_decl)) ->
            ctx, (generate_effect_operation_values ~ctx ~stmts effect_decl, let_bindings)
          | Common_def (Import _) -> ctx, (stmts, let_bindings)))
  in
  fun ~names ((module_name, _sigs, defs) : Typed_ast.Module.t) ->
    let names = Name_bindings.into_module names module_name ~place:`Def in
    let ctx = Context.create ~names ~name_table:(Mir_name.Name_table.create ()) in
    let fun_decls = Mir_name.Hash_set.create () in
    mark_extern_functions_as_declared ~ctx ~fun_decls defs;
    Compilation_error.try_with' (fun () ->
      let ctx, (stmts, let_bindings) =
        loop ~ctx ~names ~stmts:[] ~let_bindings:[] ~fun_decls defs
      in
      let (_ : Context.t), stmts =
        List.sort
          let_bindings
          ~compare:
            (Comparable.lift
               ~f:(Typed_ast.Let_binding_group.index << fst)
               [%compare: Typed_ast.Let_binding_group.Index.t])
        |> List.fold
             ~init:(ctx, stmts)
             ~f:(fun (ctx, stmts) ({ rec_; bindings; index = _ }, path) ->
             Context.with_path_into_defs ctx path ~f:(fun ctx ->
               handle_let_bindings ~ctx ~names ~stmts ~rec_ ~fun_decls bindings))
      in
      List.rev stmts)
;;
