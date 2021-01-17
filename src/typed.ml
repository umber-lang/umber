open Import
open Names

let type_error_msg = Type_bindings.type_error_msg

module Pattern = struct
  type t =
    | Constant of Untyped.Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr_name.Qualified.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) list
    | Union of t * t
  [@@deriving sexp]

  let of_untyped_with_names ~names ~types pat =
    let rec of_untyped_with_names ~names ~types pat_names = function
      | Untyped.Pattern.Constant lit ->
        pat_names, (Constant lit, Type.Concrete.cast (Untyped.Literal.typ lit))
      | Catch_all name ->
        let pat_names, typ =
          match name with
          | Some name -> Untyped.Pattern.Names.add_fresh_name pat_names name
          | None -> pat_names, Type.fresh_var ()
        in
        pat_names, (Catch_all name, typ)
      | Cnstr_appl (cnstr, args) ->
        (* TODO: inferring unqualified name given type information *)
        let rec loop pat_names args = function
          | Type.Expr.Function (arg_type, body_type), arg :: rest ->
            let pat_names, (arg, arg_type2) =
              of_untyped_with_names ~names ~types pat_names arg
            in
            Type_bindings.unify ~names ~types arg_type arg_type2;
            loop pat_names (arg :: args) (body_type, rest)
          | body_type, [] -> pat_names, (Cnstr_appl (cnstr, List.rev args), body_type)
          | _ -> type_error_msg "Arg number mismatch"
        in
        loop pat_names [] (Name_bindings.find_cnstr_type names cnstr, args)
      | Tuple fields ->
        let pat_names, fields, field_types =
          List.fold_right
            fields
            ~init:(pat_names, [], [])
            ~f:(fun field (pat_names, fields, field_types) ->
            let pat_names, (field, field_type) =
              of_untyped_with_names ~names ~types pat_names field
            in
            pat_names, field :: fields, field_type :: field_types)
        in
        pat_names, (Tuple fields, Tuple field_types)
      | Record _fields ->
        (*let _names, _fields =
          List.fold_right
            fields
            ~init:(names, [])
            ~f:(fun (field_name, field_pat) (names, fields) ->
            let names, field_pat =
              match field_pat with
              | Some field_pat ->
                let new_names, field_pat =
                  of_untyped ~handle_name ~names ~types field_pat
                in
                merge_no_shadow names new_names, Some field_pat
              | None -> names, None
            in
            names, (field_name, field_pat) :: fields)
        in*)
        (*names, (Record fields, Record (List.map ~f:(fun (name, pat) ->
              match pat with
              | Some (_, typ) -> typ
              | None -> Type_bindings.fresh_var ())))*)
        failwith "TODO: record types in patterns"
      | Union (pat1, pat2) ->
        let pat_names1, (pat1, typ1) =
          of_untyped_with_names ~names ~types pat_names pat1
        in
        let pat_names2, (pat2, typ2) =
          of_untyped_with_names ~names ~types pat_names pat2
        in
        Type_bindings.unify ~names ~types typ1 typ2;
        (* Unions must define the same names with the same types *)
        if not
             (Map.equal
                (fun entry1 entry2 ->
                  Type_bindings.unify
                    ~names
                    ~types
                    (Name_bindings.Name_entry.typ entry1)
                    (Name_bindings.Name_entry.typ entry2);
                  true)
                pat_names1
                pat_names2)
        then type_error_msg "Pattern unions must define the same names";
        pat_names1, (Union (pat1, pat2), typ1)
      | As (pat, name) ->
        let pat_names, (pat, typ) = of_untyped_with_names ~names ~types pat_names pat in
        let pat_names = Untyped.Pattern.Names.add_name pat_names name typ in
        pat_names, (As (pat, name), typ)
      | Type_annotation (pat, typ) ->
        let typ1 =
          Type.Scheme.instantiate_bounded
            ~map_name:(Name_bindings.absolutify_type_name names)
            typ
        in
        let pat_names, (pat, typ2) = of_untyped_with_names ~names ~types pat_names pat in
        Type_bindings.unify ~names ~types typ1 typ2;
        pat_names, (pat, typ2)
    in
    let pat_names, (pat, typ) =
      of_untyped_with_names ~names ~types Value_name.Map.empty pat
    in
    pat_names, (pat, Type_bindings.substitute types typ)
  ;;

  let of_untyped_into ~names ~types pattern =
    let ((pat_names, _) as pat) = of_untyped_with_names ~names ~types pattern in
    let names =
      Name_bindings.merge_names names pat_names ~combine:(fun _ _ new_entry -> new_entry)
    in
    names, pat
  ;;

  let generalize ~names ~types pat_names typ =
    let names =
      Map.fold pat_names ~init:names ~f:(fun ~key:name ~data:entry names ->
        let typ = Name_bindings.Name_entry.typ entry in
        let scheme = Type_bindings.generalize types typ in
        Name_bindings.set_scheme names name scheme)
    in
    names, Type_bindings.generalize types typ
  ;;
end

module Expr = struct
  type 'typ t =
    | Literal of Untyped.Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of 'typ t * 'typ t
    | Lambda of Pattern.t * 'typ t
    | Match of 'typ t * (Pattern.t * 'typ t) list
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t list
    | Record_literal of (Value_name.t * 'typ t option) list
    | Record_update of 'typ t * (Value_name.t * 'typ t option) list
    | Record_field_access of 'typ t * Value_name.t
  [@@deriving sexp]

  let of_untyped ~names ~types expr =
    let rec of_untyped ~names ~types expr =
      match (expr : Untyped.Expr.t) with
      | Literal lit -> Literal lit, Type.Concrete.cast (Untyped.Literal.typ lit)
      | Name name -> Name name, Name_bindings.find_type names name
      | Qualified (path, expr) ->
        of_untyped ~names:(Name_bindings.import_all names path) ~types expr
      | Fun_call (f, arg) ->
        let f, f_type = of_untyped ~names ~types f in
        let arg, arg_type = of_untyped ~names ~types arg in
        let result_type = Type.fresh_var () in
        Type_bindings.unify ~names ~types f_type (Function (arg_type, result_type));
        Fun_call (f, arg), result_type
      | Op_tree tree -> of_untyped ~names ~types (Op_tree.to_untyped_expr ~names tree)
      | Lambda (pat, body) ->
        let names, (_, (pat, pat_type)) = Pattern.of_untyped_into ~names ~types pat in
        let body, body_type = of_untyped ~names ~types body in
        Lambda (pat, body), Function (pat_type, body_type)
      | If (cond, then_, else_) ->
        let cond, cond_type = of_untyped ~names ~types cond in
        let bool_type = Type.Concrete.cast Core.Bool.typ in
        Type_bindings.unify ~names ~types cond_type bool_type;
        let (then_, then_type), (else_, else_type) =
          of_untyped ~names ~types then_, of_untyped ~names ~types else_
        in
        Type_bindings.unify ~names ~types then_type else_type;
        (* TODO: should really be referring to Bool as a primitive of some kind
           Could have it be qualified like `_Primitives.Bool` *)
        let cnstr name = Pattern.Cnstr_appl (name, []) in
        ( Match (cond, [ cnstr Core.Bool.true_, then_; cnstr Core.Bool.false_, else_ ])
        , then_type )
      | Match (expr, branches) ->
        let expr, expr_type = of_untyped ~names ~types expr in
        let branches, branch_types =
          List.map branches ~f:(fun (pat, branch) ->
            let names, (_, (pat, pat_typ)) = Pattern.of_untyped_into ~names ~types pat in
            Type_bindings.unify ~names ~types expr_type pat_typ;
            let branch, branch_type = of_untyped ~names ~types branch in
            (pat, branch), branch_type)
          |> List.unzip
        in
        (match branch_types with
        | branch_type :: _ ->
          iter_pairs branch_types ~f:(Type_bindings.unify ~names ~types);
          Match (expr, branches), branch_type
        | [] -> compiler_bug [%message "Empty match"])
      | Let { rec_; bindings; body } ->
        let names, bindings =
          if rec_
          then (
            (* FIXME: need to keep pat_names here so we can generalize later*)
            (* Process all bindings at once, allowing mutual recursion *)
            let names, bindings =
              List.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
                let names, (pat_names, (pat, pat_type)) =
                  Pattern.of_untyped_into ~names ~types pat
                in
                names, ((pat, (pat_type, pat_names)), expr))
            in
            let bindings =
              List.map bindings ~f:(Tuple2.map_snd ~f:(of_untyped ~names ~types))
            in
            ( names
            , List.map
                bindings
                ~f:(fun (((_, (pat_type, _)) as pat), (expr, expr_type)) ->
                Type_bindings.unify ~names ~types pat_type expr_type;
                pat, expr) ))
          else
            (* Process bindings in order without any recursion *)
            List.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
              let names, (pat_names, (pat, pat_type)) =
                Pattern.of_untyped_into ~names ~types pat
              in
              let expr, expr_type = of_untyped ~names ~types expr in
              Type_bindings.unify ~names ~types pat_type expr_type;
              names, ((pat, (pat_type, pat_names)), expr))
        in
        let body, body_type = of_untyped ~names ~types body in
        Let { rec_; bindings; body }, body_type
      | Tuple items ->
        let items, types = List.map items ~f:(of_untyped ~names ~types) |> List.unzip in
        Tuple items, Tuple types
      | Seq_literal _items -> failwith "TODO: seq"
      | Record_literal _fields -> failwith "TODO: record1"
      | Record_update (_expr, _fields) -> failwith "TODO: record2"
      | Record_field_access (_record, _name) -> failwith "TODO: record3"
      | Type_annotation (expr, typ) ->
        let t1 =
          Type.Scheme.instantiate_bounded
            ~map_name:(Name_bindings.absolutify_type_name names)
            typ
        in
        let expr, t2 = of_untyped ~names ~types expr in
        Type_bindings.unify ~names ~types t1 t2;
        expr, t1
    in
    of_untyped ~names ~types expr |> Tuple2.map_snd ~f:(Type_bindings.substitute types)
  ;;

  let rec map expr ~f ~var =
    match f expr with
    | `Halt expr -> expr
    | `Retry expr -> map ~f ~var expr
    | `Defer expr ->
      (match expr with
      | Let bindings -> Let (var bindings)
      | (Literal _ | Name (_, _)) as expr -> expr
      | Fun_call (func, body) -> Fun_call (map ~f ~var func, map ~f ~var body)
      | Lambda (arg, body) -> Lambda (arg, map ~f ~var body)
      | Match (expr, branches) ->
        Match (map ~f ~var expr, List.map branches ~f:(Tuple2.map_snd ~f:(map ~f ~var)))
      | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var))
      | Record_literal fields ->
        Record_literal
          (List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f ~var))))
      | Record_update (expr, fields) ->
        Record_update
          ( map ~f ~var expr
          , List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f ~var))) )
      | Record_field_access (record, field) ->
        Record_field_access (map ~f ~var record, field))
  ;;

  let map_bindings expr ~f = map expr ~f:(fun expr -> `Defer expr) ~var:f

  let rec generalize_let_bindings ~names ~types =
    map_bindings ~f:(fun { rec_; bindings; body } ->
      let bindings =
        List.map bindings ~f:(fun ((pat, (pat_type, pat_names)), expr) ->
          let names, scheme = Pattern.generalize ~names ~types pat_names pat_type in
          (pat, scheme), generalize_let_bindings ~names ~types expr)
      in
      { rec_; bindings; body = generalize_let_bindings ~names ~types body })
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Type.Scheme.t Expr.t * Type.Scheme.t) t [@@deriving sexp]

  type nonrec def = (Pattern.t, Type.Scheme.t Expr.t * Type.Scheme.t) def
  [@@deriving sexp]

  let rec gather_names ~names ~f_common ?(f_def = fun _ _ -> None) sigs defs =
    let names =
      List.fold sigs ~init:names ~f:(fun names sig_ ->
        match sig_.Node.node with
        | Common_sig common -> f_common names common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule names module_name ~f:(fun names ->
            gather_names ~names sigs [] ~f_common ~f_def))
    in
    List.fold defs ~init:names ~f:(fun names def ->
      option_or_default (f_def names def) ~f:(fun () ->
        match def.Node.node with
        | Common_def common -> f_common names common
        | Module (module_name, sigs, defs) ->
          Name_bindings.with_submodule names module_name ~f:(fun names ->
            gather_names ~names sigs defs ~f_common ~f_def)
        | Let _ | Trait _ | Impl _ -> names))
  ;;

  (* Name/type resolution steps
     1. Read all bound names in the module and submodules, and assign them fresh type variables
        - This doesn't require knowing anything - just read all the Catch_all names
        - Also read the types in somehow with placeholder values
        - This allows local imports to check properly
        - (* TODO: really only need to do sigs, and then defs if there are no sigs *)
        - (* TODO: is this needed for values anymore? Let re-grouping should put things in
             dependency order *)
     2. Read imports
        - Don't copy anything - use Imported_from
     3. Read type/trait declarations
     4. (not yet (?)) Look for trait impls (just the fact they exist)
        - Needs to be done after knowing about types/traits 
     5. Handle val/let bindings
     6. Handle let/impl expressions
        - Need to re-order and re-group bindings by their dependencies
        - After re-ordering, type each expression and generalize each binding group *)

  let rec gather_name_placeholders ~names sigs defs =
    (* Gather placeholders from all val statements first to correctly handle the presence
       of both val and let without complaining about duplicate names *)
    let names =
      gather_names ~names sigs defs ~f_common:(fun names -> function
        | Val (name, _, _) | Extern (name, _, _, _) ->
          fst (Name_bindings.add_fresh_var names name)
        | Type_decl (type_name, _) -> Name_bindings.add_type_placeholder names type_name
        | Import _ | Import_with _ | Import_without _ | Trait_sig _ -> names)
    in
    List.fold defs ~init:names ~f:(fun names def ->
      match def.Node.node with
      | Let bindings ->
        List.fold bindings ~init:names ~f:(fun names { node = pat, _; _ } ->
          Name_bindings.merge_names
            names
            (Untyped.Pattern.Names.gather pat)
            ~combine:(fun name entry1 entry2 ->
            match entry1.type_source, entry2.type_source with
            | Val_declared, Val_declared | Let_inferred, Let_inferred ->
              Name_bindings.name_error_msg "Duplicate name" (Value_name.to_ustring name)
            | _ -> entry1))
      | Module (module_name, _, defs) ->
        (* TODO: maybe don't look at submodule defs unless there's no sig? 
           Need to handle cross-module privacy properly *)
        Name_bindings.with_submodule names module_name ~f:(fun names ->
          gather_name_placeholders ~names [] defs)
      | Common_def _ | Trait _ | Impl _ -> names)
  ;;

  let gather_imports_and_type_decls ~names sigs defs =
    gather_names ~names sigs defs ~f_common:(fun names -> function
      | Import module_name -> Name_bindings.import names module_name
      | Import_with (path, imports) -> Name_bindings.import_with names path imports
      | Import_without (path, hiding) -> Name_bindings.import_without names path hiding
      | Type_decl (type_name, decl) -> Name_bindings.add_type_decl names type_name decl
      | Trait_sig _ -> failwith "TODO: trait sigs"
      | Val _ | Extern _ -> names)
  ;;

  let rec handle_value_bindings ~names ~types sigs defs =
    let handle_common ~names = function
      | Val (name, fixity, typ) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_val names name fixity typ ~unify
      | Extern (name, fixity, typ, extern_name) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_val names name fixity ([], typ) ~extern_name ~unify
      | Type_decl _ | Trait_sig _ | Import _ | Import_with _ | Import_without _ -> names
    in
    let rec handle_sigs ~names ~handle_common =
      List.fold ~init:names ~f:(fun names sig_ ->
        match sig_.Node.node with
        | Common_sig common -> handle_common ~names common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule names module_name ~f:(fun names ->
            handle_sigs ~names ~handle_common sigs))
    in
    let names = handle_sigs ~names ~handle_common sigs in
    let handle_bindings ~names =
      List.fold_map
        ~init:names
        ~f:
          (Node.fold_map ~f:(fun names (pat, expr) ->
             let pat_names, pat = Pattern.of_untyped_with_names ~names ~types pat in
             let names =
               (* Unify pattern bindings with local type information (e.g. val declarations) *)
               Name_bindings.merge_names names pat_names ~combine:(fun _ entry entry' ->
                 let typ, typ' = Name_bindings.Name_entry.(typ entry, typ entry') in
                 Type_bindings.unify ~names ~types typ typ';
                 entry)
             in
             names, ((pat, pat_names), expr)))
    in
    List.fold_map
      defs
      ~init:names
      ~f:
        (Node.fold_map ~f:(fun names -> function
           | Let bindings -> handle_bindings ~names bindings |> Tuple2.map_snd ~f:let_
           | Module (module_name, sigs, defs) ->
             let names, defs =
               Name_bindings.with_submodule' names module_name ~f:(fun names ->
                 handle_value_bindings ~names ~types sigs defs)
             in
             names, Module (module_name, sigs, defs)
           | Common_def common as def -> handle_common ~names common, def
           | Impl _ | Trait _ -> failwith "TODO: handle_value_bindings traits/impls"))
  ;;

  (** Re-group and re-order toplevel let bindings so that each group is mutually
      recursive, and the groups are given in an order appropriate for generalization.
      This is done by topologically sorting the strongly-connected components of the call
      graph (dependencies between bindings). *)
  let extract_binding_groups ~names defs =
    let rec gather_bindings_in_defs ~names other_defs defs =
      Sequence.of_list defs |> Sequence.concat_map ~f:(gather_bindings ~names other_defs)
    and gather_bindings ~names other_defs def =
      match def.Node.node with
      | Let bindings ->
        Sequence.map
          (Sequence.of_list bindings)
          ~f:(fun { node = ((_, pat_names) as pat), expr; span } ->
          let current_path = Name_bindings.current_path names in
          let bound_names = Map.key_set pat_names in
          let used_names = Untyped.Expr.names_used ~names expr in
          ( { Call_graph.Binding.bound_names; used_names; info = pat, expr, span }
          , current_path ))
      | Module (module_name, _, defs) ->
        let names = Name_bindings.into_module names module_name in
        gather_bindings_in_defs ~names other_defs defs
      | Common_def _ as node ->
        Queue.enqueue other_defs { def with node };
        Sequence.empty
      | Trait _ | Impl _ -> failwith "TODO: traits/impls"
    in
    let other_defs = Queue.create () in
    let binding_groups =
      gather_bindings_in_defs ~names other_defs defs
      |> Call_graph.of_bindings
      |> Call_graph.to_regrouped_bindings
    in
    Queue.to_list other_defs, binding_groups
  ;;

  let type_binding_group ~names ~types bindings =
    (* Order bindings in the group by span, then take the first span to represent the
       whole group. This is done to get a consistent ordering among bindings. *)
    let get_span { Call_graph.Binding.info = _, _, span; _ } = span in
    let bindings =
      List.sort bindings ~compare:(fun b b' -> Span.compare (get_span b) (get_span b'))
    in
    let span = get_span (List.hd_exn bindings) in
    let bindings =
      (* First, generalize the toplevel bindings *)
      List.map bindings ~f:(fun { info = (((_, pat_type), _) as pat), expr, span; _ } ->
        let expr, expr_type = Expr.of_untyped ~names ~types expr in
        Type_bindings.unify ~names ~types pat_type expr_type;
        pat, expr, span)
    in
    List.fold_map
      bindings
      ~init:names
      ~f:(fun names (((pat, pat_type), pat_names), expr, span) ->
      let names, scheme = Pattern.generalize ~names ~types pat_names pat_type in
      (* Generalize local let bindings just after the parent binding *)
      (* TODO: Look into the concept of type variable scope - parent variables are
         getting generalized here as well, which is probably wrong *)
      let expr = Expr.generalize_let_bindings ~names ~types expr in
      names, { Node.node = pat, (expr, scheme); span })
    |> Tuple2.map_snd ~f:(fun bindings -> { Node.node = Let bindings; span })
  ;;

  (** Reintegrate the re-ordered binding groups from [extract_binding_groups] back into
      the AST. *)
  let reintegrate_binding_groups path other_defs binding_groups =
    (* NOTE: As we disallow cross-module mutual recursion, binding groups will always be
       contained within a single module and can just be put back into the AST *)
    let binding_table = Module_path.Table.create () in
    List.iter binding_groups ~f:(fun (def, path) ->
      Hashtbl.add_multi binding_table ~key:path ~data:def);
    let rec loop binding_table path defs =
      let defs =
        List.map
          defs
          ~f:
            (Node.map ~f:(function
              | Module (module_name, sigs, defs) ->
                Module
                  (module_name, sigs, loop binding_table (path @ [ module_name ]) defs)
              | _ as def -> def))
      in
      (* Sort defs by span to get them back into their original order *)
      List.sort
        (Hashtbl.find_multi binding_table path @ defs)
        ~compare:(fun def def' -> Span.compare def.span def'.span)
    in
    loop binding_table path other_defs
  ;;

  let type_defs ~names ~types defs =
    let other_defs, binding_groups = extract_binding_groups ~names defs in
    let names, binding_groups =
      Sequence.to_list binding_groups
      |> List.fold_map ~init:names ~f:(fun names (bindings, path) ->
           let names, def = type_binding_group ~names ~types bindings in
           names, (def, path))
    in
    let path = Name_bindings.current_path names in
    names, reintegrate_binding_groups path other_defs binding_groups
  ;;

  let of_untyped ?(backtrace = true) ?names ?types (module_name, sigs, defs) =
    let names =
      option_or_default names ~f:(fun () -> Lazy.force Name_bindings.std_prelude)
    in
    let types = option_or_default types ~f:Type_bindings.create in
    try
      let names = Name_bindings.into_module names module_name in
      let names = gather_name_placeholders ~names sigs defs in
      let names = gather_imports_and_type_decls ~names sigs defs in
      let names, defs = handle_value_bindings ~names ~types sigs defs in
      let names, defs = type_defs ~names ~types defs in
      Ok (Name_bindings.into_parent names, (module_name, sigs, defs))
    with
    | exn ->
      let msg =
        if backtrace
        then (
          let backtrace = Backtrace.(to_string (Exn.most_recent ())) in
          Exn.to_string exn ^ "\n" ^ backtrace)
        else (
          match exn with
          | Type_bindings.Type_error (msg, Some (t1, t2)) ->
            (* Prevent unstable Var_ids from appearing in test output *)
            let env = Type.Param.Env_of_vars.create () in
            let map_type t =
              Type.Expr.map_vars t ~f:(Type.Param.Env_of_vars.find_or_add env)
              |> Type.Scheme.sexp_of_t
            in
            let ts = Sexp.(List [ List [ map_type t1; map_type t2 ] ]) in
            let msg = Ustring.to_string msg in
            Sexp.(
              to_string_hum
                ~indent:2
                (List [ Atom "src/type_bindings.ml.Type_error"; Atom msg; ts ]))
          | _ -> Exn.to_string exn)
      in
      Error (Ustring.of_string_exn msg)
  ;;
end
