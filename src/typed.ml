open Import
open Names

let type_error_msg = Type_bindings.type_error_msg

module Pattern = struct
  include Pattern

  type nonrec t = Nothing.t t [@@deriving sexp]

  (* TODO: either split up Untyped/Typed patterns into different types or stop returning
     a pattern from these functions *)
  let of_untyped_with_names ~names ~types (pat : Untyped.Pattern.t)
    : Names.t * (t * Type.t)
    =
    let rec of_untyped_with_names ~names ~types pat_names
      : Untyped.Pattern.t -> Names.t * (t * Type.t)
      = function
      | Constant lit -> pat_names, (Constant lit, Type.Concrete.cast (Literal.typ lit))
      | Catch_all name ->
        let pat_names, typ =
          match name with
          | Some name -> Pattern.Names.add_fresh_name pat_names name
          | None -> pat_names, Type.fresh_var ()
        in
        pat_names, (Catch_all name, typ)
      | Cnstr_appl (cnstr, args) ->
        (* TODO: inferring unqualified name given type information *)
        let arg_types, body_type =
          match Name_bindings.find_cnstr_type names cnstr with
          | Function (arg_types, body_type) -> Nonempty.to_list arg_types, body_type
          | body_type -> [], body_type
        in
        (match
           List.fold2
             arg_types
             args
             ~init:(pat_names, [])
             ~f:(fun (pat_names, args) arg_type arg ->
             let pat_names, (arg, arg_type') =
               of_untyped_with_names ~names ~types pat_names arg
             in
             Type_bindings.unify ~names ~types arg_type arg_type';
             pat_names, arg :: args)
         with
         | Ok (pat_names, args) ->
           pat_names, (Cnstr_appl (cnstr, List.rev args), body_type)
         | Unequal_lengths ->
           type_error_msg "Wrong number of arguments in constructor application")
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
        let pat_names = Pattern.Names.add_name pat_names name typ in
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
        Name_bindings.Name_entry.typ entry
        |> Type_bindings.generalize types
        |> Name_bindings.set_inferred_scheme names name)
    in
    names, Type_bindings.generalize types typ
  ;;
end

module Expr = struct
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of 'typ t * ('typ t * 'typ) Nonempty.t
    | Lambda of Pattern.t Nonempty.t * 'typ t
    | Match of 'typ t * 'typ * (Pattern.t * 'typ t) Nonempty.t
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t list
    | Record_literal of (Value_name.t * 'typ t option) list
    | Record_update of 'typ t * (Value_name.t * 'typ t option) list
    | Record_field_access of 'typ t * Value_name.t
  [@@deriving sexp]

  type generalized = Type.Scheme.t t * Type.Scheme.t [@@deriving sexp_of]

  let of_untyped, type_recursive_let_bindings =
    let rec of_untyped ~names ~types ~f_name expr =
      match (expr : Untyped.Expr.t) with
      | Literal lit -> Literal lit, Type.Concrete.cast (Literal.typ lit)
      | Name name ->
        let name_entry = Name_bindings.find_entry names name in
        f_name name name_entry;
        Name name, Name_bindings.Name_entry.typ name_entry
      | Qualified (path, expr) ->
        of_untyped ~names:(Name_bindings.import_all names path) ~types ~f_name expr
      | Fun_call (fun_, args) ->
        let fun_, fun_type = of_untyped ~names ~types ~f_name fun_ in
        let args =
          Nonempty.map args ~f:(fun arg ->
            let arg, arg_type = of_untyped ~names ~types ~f_name arg in
            arg, (arg_type, Pattern.Names.empty))
        in
        let arg_types = Nonempty.map args ~f:(fun (_, (arg_type, _)) -> arg_type) in
        let result_var = Type.Var_id.create () in
        Type_bindings.unify
          ~names
          ~types
          fun_type
          (Partial_function (arg_types, result_var));
        Fun_call (fun_, args), Var result_var
      | Op_tree tree ->
        of_untyped ~names ~types ~f_name (Op_tree.to_untyped_expr ~names tree)
      | Lambda (args, body) ->
        let names, args_and_types =
          Nonempty.fold_map args ~init:names ~f:(fun names arg ->
            let names, (_, (arg, arg_type)) = Pattern.of_untyped_into ~names ~types arg in
            names, (arg, arg_type))
        in
        let args, arg_types = Nonempty.unzip args_and_types in
        let body, body_type = of_untyped ~names ~types ~f_name body in
        Lambda (args, body), Function (arg_types, body_type)
      | If (cond, then_, else_) ->
        let cond, cond_type = of_untyped ~names ~types ~f_name cond in
        let bool_type = Type.Concrete.cast Intrinsics.Bool.typ in
        Type_bindings.unify ~names ~types cond_type bool_type;
        let (then_, then_type), (else_, else_type) =
          of_untyped ~names ~types ~f_name then_, of_untyped ~names ~types ~f_name else_
        in
        Type_bindings.unify ~names ~types then_type else_type;
        (* TODO: should really be referring to Bool as a primitive of some kind since
           otherwise you could shadow it. Could have it be qualified like
           `_Primitives.Bool` *)
        let cnstr name : Pattern.t = Cnstr_appl (name, []) in
        ( Match
            ( cond
            , (bool_type, Pattern.Names.empty)
            , [ cnstr Intrinsics.Bool.true_, then_; cnstr Intrinsics.Bool.false_, else_ ]
            )
        , then_type )
      | Match (expr, branches) ->
        let expr, expr_type = of_untyped ~names ~types ~f_name expr in
        let branches, branch_type :: rest =
          Nonempty.map branches ~f:(fun (pat, branch) ->
            let names, (_, (pat, pat_typ)) = Pattern.of_untyped_into ~names ~types pat in
            Type_bindings.unify ~names ~types expr_type pat_typ;
            let branch, branch_type = of_untyped ~names ~types ~f_name branch in
            (pat, branch), branch_type)
          |> Nonempty.unzip
        in
        List.iter_pairs (branch_type :: rest) ~f:(Type_bindings.unify ~names ~types);
        Match (expr, (expr_type, Pattern.Names.empty), branches), branch_type
      | Let { rec_; bindings; body } ->
        let names, rec_, bindings =
          if rec_
          then (
            let names, bindings =
              Nonempty.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
                let names, (pat_names, (pat, pat_type)) =
                  Pattern.of_untyped_into ~names ~types pat
                in
                names, ((pat, (pat_type, pat_names)), expr))
            in
            type_recursive_let_bindings ~names ~types ~f_name bindings)
          else (
            (* Process bindings in order without any recursion *)
            let names, bindings =
              Nonempty.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
                let names, (pat_names, (pat, pat_type)) =
                  Pattern.of_untyped_into ~names ~types pat
                in
                let expr, expr_type = of_untyped ~names ~types ~f_name expr in
                Type_bindings.unify ~names ~types pat_type expr_type;
                names, ((pat, (pat_type, pat_names)), expr))
            in
            names, false, bindings)
        in
        let body, body_type = of_untyped ~names ~types ~f_name body in
        Let { rec_; bindings; body }, body_type
      | Tuple items ->
        let items, types =
          List.map items ~f:(of_untyped ~names ~types ~f_name) |> List.unzip
        in
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
        let expr, t2 = of_untyped ~names ~types ~f_name expr in
        Type_bindings.unify ~names ~types t1 t2;
        expr, t1
    and type_recursive_let_bindings ~names ~types ~f_name bindings =
      let all_bound_names =
        Nonempty.fold
          bindings
          ~init:Pattern.Names.empty
          ~f:(fun all_bound_names ((_, (_, pat_names)), _) ->
          let all_bound_names =
            Pattern.Names.merge all_bound_names pat_names ~combine:(fun ~key:_ v _ -> v)
          in
          all_bound_names)
      in
      let used_a_bound_name = ref false in
      let f_name name name_entry =
        f_name name name_entry;
        (* FIXME: phys_equal is questionable, is there a better way? Or is that not the problem?
           Could regular equality work? If not, could assign an id to name entries. *)
        match name with
        | [], name ->
          if Option.exists
               ~f:(Name_bindings.Name_entry.equal name_entry)
               (Pattern.Names.find all_bound_names name)
          then used_a_bound_name := true
        | _ :: _, _ -> ()
      in
      let bindings =
        Nonempty.map bindings ~f:(fun (((_, (pat_type, _)) as pat), expr) ->
          let expr, expr_type = of_untyped expr ~f_name ~names ~types in
          Type_bindings.unify ~names ~types pat_type expr_type;
          pat, expr)
      in
      let rec_ = !used_a_bound_name in
      names, rec_, bindings
    in
    let of_untyped ~names ~types expr =
      of_untyped ~names ~types ~f_name:(fun _ _ -> ()) expr
      |> Tuple2.map_snd ~f:(Type_bindings.substitute types)
    in
    of_untyped, type_recursive_let_bindings ~f_name:(fun _ _ -> ())
  ;;

  let rec map expr ~f ~f_type =
    match f expr with
    | `Halt expr -> expr
    | `Retry expr -> map ~f ~f_type expr
    | `Defer expr ->
      (match expr with
       | Let { rec_; bindings; body } ->
         let bindings =
           Nonempty.map bindings ~f:(fun ((pat, typ), expr) ->
             (pat, f_type typ), map ~f ~f_type expr)
         in
         let body = map ~f ~f_type body in
         Let { rec_; bindings; body }
       | (Literal _ | Name (_, _)) as expr -> expr
       | Fun_call (fun_, args) ->
         let fun_ = map ~f ~f_type fun_ in
         let args =
           Nonempty.map args ~f:(fun (arg, arg_type) ->
             map ~f ~f_type arg, f_type arg_type)
         in
         Fun_call (fun_, args)
       | Lambda (args, body) -> Lambda (args, map ~f ~f_type body)
       | Match (expr, expr_type, branches) ->
         let expr = map ~f ~f_type expr in
         Match
           ( expr
           , f_type expr_type
           , Nonempty.map branches ~f:(Tuple2.map_snd ~f:(map ~f ~f_type)) )
       | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~f_type))
       | Record_literal fields ->
         Record_literal
           (List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f ~f_type))))
       | Record_update (expr, fields) ->
         let expr = map ~f ~f_type expr in
         Record_update
           (expr, List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f ~f_type))))
       | Record_field_access (record, field) ->
         Record_field_access (map ~f ~f_type record, field))
  ;;

  let rec generalize_let_bindings ~names ~types =
    map
      ~f_type:(fun (typ, _) -> Type_bindings.generalize types typ)
      ~f:
        (function
         | Let { rec_; bindings; body } ->
           let bindings =
             Nonempty.map bindings ~f:(fun ((pat, (pat_type, pat_names)), expr) ->
               let names, scheme = Pattern.generalize ~names ~types pat_names pat_type in
               (pat, scheme), generalize_let_bindings ~names ~types expr)
           in
           let body = generalize_let_bindings ~names ~types body in
           `Halt (Let { rec_; bindings; body })
         | expr -> `Defer expr)
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.generalized) t [@@deriving sexp_of]
  type nonrec def = (Pattern.t, Expr.generalized) def [@@deriving sexp_of]

  let rec gather_names ~names ~f_common ?f_def module_name sigs defs =
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        List.fold sigs ~init:names ~f:(fun names sig_ ->
          match sig_.Node.node with
          | Common_sig common -> f_common names common
          | Module_sig (module_name, sigs) ->
            gather_names ~names module_name sigs [] ~f_common ?f_def))
    in
    let f =
      match f_def with
      | Some f_def -> f_def
      | None ->
        fun names def ->
          (match def.Node.node with
           | Common_def common -> f_common names common
           | Module (module_name, sigs, defs) ->
             gather_names ~names module_name sigs defs ~f_common ?f_def
           | Let _ | Trait _ | Impl _ -> names)
    in
    Name_bindings.with_submodule ~place:`Def names module_name ~f:(fun names ->
      List.fold defs ~init:names ~f)
  ;;

  (* Name/type resolution steps
     0. Copy type/module declarations in sigs to defs when they are missing.
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

  module Sig_data = struct
    type t =
      { type_decls : Untyped.Module.def Node.t Type_name.Map.t
      ; fixities : Fixity.t Value_name.Map.t
      }
    [@@deriving fields]

    let empty = { type_decls = Type_name.Map.empty; fixities = Value_name.Map.empty }
    let map_type_decls t ~f = { t with type_decls = f t.type_decls }
    let map_fixities t ~f = { t with fixities = f t.fixities }
  end

  let copy_some_sigs_to_defs sigs defs =
    let empty_sig_map = Nested_map.create Sig_data.empty in
    (* Copy type and module declarations to defs if they were left out *)
    let rec gather_decls ~sig_map sigs =
      List.fold sigs ~init:sig_map ~f:(fun sig_map sig_ ->
        match sig_.Node.node with
        | Common_sig common ->
          (match common with
           | Type_decl (type_name, _) ->
             let data = { sig_ with node = Common_def common } in
             (* TODO: won't these [Map.add_exn]s raise from duplicate vals in the sig? 
               We should be raising a proper compile error instead *)
             Nested_map.map
               sig_map
               ~f:(Sig_data.map_type_decls ~f:(Map.add_exn ~key:type_name ~data))
           | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
           | Val (value_name, Some fixity, _) | Extern (value_name, Some fixity, _, _) ->
             Nested_map.map
               sig_map
               ~f:(Sig_data.map_fixities ~f:(Map.add_exn ~key:value_name ~data:fixity))
           | Val (_, None, _)
           | Extern (_, None, _, _)
           (* TODO: handle imports bringing in type declarations to copy over (?) *)
           | Import _ | Import_with _ | Import_without _ -> sig_map)
        | Module_sig (module_name, sigs) ->
          Nested_map.with_module sig_map module_name ~f:(fun sig_map ->
            gather_decls ~sig_map:(Option.value sig_map ~default:empty_sig_map) sigs))
    in
    let rec copy_to_defs ~sig_map defs =
      let sig_map, defs =
        List.fold_map
          defs
          ~init:sig_map
          ~f:
            (Node.fold_map ~f:(fun sig_map def ->
               match def with
               | Common_def common ->
                 (match common with
                  | Type_decl (type_name, _) ->
                    ( Nested_map.map
                        sig_map
                        ~f:(Sig_data.map_type_decls ~f:(Fn.flip Map.remove type_name))
                    , def )
                  | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
                  | Val (value_name, None, typ) ->
                    (* Merge the fixity from the sig *)
                    let fixity =
                      Map.find (Nested_map.current sig_map).fixities value_name
                    in
                    sig_map, Common_def (Val (value_name, fixity, typ))
                  | Extern (value_name, None, typ, extern_name) ->
                    (* Merge the fixity from the sig *)
                    let fixity =
                      Map.find (Nested_map.current sig_map).fixities value_name
                    in
                    sig_map, Common_def (Extern (value_name, fixity, typ, extern_name))
                  | Val (_, Some _, _)
                  | Extern (_, Some _, _, _)
                  | Import _ | Import_with _ | Import_without _ -> sig_map, def)
               | Module (module_name, sigs, defs) ->
                 (match Nested_map.find_module sig_map module_name with
                  | Some child_map ->
                    if List.is_empty sigs
                    then
                      ( Nested_map.remove_module sig_map module_name
                      , Module (module_name, [], copy_to_defs ~sig_map:child_map defs) )
                    else
                      (* Don't copy inherited sigs from the parent over (at least for now)
                      because it's complicated *)
                      sig_map, def
                  | None -> sig_map, def)
               | Trait _ -> failwith "TODO: copy trait_sigs to defs, without overriding"
               | Let _ | Impl _ -> sig_map, def))
      in
      let defs =
        Map.fold
          (Nested_map.current sig_map).type_decls
          ~init:defs
          ~f:(fun ~key:_ ~data:def defs -> def :: defs)
      in
      (* Copy over modules and try to populate them with declarations *)
      Nested_map.fold_modules
        sig_map
        ~init:defs
        ~f:(fun ~key:module_name ~data:sig_map defs ->
        match copy_to_defs ~sig_map [] with
        | [] -> defs
        | defs' -> Node.dummy_span (Module (module_name, [], defs')) :: defs)
    in
    copy_to_defs ~sig_map:(gather_decls ~sig_map:empty_sig_map sigs) defs
  ;;

  (** Gather placeholders for all declared names and types.
      (Needed for imports of submodules to work.) *)
  let rec gather_name_placeholders ~names module_name sigs defs =
    let f_common names = function
      | Val (name, _, _) | Extern (name, _, _, _) ->
        Name_bindings.add_name_placeholder names name
      | Type_decl (type_name, _) -> Name_bindings.add_type_placeholder names type_name
      | Import _ | Import_with _ | Import_without _ | Trait_sig _ -> names
    in
    gather_names ~names module_name sigs defs ~f_common ~f_def:(fun names def ->
      match def.node with
      | Let { bindings; rec_ } ->
        assert_or_compiler_bug ~here:[%here] rec_;
        Nonempty.fold bindings ~init:names ~f:(fun names { node = pat, _; _ } ->
          Name_bindings.merge_names
            names
            (Pattern.Names.gather pat)
            ~combine:(fun name entry entry' ->
            match Name_bindings.Name_entry.type_source entry with
            | Placeholder -> entry'
            | Let_inferred | Val_declared | Extern_declared ->
              Name_bindings.name_error_msg "Duplicate name" (Value_name.to_ustring name)))
      | Module (module_name, sigs, defs) ->
        gather_name_placeholders ~names module_name sigs defs
      | Common_def common -> f_common names common
      | Trait _ | Impl _ -> failwith "TODO: trait/impl (gather_name_placeholders)")
  ;;

  (* TODO: test that type decls which import from submodules actually work *)
  (* TODO: figure out import shadowing semantics - basically, probably ban shadowing *)

  (** Gather all imported names and local type/trait declarations. *)
  let gather_imports_and_type_decls ~names sigs defs =
    gather_names ~names sigs defs ~f_common:(fun names -> function
      | Import module_name -> Name_bindings.import names module_name
      | Import_with (path, imports) -> Name_bindings.import_with names path imports
      | Import_without (path, hiding) -> Name_bindings.import_without names path hiding
      | Type_decl (type_name, decl) -> Name_bindings.add_type_decl names type_name decl
      | Trait_sig _ -> failwith "TODO: trait sigs"
      | Val _ | Extern _ -> names)
  ;;

  (** Raise an error upon finding any cycles in a given type alias. *)
  let check_cyclic_type_alias ~names name alias =
    (* TODO: can rewrite this with [Type.Expr.map] *)
    let rec loop ~names aliases_seen = function
      | Type.Expr.Type_app (name, args) ->
        let decl = Name_bindings.find_type_decl names name in
        (match decl with
         | _, Alias alias ->
           (match Hashtbl.add aliases_seen ~key:decl ~data:name with
            | `Ok -> loop ~names aliases_seen alias
            | `Duplicate ->
              Compilation_error.raise
                Type_error
                ~msg:
                  [%message
                    "Cyclic type alias"
                      (name : Type_name.Qualified.t)
                      (decl : Type.Decl.t)])
         | _ -> ());
        List.iter args ~f:(loop ~names aliases_seen)
      | Function (args, body) ->
        Nonempty.iter args ~f:(loop ~names aliases_seen);
        loop ~names aliases_seen body
      | Tuple items -> List.iter items ~f:(loop ~names aliases_seen)
      | Var _ -> ()
      | Partial_function _ -> .
    in
    let aliases_seen = Hashtbl.create (module Type.Decl) in
    let name = Name_bindings.(Path.to_module_path (current_path names)), name in
    let decl = Name_bindings.find_absolute_type_decl names name in
    Hashtbl.set aliases_seen ~key:decl ~data:name;
    loop ~names aliases_seen alias
  ;;

  type intermediate_def =
    (Pattern.t * (Type.t * Pattern.Names.t), Untyped.Expr.t) Module.def

  (** Handle all `val` and `let` statements (value bindings/type annotations).
      Also type the patterns in each let binding and assign the names fresh type
      variables. *)
  let rec handle_value_bindings ~names ~types module_name sigs defs
    : Name_bindings.t * intermediate_def Node.t list
    =
    let handle_common ~names = function
      | Val (name, fixity, typ) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_val names name fixity typ ~unify
      | Extern (name, fixity, typ, extern_name) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_extern names name fixity ([], typ) extern_name ~unify
      | Type_decl (name, (_, Alias alias)) ->
        check_cyclic_type_alias ~names name alias;
        names
      | Type_decl _ | Trait_sig _ | Import _ | Import_with _ | Import_without _ -> names
    in
    let rec handle_sigs ~names ~handle_common =
      List.fold ~init:names ~f:(fun names sig_ ->
        match sig_.Node.node with
        | Common_sig common -> handle_common ~names common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
            handle_sigs ~names ~handle_common sigs))
    in
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        handle_sigs ~names ~handle_common sigs)
    in
    let handle_bindings ~names =
      Nonempty.fold_map ~init:names ~f:(fun binding ->
        Node.fold_map binding ~f:(fun names (pat, expr) ->
          let pat_names, pat = Pattern.of_untyped_with_names ~names ~types pat in
          let names =
            (* Unify pattern bindings with local type information (e.g. val declarations) *)
            Name_bindings.merge_names names pat_names ~combine:(fun _ entry entry' ->
              let typ, typ' = Name_bindings.Name_entry.(typ entry, typ entry') in
              Type_bindings.unify ~names ~types typ typ';
              Name_bindings.Name_entry.merge entry entry')
          in
          let pat, pat_type = pat in
          names, ((pat, (pat_type, pat_names)), expr)))
    in
    Name_bindings.with_submodule' ~place:`Def names module_name ~f:(fun names ->
      List.fold_map defs ~init:names ~f:(fun def ->
        Node.fold_map def ~f:(fun names -> function
          | Let { bindings; rec_ } ->
            handle_bindings ~names bindings
            |> Tuple2.map_snd ~f:(fun bindings -> Let { rec_; bindings })
          | Module (module_name, sigs, defs) ->
            let names, defs = handle_value_bindings ~names ~types module_name sigs defs in
            names, Module (module_name, sigs, defs)
          | Common_def common as def -> handle_common ~names common, def
          | Impl _ | Trait _ -> failwith "TODO: handle_value_bindings traits/impls")))
  ;;

  (* TODO: add type annotations to these functions *)

  (** Re-group and re-order toplevel let bindings so that each group is mutually
      recursive, and the groups are given in an order appropriate for generalization.
      This is done by topologically sorting the strongly-connected components of the call
      graph (dependencies between bindings). *)
  let extract_binding_groups ~names (defs : intermediate_def Node.t list)
    : _ * (_ Call_graph.Binding.t Nonempty.t * Name_bindings.Path.t) Sequence.t
    =
    let rec gather_bindings_in_defs ~names defs acc =
      List.fold_right defs ~init:acc ~f:(gather_bindings ~names)
    and gather_bindings ~names (def : intermediate_def Node.t) (other_defs, bindings) =
      match def.node with
      | Let { bindings = bindings'; rec_ } ->
        assert_or_compiler_bug ~here:[%here] rec_;
        ( other_defs
        , Nonempty.fold_right
            bindings'
            ~init:bindings
            ~f:(fun { Node.node = ((_, (_, pat_names)) as pat), expr; span } bindings ->
            let current_path = Name_bindings.current_path names in
            let bound_names = Map.key_set pat_names in
            let used_names = Untyped.Expr.names_used ~names expr in
            ( { Call_graph.Binding.bound_names; used_names; info = pat, expr, span }
            , current_path )
            :: bindings) )
      | Module (module_name, sigs, defs) ->
        let names = Name_bindings.into_module ~place:`Def names module_name in
        let other_defs', bindings = gather_bindings_in_defs ~names defs ([], bindings) in
        ( { def with node = Module (module_name, sigs, other_defs') } :: other_defs
        , bindings )
      | Common_def _ as node -> { def with node } :: other_defs, bindings
      | Trait _ | Impl _ -> failwith "TODO: traits/impls"
    in
    let other_defs, bindings = gather_bindings_in_defs ~names defs ([], []) in
    let binding_groups =
      Call_graph.of_bindings (Sequence.of_list bindings)
      |> Call_graph.to_regrouped_bindings
    in
    other_defs, binding_groups
  ;;

  let type_binding_group ~names ~types bindings =
    (* Order bindings in the group by span, then take the first span to represent the
       whole group. This is done to get a consistent ordering among bindings. *)
    let get_span { Call_graph.Binding.info = _, _, span; _ } = span in
    let bindings =
      Nonempty.sort bindings ~compare:(Comparable.lift [%compare: Span.t] ~f:get_span)
    in
    let span = get_span (Nonempty.hd bindings) in
    let bindings, spans =
      Nonempty.fold_right
        bindings
        ~init:([], [])
        ~f:(fun { info = pat, expr, span; _ } (bindings, spans) ->
        (pat, expr) :: bindings, span :: spans)
    in
    let bindings = Nonempty.of_list_exn bindings in
    let spans = Nonempty.of_list_exn spans in
    let (_ : Name_bindings.t), rec_, bindings =
      Expr.type_recursive_let_bindings ~names ~types bindings
    in
    Nonempty.fold_map
      (Nonempty.zip_exn bindings spans)
      ~init:names
      ~f:(fun names (((pat, (pat_type, pat_names)), expr), span) ->
      let names, scheme = Pattern.generalize ~names ~types pat_names pat_type in
      (* Generalize local let bindings just after the parent binding *)
      (* TODO: Look into the concept of type variable scope - parent variables are
         getting generalized here as well, which is probably wrong *)
      let expr = Expr.generalize_let_bindings ~names ~types expr in
      names, { Node.node = pat, (expr, scheme); span })
    |> Tuple2.map_snd ~f:(fun bindings -> { Node.node = Let { rec_; bindings }; span })
  ;;

  (** Reintegrate the re-ordered binding groups from [extract_binding_groups] back into
      the AST. *)
  let reintegrate_binding_groups
    (path : Name_bindings.Path.t)
    (other_defs : def Node.t list)
    (binding_groups : (def Node.t * Name_bindings.Path.t) list)
    : def Node.t list
    =
    (* NOTE: As we disallow cross-module mutual recursion, binding groups will always be
       contained within a single module and can just be put back into the AST *)
    let binding_table = Name_bindings.Path.Table.create () in
    List.iter binding_groups ~f:(fun (def, path) ->
      Hashtbl.add_multi binding_table ~key:path ~data:def);
    let rec loop binding_table path defs =
      let defs =
        List.map
          defs
          ~f:
            (Node.map ~f:(function
              | Module (module_name, sigs, defs) ->
                let path' = Name_bindings.Path.append path module_name ~place:`Def in
                Module (module_name, sigs, loop binding_table path' defs)
              | def -> def))
      in
      (* Sort defs by span to get them back into their original order *)
      List.sort
        (Hashtbl.find_multi binding_table path @ defs)
        ~compare:(fun def def' -> Span.compare def.span def'.span)
    in
    loop binding_table path other_defs
  ;;

  (** Type-check the expressions (definitions) for each let binding and trait implementation.
      Performs let-generalization on free variables in let statement bindings.
      (Let-generalization is not currently done for local let bindings within expressions.)
      This is the final step in the type checking process. *)
  let type_defs ~names ~types module_name (defs : intermediate_def Node.t list)
    : Name_bindings.t * def Node.t list
    =
    Name_bindings.with_submodule' ~place:`Def names module_name ~f:(fun names ->
      let other_defs, binding_groups = extract_binding_groups ~names defs in
      let names, binding_groups =
        Sequence.to_list binding_groups
        |> List.fold_map ~init:names ~f:(fun names (bindings, path) ->
             (* TODO: check if this is resolving paths correctly.
              I think it will resolve in the parent, not the current module, which is
              surely wrong.
              - It also doesn't handle sigs/defs properly - it needs to know the original bindings_path *)
             Name_bindings.with_path names path ~f:(fun names ->
               let names, def = type_binding_group ~names ~types bindings in
               names, (def, path)))
      in
      let path = Name_bindings.current_path names in
      names, reintegrate_binding_groups path other_defs binding_groups)
  ;;

  let of_untyped ~names ~types (module_name, sigs, defs) =
    try
      let defs = copy_some_sigs_to_defs sigs defs in
      let names = gather_name_placeholders ~names module_name sigs defs in
      let names = gather_imports_and_type_decls ~names module_name sigs defs in
      let names, defs = handle_value_bindings ~names ~types module_name sigs defs in
      let names, defs = type_defs ~names ~types module_name defs in
      (* TODO: should check every [Val] has a corresponding [Let]. *)
      Sig_def_diff.create ~names module_name |> Sig_def_diff.raise_if_nonempty;
      Ok (names, (module_name, sigs, defs))
    with
    | Compilation_error.Compilation_error error -> Error error
    | exn ->
      let kind : Compilation_error.Kind.t =
        match exn with
        | Name_bindings.Name_error _ -> Name_error
        | Type_bindings.Type_error _ -> Type_error
        | _ -> Other
      in
      (* TODO: would be nice to transition to creating/raising compilation errors and
         then doing this handling there *)
      let msg : Sexp.t =
        match exn with
        | Type_bindings.Type_error (msg, Some (t1, t2)) ->
          (* Prevent unstable Var_ids from appearing in test output *)
          let env = Type.Param.Env_of_vars.create () in
          let handle_var = Type.Param.Env_of_vars.find_or_add env in
          let map_type t =
            Type.Expr.map t ~var:handle_var ~pf:handle_var
            |> [%sexp_of: (Type_param_name.t, Type_param_name.t) Type.Expr.t]
          in
          List
            [ Atom (Ustring.to_string msg); List [ List [ map_type t1; map_type t2 ] ] ]
        | Name_bindings.Name_error msg -> Atom (Ustring.to_string msg)
        | _ -> sexp_of_exn exn
      in
      Error (Compilation_error.create ~msg ~exn kind)
  ;;
end
