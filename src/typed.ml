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
        Name_bindings.set_scheme ~place:`Def names name scheme)
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
        of_untyped ~names:(Name_bindings.import_all ~place:`Def names path) ~types expr
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

  type intermediate_def =
    ((Pattern.t * Type.t) * Untyped.Pattern.Names.t, Untyped.Expr.t) Module.def
  [@@deriving sexp]

  let rec gather_names ~names ~f_common ?f_def sigs defs =
    let names =
      List.fold sigs ~init:names ~f:(fun names sig_ ->
        match sig_.Node.node with
        | Common_sig common -> f_common ~place:`Sig names common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
            gather_names ~names sigs [] ~f_common ?f_def))
    in
    let f =
      match f_def with
      | Some f_def -> f_def
      | None ->
        fun names def ->
          (match def.Node.node with
          | Common_def common -> f_common ~place:`Def names common
          | Module (module_name, sigs, defs) ->
            Name_bindings.with_submodule ~place:`Def names module_name ~f:(fun names ->
              gather_names ~names sigs defs ~f_common ?f_def)
          | Let _ | Trait _ | Impl _ -> names)
    in
    List.fold defs ~init:names ~f
  ;;

  (* Name/type resolution steps
     0. Copy type/module declarations in sigs to defs when they are missing.
        TODO: is this a reasonable time to check sig/def compatibility? Probably too early
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

  module Nested_map : sig
    type 'data t

    val empty : 'data t
    val add_exn : 'data t -> key:Type_name.t -> data:'data -> 'data t
    val remove : 'data t -> Type_name.t -> 'data t
    val fold : 'data t -> init:'a -> f:(key:Type_name.t -> data:'data -> 'a -> 'a) -> 'a
    val with_module : 'data t -> Module_name.t -> f:('data t -> 'data t) -> 'data t
    val remove_module : 'data t -> Module_name.t -> 'data t

    val fold_modules
      :  'data t
      -> init:'a
      -> f:(key:Module_name.t -> data:'data t -> 'a -> 'a)
      -> 'a
  end = struct
    type 'data t =
      { data : 'data Type_name.Map.t
      ; modules : 'data t Module_name.Map.t
      }

    let empty = { data = Type_name.Map.empty; modules = Module_name.Map.empty }
    let add_exn t ~key ~data = { t with data = Map.add_exn t.data ~key ~data }
    let remove t key = { t with data = Map.remove t.data key }
    let fold t = Map.fold t.data

    let with_module t module_name ~f =
      { t with
        modules = Map.update t.modules module_name ~f:(f << Option.value ~default:empty)
      }
    ;;

    let remove_module t module_name =
      { t with modules = Map.remove t.modules module_name }
    ;;

    let fold_modules t = Map.fold t.modules
  end

  let copy_some_sigs_to_defs sigs defs =
    (* Copy type and module declarations to defs if they were left out *)
    let rec gather_decls sig_map sigs =
      List.fold sigs ~init:sig_map ~f:(fun sig_map sig_ ->
        match sig_.Node.node with
        | Common_sig common ->
          (match common with
          | Type_decl (type_name, _) ->
            let data = { sig_ with node = Common_def common } in
            Nested_map.add_exn sig_map ~key:type_name ~data
          | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
          | Val _
          | Extern _
          (* TODO: handle imports bringing in type declarations to copy over (?) *)
          | Import _
          | Import_with _
          | Import_without _ -> sig_map)
        | Module_sig (module_name, sigs) ->
          Nested_map.with_module sig_map module_name ~f:(fun sig_map ->
            gather_decls sig_map sigs))
    in
    let rec copy_to_defs sig_map defs =
      let sig_map, defs =
        List.fold_map
          defs
          ~init:sig_map
          ~f:
            (Node.fold_map ~f:(fun sig_map def ->
               match def with
               | Common_def common ->
                 (match common with
                 | Type_decl (type_name, _) -> Nested_map.remove sig_map type_name, def
                 | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
                 | Val _ | Extern _ | Import _ | Import_with _ | Import_without _ ->
                   sig_map, def)
               | Module (module_name, sigs, defs) ->
                 (* TODO: might want to handle this a bit differently *)
                 let sig_map = Nested_map.remove_module sig_map module_name in
                 if List.is_empty sigs
                 then sig_map, Module (module_name, sigs, copy_to_defs sig_map defs)
                 else
                   (* Don't copy inherited sigs from the parent over for now because it's
                      complicated *)
                   sig_map, def
               | Trait _ -> failwith "TODO: copy trait_sigs to defs, without overriding"
               | Let _ | Impl _ -> sig_map, def))
      in
      let defs =
        Nested_map.fold sig_map ~init:defs ~f:(fun ~key:_ ~data:def defs -> def :: defs)
      in
      (* Copy over modules and try to populate them with declarations *)
      Nested_map.fold_modules
        sig_map
        ~init:defs
        ~f:(fun ~key:module_name ~data:sig_map defs ->
        Node.dummy_span (Module (module_name, [], copy_to_defs sig_map [])) :: defs)
    in
    copy_to_defs (gather_decls Nested_map.empty sigs) defs
  ;;

  let rec gather_name_placeholders ~names sigs defs =
    let f_common ~place names = function
      | Val (name, _, _) | Extern (name, _, _, _) ->
        (* FIXME: PROBLEM: adding placeholders has been broken all along
           - Vals were being skipped inside defs (no common_defs were processed)
           - This hid the fact that vals were not being processed first and so placeholder
             handling was wrong
           - Kinda want to get rid of placeholder
           - Need a centralized place to do 1 val, 1 let checking
             Seems fine to do it here and then have that be an invariant (?)

           SOLUTION (probably):
           - Can maybe just do Let_inferred/Val_declared and then error in a consistent
             way - which should be handled in Name_bindings if possible
           - Wait, adding placeholders for vals in defs actually seems pointless?
             It matters in sigs, but defs have to have the let anyway, the val can't live
             alone (although we aren't checking that yet I think)
             - yeah, just skip vals in defs (ah wait, if the sigs are empty they matter)
                - ok then

            NOTE: this should be fixed now *)
        Name_bindings.add_name_placeholder ~place names name
      | Type_decl (type_name, _) ->
        Name_bindings.add_type_placeholder ~place names type_name
      | Import _ | Import_with _ | Import_without _ | Trait_sig _ -> names
    in
    gather_names ~names sigs defs ~f_common ~f_def:(fun names def ->
      match def.Node.node with
      | Let bindings ->
        List.fold bindings ~init:names ~f:(fun names { node = pat, _; _ } ->
          Name_bindings.merge_names
            names
            (Untyped.Pattern.Names.gather pat)
            ~combine:(fun name entry1 entry2 ->
            match Name_bindings.Name_entry.(type_source entry1, type_source entry2) with
            | Val_declared, Val_declared | Let_inferred, Let_inferred ->
              Name_bindings.name_error_msg "Duplicate name" (Value_name.to_ustring name)
            | _ -> entry2))
      | Module (module_name, sigs, defs) ->
        (* FIXME: maybe don't look at submodule defs unless there's no sig? 
           Need to handle cross-module privacy properly.
           In saying that, we still need to look at these inner defs at some point.
           We need to look at both sigs and defs, right?
           - this arg just affects whether we prioritize following sigs or defs to get to
             the child *)
        Name_bindings.with_submodule ~place:`Def names module_name ~f:(fun names ->
          gather_name_placeholders ~names sigs defs)
      | Common_def common -> f_common ~place:`Def names common
      | Trait _ | Impl _ -> failwith "TODO: trait/impl (gather_name_placeholders)")
  ;;

  (* TODO: test that type decls which import from submodules actually work *)
  (* TODO: figure out import shadowing semantics - basically, probably ban shadowing *)
  let gather_imports_and_type_decls ~names sigs defs =
    gather_names ~names sigs defs ~f_common:(fun ~place names -> function
      | Import module_name -> Name_bindings.import ~place names module_name
      | Import_with (path, imports) -> Name_bindings.import_with ~place names path imports
      | Import_without (path, hiding) ->
        Name_bindings.import_without ~place names path hiding
      | Type_decl (type_name, decl) ->
        Name_bindings.add_type_decl ~place names type_name decl
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
            raise_s
              [%message
                "Cyclic type alias" (name : Type_name.Qualified.t) (decl : Type.Decl.t)])
        | _ -> ());
        List.iter args ~f:(loop ~names aliases_seen)
      | Function (f, arg) ->
        loop ~names aliases_seen f;
        loop ~names aliases_seen arg
      | Tuple items -> List.iter items ~f:(loop ~names aliases_seen)
      | Var _ -> ()
    in
    let aliases_seen = Hashtbl.create (module Type.Decl) in
    let name = Name_bindings.current_path names, name in
    let decl = Name_bindings.find_absolute_type_decl names name in
    Hashtbl.set aliases_seen ~key:decl ~data:name;
    loop ~names aliases_seen alias
  ;;

  let rec handle_value_bindings ~names ~types sigs defs =
    let handle_common ~names ~place = function
      | Val (name, fixity, typ) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_val ~place names name fixity typ ~unify
      | Extern (name, fixity, typ, extern_name) ->
        let unify = Type_bindings.unify ~names ~types in
        Name_bindings.add_val ~place names name fixity ([], typ) ~extern_name ~unify
      | Type_decl (name, (_, Alias alias)) ->
        check_cyclic_type_alias ~names name alias;
        names
      | Type_decl _ | Trait_sig _ | Import _ | Import_with _ | Import_without _ -> names
    in
    let rec handle_sigs ~names ~handle_common =
      List.fold ~init:names ~f:(fun names sig_ ->
        match sig_.Node.node with
        | Common_sig common -> handle_common ~names ~place:`Sig common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
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
               Name_bindings.with_submodule'
                 ~place:`Def
                 names
                 module_name
                 ~f:(fun names -> handle_value_bindings ~names ~types sigs defs)
             in
             names, Module (module_name, sigs, defs)
           | Common_def common as def -> handle_common ~names ~place:`Def common, def
           | Impl _ | Trait _ -> failwith "TODO: handle_value_bindings traits/impls"))
  ;;

  (** Re-group and re-order toplevel let bindings so that each group is mutually
      recursive, and the groups are given in an order appropriate for generalization.
      This is done by topologically sorting the strongly-connected components of the call
      graph (dependencies between bindings). *)
  let extract_binding_groups ~names defs =
    let rec gather_bindings_in_defs ~names defs acc =
      List.fold_right defs ~init:acc ~f:(gather_bindings ~names)
    and gather_bindings ~names def (other_defs, bindings) =
      match def.Node.node with
      | Let bindings' ->
        ( other_defs
        , List.fold_right
            bindings'
            ~init:bindings
            ~f:(fun { Node.node = ((_, pat_names) as pat), expr; span } bindings ->
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
      (* TODO: what's the point in using Sequence? *)
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
              | def -> def))
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
           Name_bindings.with_path names path ~f:(fun names ->
             let names, def = type_binding_group ~names ~types bindings in
             names, (def, path)))
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
      let defs = copy_some_sigs_to_defs sigs defs in
      let names = Name_bindings.into_module ~place:`Def names module_name in
      print_s [%message "starting" (Name_bindings.without_std names : Name_bindings.t)];
      let names = gather_name_placeholders ~names sigs defs in
      print_s
        [%message
          "after gathering placeholders"
            (Name_bindings.without_std names : Name_bindings.t)];
      let names = gather_imports_and_type_decls ~names sigs defs in
      print_s
        [%message
          "after gathering imports/type decls"
            (Name_bindings.without_std names : Name_bindings.t)];
      let names, defs = handle_value_bindings ~names ~types sigs defs in
      print_s
        [%message
          "after handling value bindings"
            (Name_bindings.without_std names : Name_bindings.t)];
      let names, defs = type_defs ~names ~types defs in
      print_s
        [%message "after typing defs" (Name_bindings.without_std names : Name_bindings.t)];
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
