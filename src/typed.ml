open Import
open Names

let type_error_msg msg = Compilation_error.raise Type_error ~msg:[%message msg]

module Pattern = struct
  include Pattern

  type nonrec t = (Nothing.t, Module_path.absolute) t [@@deriving sexp]

  let of_untyped_with_names ~names ~types (pat : Untyped.Pattern.t)
    : Names.t * (t * Internal_type.t)
    =
    let rec of_untyped_with_names ~names ~types pat_names
      : Untyped.Pattern.t -> Names.t * (t * Internal_type.t)
      = function
      | Constant lit ->
        ( pat_names
        , ( Constant lit
          , Type_bindings.instantiate_type_scheme ~names ~types (Literal.typ lit) ) )
      | Catch_all name ->
        let pat_names, typ =
          match name with
          | Some name ->
            Pattern.Names.add_fresh_name pat_names name ~type_source:Placeholder
          | None -> pat_names, Internal_type.fresh_var ()
        in
        pat_names, (Catch_all name, typ)
      | Cnstr_appl (cnstr, args) ->
        (* TODO: inferring unqualified name given type information *)
        let expected_arg_types, body_type =
          let cnstr_type =
            Name_bindings.find_cnstr_type names cnstr
            |> Type_bindings.instantiate_type_or_scheme ~names ~types
          in
          match cnstr_type with
          | Function (arg_types, _effect, body_type) ->
            Nonempty.to_list arg_types, body_type
          | body_type -> [], body_type
        in
        (match
           List.fold2
             expected_arg_types
             args
             ~init:(pat_names, [])
             ~f:(fun (pat_names, args) expected_arg_type arg ->
             let pat_names, (arg, actual_arg_type) =
               of_untyped_with_names ~names ~types pat_names arg
             in
             Type_bindings.constrain
               ~names
               ~types
               ~subtype:actual_arg_type
               ~supertype:expected_arg_type;
             pat_names, arg :: args)
         with
         | Ok (pat_names, args) ->
           let cnstr =
             Name_bindings.absolutify_value_name
               names
               (Value_name.Qualified.of_cnstr_name cnstr)
             |> Value_name.Qualified.to_cnstr_name
             |> ok_or_compiler_bug ~here:[%here]
           in
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
        let result_type = Internal_type.fresh_var () in
        Type_bindings.constrain ~names ~types ~subtype:typ1 ~supertype:result_type;
        Type_bindings.constrain ~names ~types ~subtype:typ2 ~supertype:result_type;
        (* Unions must define the same names with compatible types. *)
        if not
             (Map.equal
                (fun entry1 entry2 ->
                  let entry_type = Internal_type.fresh_var () in
                  Type_bindings.constrain'
                    ~names
                    ~types
                    ~subtype:(Name_bindings.Name_entry.type_ entry1)
                    ~supertype:(Type entry_type);
                  Type_bindings.constrain'
                    ~names
                    ~types
                    ~subtype:(Name_bindings.Name_entry.type_ entry2)
                    ~supertype:(Type entry_type);
                  true)
                pat_names1
                pat_names2)
        then type_error_msg "Pattern unions must define the same names";
        pat_names1, (Union (pat1, pat2), result_type)
      | As (pat, name) ->
        let pat_names, (pat, typ) = of_untyped_with_names ~names ~types pat_names pat in
        let pat_names =
          Pattern.Names.add_name pat_names name typ ~type_source:Placeholder
        in
        pat_names, (As (pat, name), typ)
      | Type_annotation (pat, ((_ : Trait_bound.t), annotated_type)) ->
        (* TODO: Handle trait bounds for type annotations, once traits are implemented. *)
        (* FIXME: I think we need to propagate relevant constraints from the parent. *)
        let annotated_type =
          Type_bindings.instantiate_type_scheme
            ~names
            ~types
            (Type_scheme.map'
               annotated_type
               ~type_name:(Name_bindings.absolutify_type_name names)
               ~effect_name:(Name_bindings.absolutify_effect_name names))
        in
        let pat_names, (pat, inferred_type) =
          of_untyped_with_names ~names ~types pat_names pat
        in
        Type_bindings.constrain
          ~names
          ~types
          ~subtype:inferred_type
          ~supertype:annotated_type;
        pat_names, (pat, annotated_type)
    in
    let pat_names, (pat, typ) =
      of_untyped_with_names ~names ~types Value_name.Map.empty pat
    in
    (* FIXME: Do we need to substitute here? What effect does it have? I'm going to get
       rid of it for now. Previous code was:
       `pat_names, (pat, Type_bindings.substitute types typ)` *)
    pat_names, (pat, typ)
  ;;

  let of_untyped_into ~names ~types pattern =
    let ((pat_names, _) as pat) = of_untyped_with_names ~names ~types pattern in
    let names =
      Name_bindings.merge_names names pat_names ~combine:(fun _ _ new_entry -> new_entry)
    in
    names, pat
  ;;

  let generalize ~names ~types pat_names typ ~shadowing_allowed =
    (* FIXME: cleanup *)
    eprint_s
      [%message
        "Pattern.generalize"
          (pat_names : Pattern.Names.t)
          (typ : Internal_type.t)
          (types : Type_bindings.t)];
    let names =
      Map.fold pat_names ~init:names ~f:(fun ~key:name ~data:entry names ->
        let inferred_scheme =
          match Name_bindings.Name_entry.type_ entry with
          | Scheme scheme -> scheme
          | Type type_ -> Type_bindings.generalize types type_
        in
        Name_bindings.set_inferred_scheme
          names
          name
          inferred_scheme
          ~shadowing_allowed
          ~check_existing:(fun existing_entry ->
          match Name_bindings.Name_entry.type_ existing_entry with
          | Type _ -> ()
          | Scheme existing_scheme ->
            Sig_def_diff.check_val_scheme_vs_inferred_scheme
              ~names
              ~val_scheme:existing_scheme
              ~inferred_scheme))
    in
    names, Type_bindings.generalize types typ
  ;;
end

module Expr = struct
  type 'typ t =
    | Literal of Literal.t
    | Name of Value_name.Absolute.t
    | Fun_call of 'typ t Node.t * 'typ * ('typ t Node.t * 'typ) Nonempty.t
    | Lambda of Pattern.t Node.t Nonempty.t * 'typ t Node.t
    | Match of 'typ t Node.t * 'typ * (Pattern.t Node.t * 'typ t Node.t) Nonempty.t
    | Let of (Pattern.t * 'typ, 'typ t) Let_binding.t
    | Tuple of 'typ t Node.t list
    | Record_literal of (Value_name.t * 'typ t Node.t option) list
    | Record_update of 'typ t Node.t * (Value_name.t * 'typ t Node.t option) list
    | Record_field_access of 'typ t Node.t * Value_name.t Node.t
  [@@deriving sexp]

  type generalized =
    Module_path.absolute Type_scheme.t t * Module_path.absolute Type_scheme.t
  [@@deriving sexp_of]

  let type_recursive_let_bindings =
    let collect_effects ~names ~types f =
      let effects : Internal_type.effects =
        { effects = Effect_name.Absolute.Map.empty
        ; effect_var = Some (Type_var.create ())
        }
      in
      let (expr : _ t Node.t), (typ : Internal_type.t) =
        f ~add_effects:(fun subtype ->
          Type_bindings.constrain_effects ~names ~types ~subtype ~supertype:effects)
      in
      expr, typ, effects
    in
    let rec of_untyped ~names ~types ~f_name expr
      : (Internal_type.t * Pattern.Names.t) t Node.t
        * Internal_type.t
        * Internal_type.effects
      =
      (* FIXME: cleanup *)
      (* print_s [%message "Typed.Expr.of_untyped" (expr : Untyped.Expr.t Node.t)]; *)
      let node e = Node.create e (Node.span expr) in
      Node.with_value expr ~f:(fun expr ->
        match (expr : Untyped.Expr.t) with
        | Literal lit ->
          ( node (Literal lit)
          , Type_bindings.instantiate_type_scheme ~names ~types (Literal.typ lit)
          , Internal_type.no_effects )
        | Name name ->
          let name, name_entry = Name_bindings.find_entry_with_path names name in
          f_name name name_entry;
          let type_ =
            Name_bindings.Name_entry.type_ name_entry
            |> Type_bindings.instantiate_type_or_scheme ~names ~types
          in
          node (Name name), type_, Internal_type.no_effects
        | Qualified (path, expr) ->
          let names = Name_bindings.import_all names path in
          of_untyped ~names ~types ~f_name expr
        | Fun_call (fun_, args) ->
          (* FIXME: IDEA: We want to come up with a type representing all the effects
           potentially produced by this expression. This is a supertype of effects
           produced by individual components of the expression:
           In this case:
           - The function expression
           - Each function argument expression
           - The effects produced by the call itself
           
           We get to this supertype by unioning the effects of the subexpressions. We
           don't want to unify the effect variables because we actually want a supertype.
           If you have e.g. `if cond then foo else bar` we don't want to force `foo` and
           `bar` to produce exactly the same effects. Effects have true automatic
           subtyping like that. *)
          collect_effects ~names ~types (fun ~add_effects ->
            let fun_, fun_type, fun_effects = of_untyped ~names ~types ~f_name fun_ in
            add_effects fun_effects;
            let args =
              Nonempty.map args ~f:(fun arg ->
                let arg, arg_type, arg_effects = of_untyped ~names ~types ~f_name arg in
                add_effects arg_effects;
                arg, (arg_type, Pattern.Names.empty))
            in
            let arg_types = Nonempty.map args ~f:(fun (_, (arg_type, _)) -> arg_type) in
            let result_var = Type_var.create () in
            let call_effects : Internal_type.effects =
              { effects = Effect_name.Absolute.Map.empty
              ; effect_var = Some (Type_var.create ())
              }
            in
            add_effects call_effects;
            Type_bindings.constrain
              ~names
              ~types
              ~subtype:fun_type
              ~supertype:(Partial_function (arg_types, call_effects, result_var));
            node (Fun_call (fun_, (fun_type, Pattern.Names.empty), args)), Var result_var)
        | Op_tree tree ->
          of_untyped ~names ~types ~f_name (Op_tree.to_untyped_expr ~names tree)
        | Lambda (args, body) ->
          let names, args_and_types =
            Nonempty.fold_map args ~init:names ~f:(fun names arg ->
              let span = Node.span arg in
              let names, ((_ : Pattern.Names.t), (arg, arg_type)) =
                Node.with_value arg ~f:(Pattern.of_untyped_into ~names ~types)
              in
              names, (Node.create arg span, arg_type))
          in
          let args, arg_types = Nonempty.unzip args_and_types in
          let body, body_type, body_effects = of_untyped ~names ~types ~f_name body in
          ( node (Lambda (args, body))
          , Function (arg_types, body_effects, body_type)
          , Internal_type.no_effects )
        | If (cond, then_, else_) ->
          (* FIXME: Places with multiple branches e.g. if, match, now can't rely on the
           types of the two branches being unified to be equivalent, and arbitrarily
           picking one. They need to get the supertype (for e.g functions). *)
          collect_effects ~names ~types (fun ~add_effects ->
            let cond, cond_type, cond_effects = of_untyped ~names ~types ~f_name cond in
            add_effects cond_effects;
            let bool_type =
              Type_bindings.instantiate_type_scheme ~names ~types Intrinsics.Bool.typ
            in
            Type_bindings.constrain ~names ~types ~subtype:cond_type ~supertype:bool_type;
            let (then_, then_type, then_effects), (else_, else_type, else_effects) =
              ( of_untyped ~names ~types ~f_name then_
              , of_untyped ~names ~types ~f_name else_ )
            in
            add_effects then_effects;
            add_effects else_effects;
            let result_type = Internal_type.fresh_var () in
            Type_bindings.constrain
              ~names
              ~types
              ~subtype:then_type
              ~supertype:result_type;
            Type_bindings.constrain
              ~names
              ~types
              ~subtype:else_type
              ~supertype:result_type;
            let branch name expr =
              Node.create (Cnstr_appl (name, []) : Pattern.t) (Node.span expr), expr
            in
            ( node
                (Match
                   ( cond
                   , (bool_type, Pattern.Names.empty)
                   , [ branch Intrinsics.Bool.true_ then_
                     ; branch Intrinsics.Bool.false_ else_
                     ] ))
            , result_type ))
        | Match (expr, branches) ->
          collect_effects ~names ~types (fun ~add_effects ->
            let expr, expr_type, expr_effects = of_untyped ~names ~types ~f_name expr in
            add_effects expr_effects;
            let result_type = Internal_type.fresh_var () in
            let branches =
              Nonempty.map branches ~f:(fun (pat, branch) ->
                let pat_span = Node.span pat in
                let names, ((_ : Pattern.Names.t), (pat, pat_type)) =
                  Node.with_value pat ~f:(Pattern.of_untyped_into ~names ~types)
                in
                Type_bindings.constrain
                  ~names
                  ~types
                  ~subtype:expr_type
                  ~supertype:pat_type;
                let branch, branch_type, branch_effects =
                  of_untyped ~names ~types ~f_name branch
                in
                add_effects branch_effects;
                Type_bindings.constrain
                  ~names
                  ~types
                  ~subtype:branch_type
                  ~supertype:result_type;
                Node.create pat pat_span, branch)
            in
            node (Match (expr, (expr_type, Pattern.Names.empty), branches)), result_type)
        | Let { rec_; bindings; body } ->
          collect_effects ~names ~types (fun ~add_effects ->
            let names, rec_, bindings =
              if rec_
              then (
                let names, bindings =
                  Nonempty.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
                    let pat_span = Node.span pat in
                    let names, (pat_names, (pat, pat_type)) =
                      Node.with_value pat ~f:(Pattern.of_untyped_into ~names ~types)
                    in
                    names, (Node.create (pat, (pat_type, pat_names)) pat_span, expr))
                in
                type_recursive_let_bindings ~names ~types ~f_name ~add_effects bindings)
              else (
                (* Process bindings in order without any recursion *)
                let names, bindings =
                  Nonempty.fold_map bindings ~init:names ~f:(fun names (pat, expr) ->
                    let pat_span = Node.span pat in
                    let names, (pat_names, (pat, pat_type)) =
                      Node.with_value pat ~f:(Pattern.of_untyped_into ~names ~types)
                    in
                    let expr, expr_type, expr_effects =
                      of_untyped ~names ~types ~f_name expr
                    in
                    add_effects expr_effects;
                    Type_bindings.constrain
                      ~names
                      ~types
                      ~subtype:expr_type
                      ~supertype:pat_type;
                    names, (Node.create (pat, (pat_type, pat_names)) pat_span, expr))
                in
                names, false, bindings)
            in
            let body, body_type, body_effects = of_untyped ~names ~types ~f_name body in
            add_effects body_effects;
            node (Let { rec_; bindings; body }), body_type)
        | Tuple items ->
          collect_effects ~names ~types (fun ~add_effects ->
            let items, types =
              List.map items ~f:(fun item ->
                let item, type_, effects = of_untyped item ~names ~types ~f_name in
                add_effects effects;
                item, type_)
              |> List.unzip
            in
            node (Tuple items), Tuple types)
        | Seq_literal _items -> failwith "TODO: seq"
        | Record_literal _fields -> failwith "TODO: record1"
        | Record_update (_expr, _fields) -> failwith "TODO: record2"
        | Record_field_access (_record, _name) -> failwith "TODO: record3"
        | Type_annotation (expr, annotated_type) ->
          (* TODO: Handle trait bounds for type annotations, once traits are implemented. *)
          let annotated_type =
            Node.with_value
              annotated_type
              ~f:(fun ((_ : Trait_bound.t), annotated_type) ->
              Type_bindings.instantiate_type_scheme
                ~names
                ~types
                (Type_scheme.map'
                   annotated_type
                   ~type_name:(Name_bindings.absolutify_type_name names)
                   ~effect_name:(Name_bindings.absolutify_effect_name names)))
          in
          let expr, inferred_type, expr_effects = of_untyped ~names ~types ~f_name expr in
          Type_bindings.constrain
            ~names
            ~types
            ~subtype:inferred_type
            ~supertype:annotated_type;
          expr, annotated_type, expr_effects)
    and type_recursive_let_bindings ~names ~types ~f_name ~add_effects bindings =
      let all_bound_names =
        Nonempty.fold
          bindings
          ~init:Pattern.Names.empty
          ~f:(fun all_bound_names (pattern_etc, _expr) ->
          Node.with_value pattern_etc ~f:(fun (_, (_, pat_names)) ->
            let all_bound_names =
              Pattern.Names.merge all_bound_names pat_names ~combine:(fun ~key:_ v _ -> v)
            in
            all_bound_names))
      in
      let used_a_bound_name = ref false in
      let current_path = Name_bindings.current_path names in
      let f_name name name_entry =
        f_name name name_entry;
        let path, name = name in
        if Module_path.Absolute.equal path current_path
           && Option.exists
                (Pattern.Names.find all_bound_names name)
                ~f:(Name_bindings.Name_entry.identical name_entry)
        then used_a_bound_name := true
      in
      let bindings =
        Nonempty.map bindings ~f:(fun (pat, expr) ->
          let expr, expr_type, expr_effects = of_untyped expr ~f_name ~names ~types in
          add_effects expr_effects;
          Node.with_value pat ~f:(fun (_, (pat_type, _)) ->
            Type_bindings.constrain ~names ~types ~subtype:expr_type ~supertype:pat_type);
          pat, expr)
      in
      let rec_ = !used_a_bound_name in
      names, rec_, bindings
    in
    type_recursive_let_bindings ~f_name:(fun _ _ -> ())
  ;;

  let rec map expr ~f ~f_type =
    match f expr with
    | `Halt expr -> expr
    | `Retry expr -> map ~f ~f_type expr
    | `Defer expr ->
      (match expr with
       | Let { rec_; bindings; body } ->
         let bindings =
           Nonempty.map bindings ~f:(fun (pat_and_type, expr) ->
             ( Node.map pat_and_type ~f:(fun (pat, typ) -> pat, f_type typ)
             , map' expr ~f ~f_type ))
         in
         let body = map' body ~f ~f_type in
         Let { rec_; bindings; body }
       | (Literal _ | Name (_, _)) as expr -> expr
       | Fun_call (fun_, fun_type, args) ->
         let fun_ = map' fun_ ~f ~f_type in
         let fun_type = f_type fun_type in
         let args =
           Nonempty.map args ~f:(fun (arg, arg_type) ->
             map' arg ~f ~f_type, f_type arg_type)
         in
         Fun_call (fun_, fun_type, args)
       | Lambda (args, body) -> Lambda (args, map' body ~f ~f_type)
       | Match (expr, expr_type, branches) ->
         let expr = map' expr ~f ~f_type in
         Match
           ( expr
           , f_type expr_type
           , Nonempty.map branches ~f:(Tuple2.map_snd ~f:(map' ~f ~f_type)) )
       | Tuple fields -> Tuple (List.map fields ~f:(map' ~f ~f_type))
       | Record_literal fields ->
         Record_literal
           (List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map' ~f ~f_type))))
       | Record_update (expr, fields) ->
         let expr = map' ~f ~f_type expr in
         Record_update
           (expr, List.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map' ~f ~f_type))))
       | Record_field_access (record, field) ->
         Record_field_access (map' ~f ~f_type record, field))

  and map' expr ~f ~f_type = Node.map expr ~f:(map ~f ~f_type)

  (* FIXME: Is this supposed to be generalizing local let bindings in expressions? If so,
     it doesn't seem to work, according to the Generalization.um test. *)
  let rec generalize_let_bindings ~names ~types =
    map
      ~f_type:(fun (typ, _) -> Type_bindings.generalize types typ)
      ~f:(function
        | Let { rec_; bindings; body } ->
          let bindings =
            Nonempty.map bindings ~f:(fun (pattern_etc, expr) ->
              let pat_span = Node.span pattern_etc in
              let pat, (names, scheme) =
                Node.with_value pattern_etc ~f:(fun (pat, (pat_type, pat_names)) ->
                  ( pat
                  , Pattern.generalize
                      ~names
                      ~types
                      pat_names
                      pat_type
                      ~shadowing_allowed:true ))
              in
              ( Node.create (pat, scheme) pat_span
              , Node.map expr ~f:(generalize_let_bindings ~names ~types) ))
          in
          let body = Node.map body ~f:(generalize_let_bindings ~names ~types) in
          `Halt (Let { rec_; bindings; body })
        | expr -> `Defer expr)
  ;;
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.generalized, Module_path.absolute) t
  [@@deriving sexp_of]

  type nonrec def = (Pattern.t, Expr.generalized, Module_path.absolute) def
  [@@deriving sexp_of]

  let rec gather_names ~names ~f_common ?f_def module_name sigs defs =
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        List.fold sigs ~init:names ~f:(fun names sig_ ->
          Node.with_value sig_ ~f:(function
            | Common_sig common -> f_common names common
            | Module_sig (module_name, sigs) ->
              gather_names ~names module_name sigs [] ~f_common ?f_def)))
    in
    let f =
      match f_def with
      | Some f_def -> f_def
      | None ->
        fun names def ->
          Node.with_value def ~f:(function
            | Common_def common -> f_common names common
            | Module (module_name, sigs, defs) ->
              gather_names ~names module_name sigs defs ~f_common ?f_def
            | Let _ | Trait _ | Impl _ -> names)
    in
    Name_bindings.with_submodule ~place:`Def names module_name ~f:(fun names ->
      List.fold defs ~init:names ~f)
  ;;

  module Sig_data : sig
    (** Represents data from a module signature that might want to be copied to its
        definition. Currently we only copy type definitions, but you could imagine copying
        more things like fixities from val/extern declarations, or just the whole
        val/extern declaration. If we were to implicitly copy mmore, we would have to
        think about the interaction with sig/def diffs and error messages. Keeping sigs
        and defs mostly separate is simpler to think about. *)
    type t

    val empty : t
    val add_type_decl : t -> type_name:Type_name.t -> def:Untyped.Module.def Node.t -> t
    val remove_type_decl : t -> Type_name.t -> t

    val fold_defs
      :  t
      -> init:'acc
      -> f:('acc -> Untyped.Module.def Node.t -> 'acc)
      -> 'acc
  end = struct
    type t = { type_decls : Untyped.Module.def Node.t Type_name.Map.t }
    [@@deriving fields]

    let empty = { type_decls = Type_name.Map.empty }

    let add_internal map ~key ~data =
      match Map.add map ~key ~data with
      | `Ok map -> map
      | `Duplicate -> compiler_bug [%message "Sig_data.add: duplicate value"]
    ;;

    let add_type_decl { type_decls } ~type_name ~def =
      { type_decls = add_internal type_decls ~key:type_name ~data:def }
    ;;

    let remove_type_decl { type_decls } type_name =
      { type_decls = Map.remove type_decls type_name }
    ;;

    let fold_defs { type_decls } ~init ~f =
      Map.fold type_decls ~init ~f:(fun ~key:_ ~data:def acc -> f acc def)
    ;;
  end

  (* Copy type and module declarations to defs if they were left out *)
  (* TODO: A problem with just copying values over from the sigs to the defs textually is
     that they might refer to different things due to imports. It's also just kind of
     re-implementing the features of [Name_bindings]. Doing the checks at the lookup stage
     does seem like a recipe for pain though. Maybe we could do this step *after*
     resolving imports and absolutifying everything? Actually, that would mess with our
     ability to know what names exist where before resolving imports, so it wouldn't
     quite work. Maybe we could include the values as placeholders in the def, and then
      fill their actual values in after resolving imports? Pretty complicated. *)
  let rec copy_some_sigs_to_defs sigs defs =
    let rec gather_decls ~sig_map sigs =
      List.fold sigs ~init:sig_map ~f:(fun sig_map sig_ ->
        Node.with_value sig_ ~f:(function
          | Common_sig common ->
            (match common with
             | Type_decl (type_name, _) ->
               let def = Node.set sig_ (Common_def common) in
               Nested_map.map sig_map ~f:(Sig_data.add_type_decl ~type_name ~def)
             | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
             (* TODO: Implement effect copying from sigs to defs *)
             | Effect _
             | Val _
             | Extern _
             (* We could consider handling imports bringing in type declarations to copy
                over, but the cost-benefit of this feature isn't clear right now. *)
             | Import _ -> sig_map)
          | Module_sig (module_name, sigs) ->
            Nested_map.with_module sig_map module_name ~f:(fun sig_map ->
              gather_decls
                ~sig_map:
                  (Option.value sig_map ~default:(Nested_map.create Sig_data.empty))
                sigs)))
    in
    let rec copy_to_defs ~sig_map defs =
      (* We want to add missing information from the sig to the def e.g. type declarations.
         This is done by removing matching entries from [sig_map] when found, then adding
         the remaining ones at the end. *)
      let sig_map, defs =
        List.fold_map defs ~init:sig_map ~f:(fun sig_map defs ->
          Node.fold_map sig_map defs ~f:(fun sig_map def ->
            match def with
            | Common_def common ->
              (match common with
               | Type_decl (type_name, _) ->
                 ( Nested_map.map sig_map ~f:(Fn.flip Sig_data.remove_type_decl type_name)
                 , def )
               | Effect _ ->
                 (* TODO: Copy effect declarations to defs *)
                 sig_map, def
               | Trait_sig _ -> failwith "TODO: copy trait_sigs to defs"
               | Val _ | Extern _ | Import _ -> sig_map, def)
            | Module (module_name, sigs, defs) ->
              (match Nested_map.find_module sig_map module_name with
               | Some child_map ->
                 let sig_map = Nested_map.remove_module sig_map module_name in
                 if List.is_empty sigs
                 then
                   ( sig_map
                   , Module (module_name, sigs, copy_to_defs ~sig_map:child_map defs) )
                 else
                   (* Don't copy inherited sigs from the parent over (at least for now)
                      because it's complicated. However, we do want to copy the local sigs
                      to its defs here. *)
                   sig_map, Module (module_name, sigs, copy_some_sigs_to_defs sigs defs)
               | None ->
                 (* If there's no module in the signature, there's nothing to copy from
                    there. We still want to copy the local sigs to its defs here though. *)
                 sig_map, Module (module_name, sigs, copy_some_sigs_to_defs sigs defs))
            | Trait _ -> failwith "TODO: copy trait_sigs to defs, without overriding"
            | Let _ | Impl _ -> sig_map, def))
      in
      let defs =
        Sig_data.fold_defs (Nested_map.current sig_map) ~init:defs ~f:(fun defs def ->
          def :: defs)
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
    copy_to_defs
      ~sig_map:(gather_decls ~sig_map:(Nested_map.create Sig_data.empty) sigs)
      defs
  ;;

  (** Gather placeholders for all declared names and types.
      (Needed for imports of submodules to work.) *)
  let rec gather_name_placeholders ~names module_name sigs defs =
    let f_common names = function
      | Val (name, _, _) | Extern (name, _, _, _) ->
        Name_bindings.add_name_placeholder names name
      | Type_decl (type_name, decl) ->
        Name_bindings.add_type_decl_placeholder names type_name decl
      | Effect (effect_name, effect) ->
        Name_bindings.add_effect_placeholder names effect_name effect
      | Import _ | Trait_sig _ -> names
    in
    gather_names ~names module_name sigs defs ~f_common ~f_def:(fun names def ->
      Node.with_value def ~f:(function
        | Let { bindings; rec_ } ->
          assert_or_compiler_bug ~here:[%here] rec_;
          Nonempty.fold bindings ~init:names ~f:(fun names (pat, _) ->
            Node.with_value pat ~f:(fun pat ->
              Name_bindings.merge_names
                names
                (Pattern.Names.gather pat ~type_source:Placeholder)
                ~combine:(fun _ _ entry' -> entry')))
        | Module (module_name, sigs, defs) ->
          gather_name_placeholders ~names module_name sigs defs
        | Common_def common -> f_common names common
        | Trait _ | Impl _ -> failwith "TODO: trait/impl (gather_name_placeholders)"))
  ;;

  let import_mentions_prelude : Module.Import.t -> bool = function
    | { kind = Absolute; paths = Module (module_name, paths) } ->
      Module_name.equal module_name Intrinsics.std_module_name
      && Nonempty.exists paths ~f:(function
           | Module (module_name, _) ->
             Module_name.equal module_name Intrinsics.prelude_module_name
           | Name name | Name_as (name, _) | Name_excluded name ->
             Unidentified_name.equal
               name
               (Module_name.unidentify Intrinsics.prelude_module_name)
           | All -> true)
    | { kind = Absolute; paths = All | Name _ | Name_as _ | Name_excluded _ }
    | { kind = Relative _; paths = _ } -> false
  ;;

  let gather_imports ~names ~include_std module_name sigs defs =
    let import_mentioned_prelude = ref false in
    let names =
      gather_names ~names module_name sigs defs ~f_common:(fun names -> function
        | Import import ->
          if include_std && not !import_mentioned_prelude
          then import_mentioned_prelude := import_mentions_prelude import;
          Name_bindings.import names import
        | Val _ | Extern _ | Type_decl _ | Effect _ | Trait_sig _ -> names)
    in
    if !import_mentioned_prelude || not include_std
    then names
    else
      List.fold [ `Sig; `Def ] ~init:names ~f:(fun names place ->
        Name_bindings.with_submodule
          names
          ~place
          module_name
          ~f:(Fn.flip Name_bindings.import_all_absolute Intrinsics.prelude_module_path))
  ;;

  let absolutify_everything =
    let absolutify_common ~names common =
      match (common : _ Module.common) with
      | Val (name, fixity, type_) ->
        Val
          ( name
          , fixity
          , Tuple2.map_snd type_ ~f:(Name_bindings.absolutify_type_scheme names) )
      | Extern (name, fixity, type_, extern_name) ->
        Extern
          (name, fixity, Name_bindings.absolutify_type_scheme names type_, extern_name)
      | Type_decl (type_name, decl) ->
        Type_decl (type_name, Name_bindings.absolutify_type_decl names decl)
      | Effect (effect_name, effect) ->
        Effect (effect_name, Name_bindings.absolutify_effect names effect)
      | Import _ as common -> common
      | Trait_sig _ -> failwith "absolutify_everything: traits"
    in
    let rec absolutify_sigs ~names sigs =
      List.map sigs ~f:(fun sig_ ->
        Node.map sig_ ~f:(function
          | Common_sig common -> Common_sig (absolutify_common ~names common)
          | Module_sig (module_name, sigs) ->
            Module_sig
              ( module_name
              , absolutify_sigs
                  ~names:(Name_bindings.into_module names ~place:`Sig module_name)
                  sigs )))
    in
    let rec absolutify_defs ~names defs =
      List.map defs ~f:(fun sig_ ->
        Node.map sig_ ~f:(function
          | Common_def common -> Common_def (absolutify_common ~names common)
          | Let _ as def -> def
          | Module (module_name, sigs, defs) ->
            Module
              ( module_name
              , absolutify_sigs
                  ~names:(Name_bindings.into_module names ~place:`Sig module_name)
                  sigs
              , absolutify_defs
                  ~names:(Name_bindings.into_module names ~place:`Def module_name)
                  defs )
          | Trait _ | Impl _ -> failwith "absolutify_everything: traits and impls"))
    and absolutify_module ~names module_name sigs defs =
      ( absolutify_sigs
          ~names:(Name_bindings.into_module names ~place:`Sig module_name)
          sigs
      , absolutify_defs
          ~names:(Name_bindings.into_module names ~place:`Def module_name)
          defs )
    in
    absolutify_module
  ;;

  let gather_type_decls ~names ~types sigs defs =
    gather_names ~names sigs defs ~f_common:(fun names -> function
      | Type_decl (type_name, decl) -> Name_bindings.add_type_decl names type_name decl
      | Effect (effect_name, effect) ->
        let constrain = Type_bindings.constrain' ~names ~types in
        Name_bindings.add_effect names effect_name effect ~constrain
      | Trait_sig _ -> failwith "TODO: trait sigs"
      | Val _ | Extern _ | Import _ -> names)
  ;;

  (** Raise an error upon finding any cycles in a given type alias. *)
  let check_cyclic_type_alias ~names alias =
    (* TODO: can rewrite this with [Type.Expr.map] *)
    (* TODO: This could stop checking for cyclic aliases early if it reaches a type in
       another file. (There should be a separate check for cyclic imports.) *)
    let rec loop ~names ~aliases_seen (alias : Module_path.absolute Type_scheme.type_) =
      match alias with
      | Type_app (name, args) ->
        let type_entry = Name_bindings.find_absolute_type_entry names name in
        let decl = Name_bindings.Type_entry.decl type_entry in
        (match snd decl with
         | Alias alias ->
           let id = Name_bindings.Type_entry.id type_entry in
           if Set.mem aliases_seen id
           then
             Compilation_error.raise
               Type_error
               ~msg:
                 [%message
                   "Cyclic type alias"
                     (name : Type_name.Absolute.t)
                     (decl : Module_path.absolute Type_decl.t)]
           else loop ~names ~aliases_seen:(Set.add aliases_seen id) alias
         | Abstract | Variants _ | Record _ -> ());
        List.iter args ~f:(loop ~names ~aliases_seen)
      | Function (args, effects, body) ->
        Nonempty.iter args ~f:(loop ~names ~aliases_seen);
        Option.iter effects ~f:(loop_effects ~names ~aliases_seen);
        loop ~names ~aliases_seen body
      | Tuple items -> List.iter items ~f:(loop ~names ~aliases_seen)
      | Union items | Intersection items ->
        Nonempty.iter items ~f:(loop ~names ~aliases_seen)
      | Var _ -> ()
    and loop_effects
      ~names
      ~aliases_seen
      (effects : Module_path.absolute Type_scheme.effects)
      =
      match effects with
      | Effect ((_ : Effect_name.Absolute.t), args) ->
        List.iter args ~f:(loop ~names ~aliases_seen)
      | Effect_union effects | Effect_intersection effects ->
        Nonempty.iter effects ~f:(loop_effects ~names ~aliases_seen)
      | Effect_var _ -> ()
    in
    loop ~names ~aliases_seen:Name_bindings.Type_entry.Id.Set.empty alias
  ;;

  (* TODO: We should make this a record type, it would a lot of this code way easier to
     read. *)
  type intermediate_def =
    ( Pattern.t * (Internal_type.t * Pattern.Names.t)
    , Untyped.Expr.t
    , Module_path.absolute )
    Module.def

  (** Handle all `val` and `let` statements (value bindings/type annotations).
      Also type the patterns in each let binding and assign the names fresh type
      variables. *)
  let rec handle_value_bindings ~names ~types module_name sigs defs
    : Name_bindings.t * intermediate_def Node.t list
    =
    let handle_common ~names = function
      | Val (name, fixity, typ) ->
        let constrain = Type_bindings.constrain' ~names ~types in
        Name_bindings.add_val names name fixity typ ~constrain
      | Extern (name, fixity, typ, extern_name) ->
        let constrain = Type_bindings.constrain' ~names ~types in
        Name_bindings.add_extern names name fixity ([], typ) extern_name ~constrain
      | Type_decl (_, (_, Alias alias)) ->
        check_cyclic_type_alias ~names alias;
        names
      | Effect _ ->
        (* FIXME: Check for cyclic effect types. *)
        names
      | Type_decl _ | Trait_sig _ | Import _ -> names
    in
    let rec handle_sigs ~names ~handle_common =
      List.fold ~init:names ~f:(fun names sig_ ->
        Node.with_value sig_ ~f:(function
          | Common_sig common -> handle_common ~names common
          | Module_sig (module_name, sigs) ->
            Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
              handle_sigs ~names ~handle_common sigs)))
    in
    let names =
      Name_bindings.with_submodule ~place:`Sig names module_name ~f:(fun names ->
        handle_sigs ~names ~handle_common sigs)
    in
    let handle_bindings ~names =
      Nonempty.fold_map ~init:names ~f:(fun names (pat, expr) ->
        let pat_span = Node.span pat in
        let pat_names, pat =
          Node.with_value pat ~f:(Pattern.of_untyped_with_names ~names ~types)
        in
        let names =
          (* Unify pattern bindings with local type information (e.g. val declarations) *)
          Name_bindings.merge_names
            names
            pat_names
            ~combine:(fun _ existing_entry new_entry ->
            let existing_type = Name_bindings.Name_entry.type_ existing_entry in
            let new_type = Name_bindings.Name_entry.type_ new_entry in
            Type_bindings.constrain'
              ~names
              ~types
              ~subtype:new_type
              ~supertype:existing_type;
            Name_bindings.Name_entry.merge existing_entry new_entry)
        in
        let pat, pat_type = pat in
        names, (Node.create (pat, (pat_type, pat_names)) pat_span, expr))
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

  (** Re-group and re-order toplevel let bindings so that each group is mutually
      recursive, and the groups are given in an order appropriate for generalization.
      This is done by topologically sorting the strongly-connected components of the call
      graph (dependencies between bindings). *)
  let extract_binding_groups ~names (defs : intermediate_def Node.t list)
    : _ * (_ Call_graph.Binding.t Nonempty.t * Module_path.Absolute.t) list
    =
    let rec gather_bindings_in_defs ~names defs acc =
      List.fold_right defs ~init:acc ~f:(gather_bindings ~names)
    and gather_bindings ~names (def : intermediate_def Node.t) (other_defs, bindings) =
      Node.with_value def ~f:(function
        | Let { bindings = bindings'; rec_ } ->
          (* All toplevel bindings are initially considered to be recursive. If found to
             be non-recursive, we will set `rec_ = false` later. *)
          assert_or_compiler_bug ~here:[%here] rec_;
          ( other_defs
          , Nonempty.fold_right bindings' ~init:bindings ~f:(fun (pat, expr) bindings ->
              let pat_names =
                Node.with_value pat ~f:(fun (_, (_, pat_names)) -> pat_names)
              in
              let current_path = Name_bindings.current_path names in
              let bound_names = Map.key_set pat_names in
              let used_names = Node.with_value expr ~f:(Untyped.Expr.names_used ~names) in
              ( { Call_graph.Binding.bound_names; used_names; info = pat, expr }
              , current_path )
              :: bindings) )
        | Module (module_name, sigs, defs) ->
          let names = Name_bindings.into_module ~place:`Def names module_name in
          let other_defs', bindings =
            gather_bindings_in_defs ~names defs ([], bindings)
          in
          Node.set def (Module (module_name, sigs, other_defs')) :: other_defs, bindings
        | Common_def _ as node -> Node.set def node :: other_defs, bindings
        | Trait _ | Impl _ -> failwith "TODO: traits/impls")
    in
    let other_defs, bindings = gather_bindings_in_defs ~names defs ([], []) in
    let binding_groups = Call_graph.regroup_bindings bindings in
    other_defs, binding_groups
  ;;

  let type_binding_group ~names ~types bindings =
    (* TODO: The spans should be kept as consistent with the original source as possible
       in order to report accurate errors. It's better that they match the original source
       location of the code than they actually make sense for the AST itself. *)
    (* Order bindings in the group by span, then take the first span to represent the
       whole group. This is done to get a consistent ordering among bindings. *)
    let get_spans (pat, expr) = Node.span pat, Node.span expr in
    let bindings =
      Nonempty.map bindings ~f:Call_graph.Binding.info
      |> Nonempty.sort ~compare:(Comparable.lift [%compare: Span.t * Span.t] ~f:get_spans)
    in
    let representative_span =
      get_spans (Nonempty.hd bindings) |> Tuple2.uncurry Span.combine
    in
    (* FIXME: cleanup *)
    (* print_s
      [%message
        "typing bindings"
          (bindings
            : ((Pattern.t
               * (Internal_type.t * Name_bindings.Name_entry.t Value_name.Map.t))
               Node.t
              * Untyped.Expr.t Node.t)
              Nonempty.t)]; *)
    let (_ : Name_bindings.t), rec_, bindings =
      Expr.type_recursive_let_bindings ~names ~types bindings ~add_effects:(fun effects ->
        (* No effects are handled at toplevel, so get rid of any produced effects. *)
        Type_bindings.constrain_effects_to_be_total ~names ~types effects)
    in
    Nonempty.fold_map bindings ~init:names ~f:(fun names (pattern_etc, expr) ->
      let pat_span = Node.span pattern_etc in
      let pat, (names, scheme) =
        Node.with_value pattern_etc ~f:(fun (pat, (pat_type, pat_names)) ->
          ( pat
          , Pattern.generalize ~names ~types pat_names pat_type ~shadowing_allowed:false ))
      in
      (* Generalize local let bindings just after the parent binding *)
      let expr_and_scheme =
        Node.map expr ~f:(fun expr ->
          let expr = Expr.generalize_let_bindings expr ~names ~types in
          expr, scheme)
      in
      names, (Node.create pat pat_span, expr_and_scheme))
    |> Tuple2.map_snd ~f:(fun bindings ->
         Node.create (Let { rec_; bindings }) representative_span)
  ;;

  (** Reintegrate the re-ordered binding groups from [extract_binding_groups] back into
      the AST. *)
  let reintegrate_binding_groups
    (path : Module_path.Absolute.t)
    (other_defs : def Node.t list)
    (binding_groups : (def Node.t * Module_path.Absolute.t) list)
    : def Node.t list
    =
    (* NOTE: As we disallow cross-module mutual recursion, binding groups will always be
       contained within a single module and can just be put back into the AST *)
    let binding_table = Module_path.Absolute.Table.create () in
    List.iter binding_groups ~f:(fun (def, path) ->
      Hashtbl.add_multi binding_table ~key:path ~data:def);
    let rec loop binding_table path defs =
      let defs =
        List.map
          defs
          ~f:
            (Node.map ~f:(function
              | Module (module_name, sigs, defs) ->
                let path' = Module_path.append path [ module_name ] in
                Module (module_name, sigs, loop binding_table path' defs)
              | def -> def))
      in
      (* Sort defs by span to get them back into their original order *)
      List.sort
        (Hashtbl.find_multi binding_table path @ defs)
        ~compare:(Comparable.lift Span.compare ~f:Node.span)
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
        List.fold_map binding_groups ~init:names ~f:(fun names (bindings, path) ->
          Name_bindings.with_path_into_defs names path ~f:(fun names ->
            let names, def = type_binding_group ~names ~types bindings in
            names, (def, path)))
      in
      let path = Name_bindings.current_path names in
      let defs = reintegrate_binding_groups path other_defs binding_groups in
      let rec check_def_modules ~names defs =
        List.iter defs ~f:(fun def ->
          Node.with_value def ~f:(function
            | Module (module_name, _, defs) ->
              check_def_modules
                ~names:(Name_bindings.into_module names module_name ~place:`Def)
                defs;
              Sig_def_diff.check ~names module_name
            | Common_def _ | Let _ | Trait _ | Impl _ -> ()))
      in
      check_def_modules ~names defs;
      names, defs)
  ;;

  let rec check_every_val_is_defined ~names module_name (defs : def Node.t list) =
    let names = Name_bindings.into_module names ~place:`Def module_name in
    List.iter defs ~f:(fun def ->
      Node.with_value def ~f:(function
        | Common_def (Val (name, _, _)) ->
          let entry =
            Name_bindings.find_absolute_entry
              names
              (Name_bindings.current_path names, name)
          in
          (match Name_bindings.Name_entry.type_source entry with
           | Val_and_let -> ()
           | Val_declared ->
             Compilation_error.raise
               Name_error
               ~msg:[%message "No definition for val declaration"]
           | Placeholder | Let_inferred | Extern_declared ->
             compiler_bug
               [%message
                 "Unexpected type source for val entry"
                   (name : Value_name.t)
                   (entry : Name_bindings.Name_entry.t)])
        | Module (module_name, _sigs, defs) ->
          check_every_val_is_defined ~names module_name defs
        | Common_def (Extern _ | Type_decl _ | Effect _ | Trait_sig _ | Import _)
        | Let _ | Trait _ | Impl _ -> ()))
  ;;

  (* TODO: Type inference is local to groups of toplevel let bindings, so we could be
     creating a new [Type_bindings.t] to use for each of those, instead of re-using one
     for a whole file. This would save some memory and hopefully have the GC scan less. *)
  let of_untyped ~names ~types ~include_std (module_name, sigs, defs) =
    try
      let defs = copy_some_sigs_to_defs sigs defs in
      let names = gather_name_placeholders ~names module_name sigs defs in
      let names = gather_imports ~names ~include_std module_name sigs defs in
      let sigs, defs = absolutify_everything ~names module_name sigs defs in
      let names = gather_type_decls ~names ~types module_name sigs defs in
      let names, defs = handle_value_bindings ~names ~types module_name sigs defs in
      let names, defs = type_defs ~names ~types module_name defs in
      check_every_val_is_defined ~names module_name defs;
      Sig_def_diff.check ~names module_name;
      Ok (names, (module_name, sigs, defs))
    with
    | Compilation_error.Compilation_error error ->
      Error { error with backtrace = Some (Backtrace.Exn.most_recent ()) }
  ;;
end
