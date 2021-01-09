open Import
open Names

let type_error_msg = Type_bindings.type_error_msg

module Pattern = struct
  type t =
    | Constant of Untyped.Literal.t
    | Catch_all of Value_name.t option
    | Cnstr_appl of Cnstr_name.Qualified.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) list
    | Union of t * t
  [@@deriving sexp]

  (* TODO: consider making this a map to types directly, since let_inferred is always used *)
  let add_fresh_name pat_names name =
    let var = Type.fresh_var () in
    let name_entry = Name_bindings.Name_entry.let_inferred var in
    match Map.add pat_names ~key:name ~data:name_entry with
    | `Ok pat_names -> pat_names, var
    | `Duplicate ->
      type_error_msg
        ("Duplicate name in pattern: " ^ Ustring.to_string (Value_name.to_ustring name))
  ;;

  let of_untyped_with_names ~names ~types pat =
    let rec of_untyped_with_names ~names ~types pat_names = function
      | Untyped.Pattern.Constant lit ->
        pat_names, (Constant lit, Type.Concrete.cast (Untyped.Literal.typ lit))
      | Catch_all name ->
        let pat_names, typ =
          match name with
          | Some name -> add_fresh_name pat_names name
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
        (* Unions must define the same names, but the types may be slightly different
           e.g. different variables *)
        if not (Map.equal (fun _ _ -> true) pat_names1 pat_names2)
        then type_error_msg "Pattern unions must define the same names";
        pat_names1, (Union (pat1, pat2), typ1)
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
    of_untyped_with_names ~names ~types pattern
    |> Tuple2.map_fst
         ~f:(Name_bindings.merge_names names ~combine:(fun _ _ new_val -> new_val))
  ;;

  let rec generalize ~names ~types pat_type =
    match (pat_type : t * Type.t) with
    | (Catch_all None | Constant _), typ -> names, Type_bindings.generalize types typ
    | Catch_all (Some name), typ ->
      let scheme = Type_bindings.generalize types typ in
      Name_bindings.set_scheme names name scheme, scheme
    | Cnstr_appl (cnstr, args), typ ->
      let rec loop ~names ~types = function
        | Type.Expr.Function (arg_type, body_type), arg :: args ->
          let names, _ = generalize ~names ~types (arg, arg_type) in
          loop ~names ~types (body_type, args)
        | body_type, [] ->
          Type_bindings.unify ~names ~types body_type typ;
          names, Type_bindings.generalize types body_type
        | _ -> compiler_bug [%message "Constructor arg number mismatch"]
      in
      loop ~names ~types (Name_bindings.find_cnstr_type names cnstr, args)
    | Tuple fields, Tuple field_types ->
      List.zip_exn fields field_types
      |> List.fold_map ~init:names ~f:(fun names -> generalize ~names ~types)
      |> Tuple2.map_snd ~f:Type.Expr.tuple
    | Tuple _, got -> compiler_bug [%message "Expected tuple type" (got : Type.t)]
    | Record _, _ -> failwith "TODO: generalizing record types in patterns"
    | Union (pat, _), typ ->
      (* Both branches must bind the same names (checked earlier), so only one
         branch needs to be considered) *)
      generalize ~names ~types (pat, typ)
  ;;

  let gather_names =
    let rec gather_names pat_names = function
      | Untyped.Pattern.Catch_all (Some name) -> fst (add_fresh_name pat_names name)
      | Cnstr_appl (_, items) | Tuple items ->
        List.fold items ~init:pat_names ~f:gather_names
      | Record fields ->
        List.fold fields ~init:pat_names ~f:(fun pat_names -> function
          | name, None -> fst (add_fresh_name pat_names name)
          | _, Some pat -> gather_names pat_names pat)
      | Union (pat, _) ->
        (* Both branches bind the same names, so only one need be considered *)
        gather_names pat_names pat
      | Constant _ | Catch_all None | Type_annotation _ -> pat_names
    in
    gather_names Value_name.Map.empty
  ;;
end

module Expr = struct
  type t =
    | Literal of Untyped.Literal.t
    | Name of Value_name.Qualified.t
    | Fun_call of t * t
    | Lambda of Pattern.t * t
    | Match of t * (Pattern.t * t) list
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t list
    | Record_literal of (Value_name.t * t option) list
    | Record_update of t * (Value_name.t * t option) list
    | Record_field_access of t * Value_name.t
  [@@deriving sexp]

  (* Re-associate the tree through rotations until each operator has fixity less
     than its children's fixities *)
  let rec reassociate_op_tree ~names = function
    | Btree.Node (op_name, left_child, right_child) as root ->
      let op_assoc, op_level = Name_bindings.find_fixity names op_name in
      (match left_child with
      | Node (left_name, _, _) ->
        let left_assoc, left_level = Name_bindings.find_fixity names left_name in
        let comp = Fixity.Level.compare op_level left_level in
        if comp < 0
        then (* Top is lower (looser), this is good *)
             (* FIXME: do something here *)
          root
        else if comp > 0
        then
          (* Top is higher (tigher), rotate to fix *)
          reassociate_op_tree ~names (Btree.rotate_clockwise_exn root)
        else (
          (* Equal level - check associativity *)
          (* This the left child, so they should both be right-associative *)
          (*FIXME: equal level*)
          match op_assoc, left_assoc with
          | Fixity.Assoc.(Right, Right) ->
            Node (op_name, reassociate_op_tree ~names left_child, right_child)
          | _ -> type_error_msg "Associativity error")
      | Leaf _ ->
        (match right_child with
        | Node (right_name, _, _) ->
          let right_assoc, right_level = Name_bindings.find_fixity names right_name in
          let comp = Fixity.Level.compare op_level right_level in
          if comp < 0
          then (* FIXME: we still have to do something here, right? *)
            root
          else if comp > 0
          then reassociate_op_tree ~names (Btree.rotate_anticlockwise_exn root)
          else (
            match op_assoc, right_assoc with
            | Fixity.Assoc.(Left, Left) ->
              (* FIXME: this doesn't seem right, as I think you may have to come back up the tree sometimes
                 Also, what happens to the left child here? *)
              Node (op_name, left_child, reassociate_op_tree ~names right_child)
            | _ -> type_error_msg "Associativity error")
        | Leaf _ -> (* FIXME: do something here *) root))
    | Leaf _ as leaf -> leaf
  ;;

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
      | Op_tree tree ->
        let rec expr_of_tree = function
          | Btree.Leaf expr -> expr
          | Node (op_name, left_child, right_child) ->
            let left_arg, right_arg = expr_of_tree left_child, expr_of_tree right_child in
            Untyped.Expr.Fun_call (Fun_call (Name op_name, left_arg), right_arg)
        in
        of_untyped ~names ~types (expr_of_tree (reassociate_op_tree ~names tree))
      | Lambda (pat, body) ->
        let names, (pat, pat_type) = Pattern.of_untyped_into ~names ~types pat in
        let body, body_type = of_untyped ~names ~types body in
        Lambda (pat, body), Function (pat_type, body_type)
      | If (cond, b1, b2) ->
        let cond, cond_type = of_untyped ~names ~types cond in
        let bool_type = Type.Concrete.cast Core.Bool.typ in
        Type_bindings.unify ~names ~types cond_type bool_type;
        let (b1, b1_type), (b2, b2_type) =
          of_untyped ~names ~types b1, of_untyped ~names ~types b2
        in
        Type_bindings.unify ~names ~types b1_type b2_type;
        let prelude_cnstr name =
          Pattern.Cnstr_appl
            ((Core.prelude_module_path, Cnstr_name.of_string_exn name), [])
        in
        Match (cond, [ prelude_cnstr "True", b1; prelude_cnstr "False", b2 ]), b1_type
      | Match (expr, branches) ->
        let expr, expr_type = of_untyped ~names ~types expr in
        let branches, branch_types =
          List.map branches ~f:(fun (pat, branch) ->
            let names, (pat, pat_typ) = Pattern.of_untyped_into ~names ~types pat in
            Type_bindings.unify ~names ~types expr_type pat_typ;
            let branch, branch_type = of_untyped ~names ~types branch in
            (pat, branch), branch_type)
          |> List.unzip
        in
        (match branch_types with
        | branch_type :: _ ->
          iter_pairs branch_types ~f:(Type_bindings.unify ~names ~types);
          Match (expr, branches), branch_type
        | [] -> raise_s [%message "Empty match"])
      | Let { rec_; pat; expr; body } ->
        (* FIXME: handle recursion? *)
        let expr, expr_type = of_untyped ~names ~types expr in
        let names, (pat, pat_type) = Pattern.of_untyped_into ~names ~types pat in
        Type_bindings.unify ~names ~types expr_type pat_type;
        let body, body_type = of_untyped ~names ~types body in
        Let { rec_; pat; expr; body }, body_type
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
end

module Module = struct
  include Module

  type nonrec t = (Pattern.t, Expr.t * Type.Scheme.t) t [@@deriving sexp]
  type nonrec def = (Pattern.t, Expr.t * Type.Scheme.t) def [@@deriving sexp]

  (* Handling order:
     1. Imports
     2. Type/trait declarations (in sigs or defs)
     3. Trait impls (TODO: allow them in sigs) - not the bodies, just the fact they exist
     4. Val statements + Let statement patterns (unify if val is after let)
     5. Let statement/trait impl bodies (expressions)
     
     1-2 can be handled by filtering the sigs/defs and handing to gather_module_names (?)
     3 we'll skip for now
     4 can be done again by gather_module names
     5 is done by type_defs
     
     TODO: also need to consider combingin multiple declarations e.g.
     ```
     module :
       type T
     type alias T = Int  # What about `type T = alias Int`?
     ```
     Or even more advanced ideas like property-like record representations e.g.
     ```
     module :
       type TimeSpan = { seconds, minutes, hours, days, weeks, months, years : Int }
     type TimeSpan = Int  # just a count of seconds
     ```
     Need to figure out some way of specifying the conversions, etc.
     (conversion to/from? getters/setters?)
     Should be difficult but seems worth it for the ability to use record features like
     pattern matching without breaking encapsulation *)

  let rec gather_names ~names sigs defs ~handle_common =
    let names =
      List.fold sigs ~init:names ~f:(fun names -> function
        | Common_sig common -> handle_common names common
        | Module_sig (module_name, sigs) ->
          Name_bindings.with_submodule names module_name ~f:(fun names ->
            gather_names ~names sigs [] ~handle_common))
    in
    List.fold defs ~init:names ~f:(fun names -> function
      | Common_def common -> handle_common names common
      | Module (module_name, sigs, defs) ->
        Name_bindings.with_submodule names module_name ~f:(fun names ->
          gather_names ~names sigs defs ~handle_common)
      | Let _ | Trait _ | Impl _ -> names)
  ;;

  (* FIXME: decide what I'm doing with resolution order here:
     imports can't just be first, because imports can refer to local submodules,
     which would require full handling (?) to import the names for values/types
     
     I might just be making things much harder than necessary for myself
     
     Alternative: when importing, instead of copying, write a reference to the originally
     imported type e.g. (Imported_from Std.Prelude) 
     - Then imports can just refer to the names
     - Bound names also need gathering then? Although patterns can't be typed yet...
       -> Extracting just the names is fine to do
       -> I could type the patterns if I require type declarations to be before the use
          (which seems fine)
       -> so that would make the resolution steps:
          1. Imports, type/trait decls, reading val bindings, typing patterns
          2. Type the expressions
             - so let definitions can be in any order, but types and imports must be
               before their usage
             - this would effectively force submodules to be at the top of their parent
               modules, since types defined in submodules can't be used until after
           
      What does GHC probably do? (very, very roughly)
      1. Read imports and type decls
      2. Handle bindings
      No submodules so imports don't rely on typing other modules right now
      The problem is mutual recursion, effectively

      How about this - everything just works like in OCaml, except as if all the let
      statements were in a massive let rec/and block?
      So order matters for imports/type decls
      - Sub module handling? Do the let statements occur before or after?
        Split into sections?
      - I think this ends up being a bit confusing/weird

      -----
      Different idea for steps
      1. Read all bound names in the module and submodules, and assign them fresh type variables
         - this doesn't require knowing anything - just read all the Catch_all names
         - Also read the types in somehow with placeholder values - maybe Abstract? - or just an option with None
         - This allows local imports to check properly
         - TODO: really only need to do sigs, and then defs if there are no sigs
      2. Read imports
         - Don't copy anything - use Imported_from
      3. Read type/trait declarations
         - mutual recursion in types is handled somehow
      4. (not yet (?)) Look for trait impls (just the fact they exist)
         - Needs to be done after knowing about types/traits 
      5. Handle val/let bindings
      6. Handle let/impl expressions

      In summary, everything is as done now, but with an extra step at the start to read
      names/types, and imports are not just copied as values.
      -----
      *)

  (** Gather placeholders for all declared names and types.
      (Needed for imports of submodules to work.) *)
  let gather_name_placeholders ~names sigs defs =
    let names =
      gather_names ~names sigs defs ~handle_common:(fun names -> function
        | Val (name, _, _) -> fst (Name_bindings.add_fresh_var names name)
        | Type_decl (type_name, _) -> Name_bindings.add_type_placeholder names type_name
        | Import _ | Import_with _ | Import_without _ | Trait_sig _ -> names)
    in
    List.fold defs ~init:names ~f:(fun names -> function
      | Let (pat, _) ->
        Name_bindings.merge_names
          names
          (Pattern.gather_names pat)
          ~combine:(fun name entry1 entry2 ->
          match entry1.type_source, entry2.type_source with
          | Val_declared, Val_declared | Let_inferred, Let_inferred ->
            Name_bindings.name_error_msg "Duplicate name" (Value_name.to_ustring name)
          | _ -> entry1)
      | _ -> names)
  ;;

  (** Gather all imported names and local type/trait declarations *)
  let gather_imports_and_type_decls =
    gather_names ~handle_common:(fun names -> function
      | Import module_name -> Name_bindings.import names module_name
      | Import_with (path, imports) -> Name_bindings.import_with names path imports
      | Import_without (path, hiding) -> Name_bindings.import_without names path hiding
      | Type_decl (type_name, decl) -> Name_bindings.add_type_decl names type_name decl
      | Trait_sig _ -> failwith "TODO: trait sigs"
      | Val _ -> names)
  ;;

  (** Handle all `val` and `let` statements (value bindings/type annotations).
      Also type the patterns in each let binding and assign the names fresh type variables *)
  let rec handle_value_bindings ~names ~types sigs defs =
    let names =
      gather_names ~names sigs defs ~handle_common:(fun names -> function
        | Val (name, fixity, typ) ->
          (* TODO: check that val_after_let is impossible, and then probably remove it
             (since vals are now checked before lets, it should be fine to remove) *)
          let unify = Type_bindings.unify ~names ~types in
          Name_bindings.add_val names name fixity typ ~unify
        | Type_decl _ | Trait_sig _ | Import _ | Import_with _ | Import_without _ -> names)
    in
    List.fold_map defs ~init:names ~f:(fun names -> function
      | Let (pat, expr) ->
        (* FIXME: handle recursion *)
        let pat_names, pat = Pattern.of_untyped_with_names ~names ~types pat in
        let names =
          (* Unify pattern bindings with local type information (e.g. val declarations) *)
          Name_bindings.merge_names names pat_names ~combine:(fun _ entry entry' ->
            let typ, typ' = Name_bindings.Name_entry.(typ entry, typ entry') in
            Type_bindings.unify ~names ~types typ typ';
            entry)
        in
        names, Let (pat, expr)
      | Module (module_name, sigs, defs) ->
        let names = Name_bindings.into_module names module_name in
        let names, defs = handle_value_bindings ~names ~types sigs defs in
        names, Module (module_name, sigs, defs)
      | Common_def _ as def -> names, def
      | Impl _ | Trait _ -> failwith "TODO: handle_value_bindings traits/impls")
  ;;

  (** Type-check the expressions (definitions) for each let binding and trait implementation.
      Performs let-generalization on free variables in let statement bindings.
      (Let-generalization is not currently done for local let bindings within expressions.)
      This is the final step in the type checking process. *)
  let rec type_defs ~names ~types =
    List.fold_map ~init:names ~f:(fun names -> function
      | Common_def _ as def -> names, def
      | Let ((pat, pat_type), expr) ->
        (* FIXME: handle recursion *)
        let expr, expr_type = Expr.of_untyped ~names ~types expr in
        Type_bindings.unify ~names ~types pat_type expr_type;
        let names, scheme = Pattern.generalize ~names ~types (pat, pat_type) in
        names, Let (pat, (expr, scheme))
      | Module (module_name, sigs, defs) ->
        let names, defs =
          type_defs ~names:(Name_bindings.into_module names module_name) ~types defs
        in
        Name_bindings.into_parent names, Module (module_name, sigs, defs)
      | Trait _ | Impl _ -> failwith "TODO: type_defs traits/impls")
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
      let exn_msg = Exn.to_string exn in
      let full_msg =
        if backtrace
        then exn_msg ^ "\n" ^ Backtrace.(to_string (Exn.most_recent ()))
        else exn_msg
      in
      Error (Ustring.of_string_exn full_msg)
  ;;
end
