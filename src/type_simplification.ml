open! Import
open! Names

(* Simplify types using ideas from these papers:
   - Inferring Algebraic Effects https://arxiv.org/pdf/1312.2334.pdf
   - The Simple Essence of Algebraic Subtyping https://dl.acm.org/doi/pdf/10.1145/3409006 *)

let eprint_s = eprint_s [%here]

(* TODO: Consider upstreaming these in Type_scheme *)

let map_type_with_polarity =
  let rec loop ~polarity (typ : _ Type_scheme.type_) ~f ~f_effects ~type_name ~effect_name
    =
    match (f ~polarity typ : _ Map_action.t) with
    | Halt typ -> typ
    | Retry typ -> loop ~polarity typ ~f ~f_effects ~type_name ~effect_name
    | Defer typ ->
      (match typ with
       | Var _ as typ -> typ
       | Type_app (name, fields) ->
         (* TODO: Handle type variable variance. *)
         Type_app
           ( type_name name
           , List.map fields ~f:(loop ~polarity ~f ~f_effects ~type_name ~effect_name) )
       | Tuple fields ->
         Tuple (List.map fields ~f:(loop ~polarity ~f ~f_effects ~type_name ~effect_name))
       | Function (args, effects, result) ->
         let args =
           Nonempty.map args ~f:(fun arg ->
             loop
               arg
               ~polarity:(Polarity.flip polarity)
               ~f
               ~f_effects
               ~type_name
               ~effect_name)
         in
         let effects =
           loop_effects ~polarity ~f ~f_effects effects ~type_name ~effect_name
         in
         Function
           (args, effects, loop ~polarity ~f ~f_effects ~type_name ~effect_name result)
       | Union types ->
         Type_scheme.union
           (Non_single_list.map
              types
              ~f:(loop ~polarity ~f ~f_effects ~type_name ~effect_name))
       | Intersection types ->
         Type_scheme.intersection
           (Non_single_list.map
              types
              ~f:(loop ~polarity ~f ~f_effects ~type_name ~effect_name)))
  and loop_effects
    ~polarity
    (effects : _ Type_scheme.effects)
    ~f
    ~f_effects
    ~type_name
    ~effect_name
    =
    match (f_effects ~polarity effects : _ Map_action.t) with
    | Halt effects -> effects
    | Retry effects ->
      loop_effects ~polarity effects ~f ~f_effects ~type_name ~effect_name
    | Defer effects ->
      (match effects with
       | Effect_var _ as effects -> effects
       | Effect (name, args) ->
         (* TODO: Handle type variable variance. *)
         Effect
           ( effect_name name
           , List.map args ~f:(loop ~polarity ~f ~f_effects ~type_name ~effect_name) )
       | Effect_union effects ->
         Type_scheme.effect_union
           (Non_single_list.map
              effects
              ~f:(loop_effects ~polarity ~f ~f_effects ~type_name ~effect_name)))
  in
  loop ~polarity:Positive
;;

let fold_type_with_polarity =
  let unwrap_continue : _ Fold_action.t -> _ Fold_action.t = function
    | Continue ((_ : Polarity.t), acc) -> Continue acc
    | Stop _ as stop -> stop
  in
  let rec loop type_ ~init ~f ~f_effects ~polarity : _ Fold_action.t =
    Type_scheme.fold_until
      type_
      ~init:(polarity, init)
      ~f:(handle_type ~f ~f_effects)
      ~f_effects:(handle_effects ~f_effects)
    |> unwrap_continue
  and loop_effects effects ~init ~f ~f_effects ~polarity =
    Type_scheme.fold_effects_until
      effects
      ~init:(polarity, init)
      ~f:(handle_type ~f ~f_effects)
      ~f_effects:(handle_effects ~f_effects)
    |> unwrap_continue
  and handle_type (polarity, init) (type_ : _ Type_scheme.type_) ~f ~f_effects =
    match%bind.Fold_action f ~polarity init type_ with
    | `Halt init -> Continue (`Halt (polarity, init))
    | `Defer init ->
      (match type_ with
       | Function (args, effects, res) ->
         let%bind.Fold_action init =
           Nonempty.fold_until args ~init ~f:(fun init arg ->
             loop arg ~init ~f ~f_effects ~polarity:(Polarity.flip polarity))
         in
         let%bind.Fold_action init = loop_effects effects ~init ~f ~f_effects ~polarity in
         let%map.Fold_action init = loop res ~init ~f ~f_effects ~polarity in
         `Halt (polarity, init)
       (* TODO: Handle type variable variance in [Type_app]. *)
       | Var _ | Type_app _ | Tuple _ | Union _ | Intersection _ ->
         Continue (`Defer (polarity, init)))
  and handle_effects (polarity, init) effects ~f_effects =
    (* TODO: Handle effect variable variance. *)
    let%map.Fold_action init = f_effects ~polarity init effects in
    match init with
    | `Halt init -> `Halt (polarity, init)
    | `Defer init -> `Defer (polarity, init)
  in
  fun type_ ~init ~f ~f_effects -> loop type_ ~init ~f ~f_effects ~polarity:Positive
;;

let simplify_var_sandwiches type_ ~lower_bounds ~upper_bounds =
  (* Union variables which are sandwiched together - variables like a where a <: b and
     a >: b. *)
  let var_classes = Type_param.Table.create () in
  let find_var_class var_classes var =
    Hashtbl.find_or_add var_classes var ~default:(fun () -> Union_find.create var)
  in
  Map.iter2 lower_bounds upper_bounds ~f:(fun ~key:var ~data ->
    match data with
    | `Both (subtypes, supertypes) ->
      let var_class = find_var_class var_classes var in
      Set.iter (Set.inter subtypes supertypes) ~f:(fun var' ->
        Union_find.union var_class (find_var_class var_classes var'))
    | `Left _ | `Right _ -> ());
  Type_scheme.map_vars type_ ~f:(fun var ->
    Union_find.get (find_var_class var_classes var))
;;

let get_positive_and_negative_vars outer_type ~context_vars =
  let add_var vars ~polarity var =
    By_polarity.update vars ~polarity ~f:(fun vars ->
      Map.update vars var ~f:(function
        | Some count -> count + 1
        | None -> 1))
  in
  fold_type_with_polarity
    outer_type
    ~init:
      (* TODO: It might make more sense to just start the polarity as negative instead of
         positive when recording it instead of flipping it here. *)
      (* Context vars are treated as "inputs" to an expression, so flip their polarity. *)
      (By_polarity.flip context_vars)
    ~f:(fun ~polarity vars -> function
         | Var var -> Continue (`Halt (add_var vars ~polarity var))
         | _ -> Continue (`Defer vars))
    ~f_effects:(fun ~polarity vars -> function
                 | Effect_var var -> Continue (`Halt (add_var vars ~polarity var))
                 | _ -> Continue (`Defer vars))
  |> Fold_action.id
;;

let replace_constraints_with_unions_and_intersections type_ ~lower_bounds ~upper_bounds =
  let replace_var var ~(polarity : Polarity.t) ~make_var ~union ~intersection =
    let bounds_map, combine =
      match polarity with
      | Positive -> lower_bounds, union
      | Negative -> upper_bounds, intersection
    in
    let bounds = Map.find bounds_map var |> Option.value ~default:Type_param.Set.empty in
    match Set.to_list bounds with
    | [] -> make_var var
    | vars ->
      List.map (var :: vars) ~f:make_var
      |> Non_single_list.of_list_convert ~make:combine ~singleton:Fn.id
  in
  map_type_with_polarity
    type_
    ~type_name:Fn.id
    ~effect_name:Fn.id
    ~f:(fun ~polarity type_ ->
      match type_ with
      | Var var ->
        Halt
          (replace_var
             var
             ~polarity
             ~make_var:Type_scheme.var
             ~intersection:Type_scheme.intersection
             ~union:Type_scheme.union)
      | type_ -> Defer type_)
    ~f_effects:(fun ~polarity effects ->
      match effects with
      | Effect_var var ->
        Halt
          (replace_var
             var
             ~polarity
             ~make_var:Type_scheme.effect_var
             ~union:Type_scheme.effect_union
             ~intersection:Type_scheme.effect_union)
      | effects -> Defer effects)
;;

(** Remove variables which only occur once in a positive or negative position
    respectively. Replace them with empty unions or intersections (the bottom and top
    types). It's important not to remove variables which occur more than once, since that
    erases the information that they are the same. *)
let remove_polar_vars type_ ~context_vars =
  let { By_polarity.positive = positive_vars; negative = negative_vars } =
    get_positive_and_negative_vars type_ ~context_vars
  in
  eprint_s
    [%lazy_message
      "vars after simplifying var sandwiches and subbing in unions/intersections"
        (type_ : _ Type_scheme.type_)
        (negative_vars : int Type_param.Map.t)
        (positive_vars : int Type_param.Map.t)];
  let replace_var var ~(polarity : Polarity.t) ~make_var ~bottom ~top =
    match polarity with
    | Positive ->
      if Map.mem negative_vars var || Map.find_exn positive_vars var > 1
      then make_var var
      else bottom
    | Negative ->
      if Map.mem positive_vars var || Map.find_exn negative_vars var > 1
      then make_var var
      else (
        match top with
        | Some top -> top
        | None -> make_var var)
  in
  map_type_with_polarity
    type_
    ~type_name:Fn.id
    ~effect_name:Fn.id
    ~f:(fun ~polarity type_ ->
      match type_ with
      | Var var ->
        Halt
          (replace_var
             var
             ~polarity
             ~make_var:Type_scheme.var
             ~bottom:(Union [])
             ~top:(Some (Intersection [])))
      | type_ -> Defer type_)
    ~f_effects:(fun ~polarity effects ->
      match effects with
      | Effect_var var ->
        Halt
          (replace_var
             var
             ~polarity
             ~make_var:Type_scheme.effect_var
             ~bottom:(Effect_union [])
             ~top:None)
      | effects -> Defer effects)
;;

(** Union all variables which mutually always co-occur in positive and/or negative
    positions. Replace variables which always co-occur in positive and/or negative
    positions with another variable. *)
let replace_co_occurring_vars type_ =
  let co_occurences_by_var =
    let update
      ~(co_occurences_by_var : Type_param.Set.t Type_param.Map.t By_polarity.t)
      ~(polarity : Polarity.t)
      vars
      =
      Set.fold vars ~init:co_occurences_by_var ~f:(fun co_occurences_by_var var ->
        let other_vars = Set.remove vars var in
        By_polarity.update co_occurences_by_var ~polarity ~f:(fun co_occurences_by_var ->
          Map.update co_occurences_by_var var ~f:(function
            | Some existing_vars -> Set.inter existing_vars other_vars
            | None -> other_vars)))
    in
    fold_type_with_polarity
      type_
      ~init:
        { By_polarity.positive = Type_param.Map.empty; negative = Type_param.Map.empty }
      ~f:(fun ~polarity co_occurences_by_var type_ ->
        match type_ with
        | Union types | Intersection types ->
          let vars =
            Non_single_list.to_list types
            |> List.map ~f:(function
                 | Var v -> v
                 | Type_app _ | Function _ | Tuple _ | Union _ | Intersection _ ->
                   (* TODO: Maybe we should encode in the type that only vars are allowed. *)
                   compiler_bug
                     [%message
                       "union or intersection with more than just vars "
                         (type_ : _ Type_scheme.type_)])
            |> Type_param.Set.of_list
          in
          Continue (`Halt (update ~co_occurences_by_var ~polarity vars))
        | Var var ->
          Continue
            (`Halt
              (update ~co_occurences_by_var ~polarity (Type_param.Set.singleton var)))
        | Type_app _ | Tuple _ | Function _ -> Continue (`Defer co_occurences_by_var))
      ~f_effects:(fun ~polarity co_occurences_by_var effects ->
        match effects with
        | Effect_union effects ->
          let vars =
            Non_single_list.to_list effects
            |> List.filter_map ~f:(function
                 | Effect_var v -> Some v
                 | Effect _ | Effect_union _ -> None)
            |> Type_param.Set.of_list
          in
          Continue (`Halt (update ~co_occurences_by_var ~polarity vars))
        | Effect_var var ->
          Continue
            (`Halt
              (update ~co_occurences_by_var ~polarity (Type_param.Set.singleton var)))
        | Effect _ -> Continue (`Defer co_occurences_by_var))
    |> Fold_action.id
  in
  eprint_s
    [%lazy_message
      (co_occurences_by_var : Type_param.Set.t Type_param.Map.t By_polarity.t)];
  let replacements = Type_param.Table.create () in
  By_polarity.iter co_occurences_by_var ~f:(fun co_occurences_by_var ~polarity:_ ->
    Map.iteri co_occurences_by_var ~f:(fun ~key:var ~data:co_occuring_vars ->
      let merge_with_self = ref false in
      let to_merge =
        Set.to_list co_occuring_vars
        |> List.map ~f:(fun var' ->
             match Map.find co_occurences_by_var var' with
             | Some co_occurences' when Set.mem co_occurences' var ->
               (* When variables mutually co-occur, union them together. *)
               merge_with_self := true;
               (match Hashtbl.find replacements var' with
                | None ->
                  let other_var_class = Union_find.create var' in
                  Hashtbl.add_exn replacements ~key:var' ~data:other_var_class;
                  other_var_class
                | Some other_var_class ->
                  Union_find.union other_var_class (Union_find.create var');
                  other_var_class)
             | Some _ | None -> Union_find.create var')
      in
      let to_merge =
        if !merge_with_self then Union_find.create var :: to_merge else to_merge
      in
      match to_merge with
      | [] -> ()
      | var_replacement_class :: other_var_classes ->
        List.iter other_var_classes ~f:(Union_find.union var_replacement_class);
        (match Hashtbl.find replacements var with
         | None -> Hashtbl.set replacements ~key:var ~data:var_replacement_class
         | Some existing_class -> Union_find.union existing_class var_replacement_class)));
  Type_scheme.map_vars type_ ~f:(fun var ->
    Hashtbl.find replacements var |> Option.value_map ~f:Union_find.get ~default:var)
;;

(* TODO: The context vars we pass here are imprecise. We gather context vars as we do
   inference, but then we generalize at the end, so when considering each type, we
   consider every type variable in the entire expression to be part of the context. This
   is particularly egregious for the types toplevel bindings, which shouldn't depend on
   anything. *)
let simplify_type ((type_, constraints) : _ Type_scheme.t) ~context_vars =
  eprint_s
    [%lazy_message
      "simplify_type"
        (type_, constraints : _ Type_scheme.t)
        (context_vars : int Type_param.Map.t By_polarity.t)];
  let type_ =
    let lower_bounds =
      List.map constraints ~f:(fun { subtype; supertype } -> supertype, subtype)
      |> Type_param.Map.of_alist_fold ~init:Type_param.Set.empty ~f:Set.add
    in
    let upper_bounds =
      List.map constraints ~f:(fun { subtype; supertype } -> subtype, supertype)
      |> Type_param.Map.of_alist_fold ~init:Type_param.Set.empty ~f:Set.add
    in
    let type_ = simplify_var_sandwiches type_ ~lower_bounds ~upper_bounds in
    eprint_s [%lazy_message "after simplifying sandwiches" (type_ : _ Type_scheme.type_)];
    replace_constraints_with_unions_and_intersections type_ ~lower_bounds ~upper_bounds
  in
  let type_ = remove_polar_vars type_ ~context_vars in
  eprint_s [%lazy_message "after removing polar vars" (type_ : _ Type_scheme.type_)];
  let type_ = replace_co_occurring_vars type_ in
  (* TODO: We don't know whether variables are used elsewhere in the expression without
     some notion of type variable scope. This means replacing vars only used once in a
     type expression with Never/Any isn't sound - it is only sound if the variable does
     not appear anywhere else. Maybe we need to simplify all the types for a statement at
     the same time? I suppose we could change this to simplify a list of types or
     something.

     This constraint is similar to generalization - we can't get rid of variables which
     appear elsewhere in the type environment.
     
     We also can't just come up with random names like `a`, `b`, etc. since the scope
     isn't well-defined. We need to make sure we give the same variables the same names,
     and maintain this in all the simplified types in some expression.
     
     So we either need to simplify all the types all at once, or simplify them one at a
     time but keep some state tracking what variables are mapped to what. For simplicity
     maybe we can start off assuming that variables are always scoped to the whole
     statement level. Then we just need to make sure the mappings we make are consistent
     with that. Though that isn't fully expressive - we still need explicit
     quantification to express local generalized bindings (though I guess we haven't
     implemented those yet, so maybe punt).
     
     Also consider making binders for type variables explicit in the AST. If we could
     insert them during inference maybe that could improve things? You'd know if a type
     variable was local to an expression. But I'm not sure how that'd help when
     simplifying a variable that appears in multiple places - you still kinda have to know
     how they interact (the reasoning is fundamentally not local).
  *)
  (* All of the constraints are made moot by replacing vars with the union of their
     relevant bounds, so there will be none left. *)
  eprint_s
    [%lazy_message "after replacing co-occuring vars" (type_ : _ Type_scheme.type_)];
  type_, []
;;

let%test_module _ =
  (module struct
    let parse_var s = Type_param.t_of_sexp (Atom s)
    let v = Type_scheme.var << parse_var
    let ev i = Type_scheme.effect_var (parse_var [%string "e%{i#Int}"])

    let ( <: ) s1 s2 : Type_scheme.constraint_ =
      { subtype = parse_var s1; supertype = parse_var s2 }
    ;;

    let bool = fst Intrinsics.Bool.typ
    let list arg = Type_scheme.Type_app (Type_name.Absolute.of_string "List", [ arg ])
    let empty_context_vars = By_polarity.init (const Type_param.Map.empty)

    let run_test original_type ~context_vars =
      print_s
        [%sexp
          { original_type : _ Type_scheme.t
          ; simplified_type =
              (simplify_type original_type ~context_vars : _ Type_scheme.t)
          }]
    ;;

    let%expect_test "List.concat_map" =
      run_test
        ~context_vars:empty_context_vars
        ( Function
            ( [ list (v "a"); Function ([ v "b" ], ev 1, list (v "c")) ]
            , ev 2
            , list (v "d") )
        , [ "a" <: "b"; "e1" <: "e2"; "c" <: "d" ] );
      [%expect
        {|
          ((original_type
            ((Function
              ((Type_app List ((Var a)))
               (Function ((Var b)) (Effect_var e1) (Type_app List ((Var c)))))
              (Effect_var e2) (Type_app List ((Var d))))
             (((subtype a) (supertype b)) ((subtype e1) (supertype e2))
              ((subtype c) (supertype d)))))
           (simplified_type
            ((Function
              ((Type_app List ((Var a)))
               (Function ((Var a)) (Effect_var e1) (Type_app List ((Var c)))))
              (Effect_var e1) (Type_app List ((Var c))))
             ()))) |}]
    ;;

    let%expect_test "if" =
      run_test
        ~context_vars:empty_context_vars
        (Function ([ bool; v "a"; v "b" ], ev 1, v "c"), [ "a" <: "c"; "b" <: "c" ]);
      [%expect
        {|
          ((original_type
            ((Function ((Type_app Bool ()) (Var a) (Var b)) (Effect_var e1) (Var c))
             (((subtype a) (supertype c)) ((subtype b) (supertype c)))))
           (simplified_type
            ((Function ((Type_app Bool ()) (Var a) (Var a)) (Effect_union ()) (Var a))
             ()))) |}]
    ;;

    let%expect_test "<=" =
      run_test
        ~context_vars:empty_context_vars
        (Function ([ v "a"; v "b" ], Effect_union [], bool), [ "a" <: "c"; "b" <: "c" ]);
      [%expect
        {|
          ((original_type
            ((Function ((Var a) (Var b)) (Effect_union ()) (Type_app Bool ()))
             (((subtype a) (supertype c)) ((subtype b) (supertype c)))))
           (simplified_type
            ((Function ((Var c) (Var c)) (Effect_union ()) (Type_app Bool ())) ()))) |}]
    ;;

    let%expect_test "id" =
      run_test
        ~context_vars:empty_context_vars
        (Function ([ v "a" ], Effect_union [], v "a"), []);
      [%expect
        {|
          ((original_type ((Function ((Var a)) (Effect_union ()) (Var a)) ()))
           (simplified_type ((Function ((Var a)) (Effect_union ()) (Var a)) ()))) |}];
      run_test
        ~context_vars:empty_context_vars
        (Function ([ v "a" ], ev 1, v "b"), [ "a" <: "b" ]);
      [%expect
        {|
          ((original_type
            ((Function ((Var a)) (Effect_var e1) (Var b))
             (((subtype a) (supertype b)))))
           (simplified_type ((Function ((Var a)) (Effect_union ()) (Var a)) ()))) |}]
    ;;

    let%expect_test "between" =
      run_test
        ~context_vars:empty_context_vars
        ( Function ([ v "c"; Tuple [ v "a"; v "b" ] ], Effect_union [], bool)
        , [ "c" <: "a"; "c" <: "b" ] );
      [%expect
        {|
          ((original_type
            ((Function ((Var c) (Tuple ((Var a) (Var b)))) (Effect_union ())
              (Type_app Bool ()))
             (((subtype c) (supertype a)) ((subtype c) (supertype b)))))
           (simplified_type
            ((Function ((Intersection ((Var a) (Var b))) (Tuple ((Var a) (Var b))))
              (Effect_union ()) (Type_app Bool ()))
             ()))) |}]
    ;;

    let%expect_test "consider context var polarity" = ()

    (* TODO: Quickcheck test that type simplification is idempotent. *)
  end)
;;
