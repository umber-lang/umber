open Import
open Names

module Cnstr = struct
  (* TODO: decide what to do with records *)
  module T = struct
    type t =
      | Named of Cnstr_name.t
      | Tuple
    [@@deriving compare, equal, sexp, variants]
  end

  include T
  include Comparable.Make (T)
end

module Block_index : sig
  type t [@@deriving sexp]

  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int [@@deriving sexp]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Cnstr_info : sig
  type t [@@deriving sexp_of]

  (* TODO: Consider exposing a function to zip with the list of args. I think the
     callsites generally do that, which leads to looking up each constructor separately. *)

  val tag : t -> Cnstr.t -> Cnstr_tag.t
  val arg_type : t -> Cnstr.t -> Block_index.t -> Module_path.absolute Type_scheme.type_
  val cnstrs : t -> Cnstr.t list

  val fold
    :  t
    -> init:'acc
    -> f:
         ('acc
          -> Cnstr.t
          -> Cnstr_tag.t
          -> Module_path.absolute Type_scheme.type_ list
          -> 'acc)
    -> 'acc

  val of_variants : (Cnstr_name.t * Module_path.absolute Type_scheme.type_ list) list -> t
  val of_tuple : Module_path.absolute Type_scheme.type_ list -> t
end = struct
  type t =
    | Variants of
        { constant_cnstrs : Cnstr_name.t list
        ; non_constant_cnstrs :
            (Cnstr_name.t * Module_path.absolute Type_scheme.type_ list) list
        }
    | Tuple of Module_path.absolute Type_scheme.type_ list
  [@@deriving sexp_of]

  let of_variants variants =
    let constant_cnstrs, non_constant_cnstrs =
      List.partition_map variants ~f:(fun (cnstr_name, args) ->
        if List.is_empty args then First cnstr_name else Second (cnstr_name, args))
    in
    Variants { constant_cnstrs; non_constant_cnstrs }
  ;;

  let of_tuple args = Tuple args

  let fold t ~init:acc ~f =
    match t with
    | Variants { constant_cnstrs; non_constant_cnstrs } ->
      let acc =
        List.foldi constant_cnstrs ~init:acc ~f:(fun i acc cnstr_name ->
          f acc (Cnstr.Named cnstr_name) (Cnstr_tag.of_int i) [])
      in
      List.foldi non_constant_cnstrs ~init:acc ~f:(fun i acc (cnstr_name, args) ->
        f acc (Named cnstr_name) (Cnstr_tag.of_int i) args)
    | Tuple args -> f acc Tuple Cnstr_tag.default args
  ;;

  let cnstrs t = List.rev (fold t ~init:[] ~f:(fun acc cnstr _tag _args -> cnstr :: acc))

  let lookup_cnstr t cnstr =
    match t, (cnstr : Cnstr.t) with
    | Variants { constant_cnstrs; non_constant_cnstrs }, Named cnstr_name ->
      (match
         List.findi constant_cnstrs ~f:(fun (_ : int) -> Cnstr_name.( = ) cnstr_name)
       with
       | Some (i, (_ : Cnstr_name.t)) -> Cnstr_tag.of_int i, []
       | None ->
         (match
            List.findi non_constant_cnstrs ~f:(fun _ ->
              Cnstr_name.( = ) cnstr_name << fst)
          with
          | Some (i, (_, args)) -> Cnstr_tag.of_int i, args
          | None ->
            compiler_bug
              [%message
                "Constructor name lookup failed"
                  (cnstr_name : Cnstr_name.t)
                  ~cnstr_info:(t : t)]))
    | Tuple args, Tuple -> Cnstr_tag.default, args
    | Variants _, Tuple | Tuple _, Named _ ->
      compiler_bug
        [%message "Incompatible cnstr info" (cnstr : Cnstr.t) ~cnstr_info:(t : t)]
  ;;

  let tag t cnstr = fst (lookup_cnstr t cnstr)

  let arg_type t cnstr index =
    List.nth_exn (snd (lookup_cnstr t cnstr)) (Block_index.to_int index)
  ;;
end

module Fun_decl = struct
  type t =
    { name : Mir_name.t
    ; arity : int
    }
  [@@deriving sexp_of]
end

module Extern_decl = struct
  type t =
    { name : Mir_name.t
    ; extern_name : Extern_name.t
    ; arity : int
    }
  [@@deriving sexp_of]
end

(* TODO: This doesn't handle polymorphic types particularly smartly. Should think about
   whether that matters. Actually, since this is only used for extern declarations, we
   don't need to be super clever about this. *)
let rec arity_of_type ~names (type_ : Module_path.absolute Type_scheme.type_) =
  match type_ with
  | Var _ | Tuple _ -> 0
  | Type_app (type_name, _) ->
    (match
       snd (Name_bindings.find_absolute_type_decl ~defs_only:true names type_name)
     with
     | Abstract | Variants _ | Record _ -> 0
     | Alias type_ -> arity_of_type ~names type_)
  | Function (args, _, _) -> Nonempty.length args
  | Union types | Intersection types ->
    let types = Non_single_list.to_list types in
    let type_arities = List.map types ~f:(arity_of_type ~names) in
    (match List.all_equal type_arities ~equal:Int.equal with
     | Some arity -> arity
     | None ->
       compiler_bug
         [%message
           "Conflicting arities for union or intersection type"
             (type_ : _ Type_scheme.type_)
             (type_arities : int list)])
;;

module Context : sig
  type t [@@deriving sexp_of]

  val create : names:Name_bindings.t -> name_table:Mir_name.Name_table.t -> t
  val add_value_name : t -> Value_name.t -> t * Mir_name.t
  val copy_name : t -> Mir_name.t -> Mir_name.t

  module Extern_info : sig
    type t =
      | Local
      | External of { arity : int }
      | Bool_intrinsic of { tag : Cnstr_tag.t }
    [@@deriving sexp_of]
  end

  val find_value_name : t -> Value_name.Absolute.t -> Mir_name.t * Extern_info.t
  val find_value_name_assert_local : t -> Value_name.t -> Mir_name.t
  val find_value_name_assert_external : t -> Value_name.t -> Mir_name.t
  val peek_value_name : t -> Value_name.Absolute.t -> Mir_name.t option

  type find_override := Value_name.Absolute.t -> Mir_name.t -> Mir_name.t option

  val with_find_override : t -> f:find_override -> t
  val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
  val current_path : t -> Module_path.Absolute.t
  val find_cnstr_info : t -> Module_path.absolute Type_scheme.type_ -> Cnstr_info.t

  val find_cnstr_info_from_decl
    :  t
    -> Module_path.absolute Type_decl.decl
    -> follow_aliases:bool
    -> Cnstr_info.t option
end = struct
  module Extern_info = struct
    type t =
      | Local
      | External of { arity : int }
      | Bool_intrinsic of { tag : Cnstr_tag.t }
    [@@deriving sexp_of]
  end

  type t =
    { expr_local_names : Mir_name.t Value_name.Absolute.Map.t
    ; toplevel_names : (Mir_name.t * Extern_info.t) Value_name.Absolute.Table.t
    ; module_path : Module_path.Absolute.t
    ; name_bindings : (Name_bindings.t[@sexp.opaque])
    ; name_table : (Mir_name.Name_table.t[@sexp.opaque])
    ; find_override :
        (Value_name.Absolute.t -> Mir_name.t -> Mir_name.t option[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let with_module t module_name ~f =
    let name_bindings =
      Name_bindings.into_module t.name_bindings module_name ~place:`Def
    in
    let t, x = f { t with name_bindings } in
    { t with name_bindings = Name_bindings.into_parent name_bindings }, x
  ;;

  let current_path t = Name_bindings.current_path t.name_bindings
  let copy_name t name = Mir_name.copy_name t.name_table name

  let lookup_toplevel_name t ((path, _) as name) =
    let entry = Name_bindings.find_absolute_entry t.name_bindings name in
    let extern_name = Name_bindings.Name_entry.extern_name entry in
    let fallback_to_external () : Extern_info.t =
      let scheme =
        match Name_bindings.Name_entry.type_ entry with
        | Scheme scheme -> scheme
        | Type _ ->
          compiler_bug
            [%message
              "Didn't find type scheme for external name entry"
                (name : Value_name.Absolute.t)
                (entry : Name_bindings.Name_entry.t)]
      in
      External { arity = arity_of_type ~names:t.name_bindings (fst scheme) }
    in
    let extern_info : Extern_info.t =
      match extern_name with
      | None ->
        if Module_path.is_prefix path ~prefix:t.module_path
        then Local
        else fallback_to_external ()
      | Some extern_name ->
        (match Extern_name.to_ustring extern_name |> Ustring.to_string with
         | "%false" -> Bool_intrinsic { tag = Cnstr_tag.of_int 0 }
         | "%true" -> Bool_intrinsic { tag = Cnstr_tag.of_int 1 }
         | _ -> fallback_to_external ())
    in
    let mir_name = Mir_name.create_exportable_name name in
    mir_name, extern_info
  ;;

  let peek_toplevel_name_internal t name =
    try
      Some
        (Hashtbl.find_or_add t.toplevel_names name ~default:(fun () ->
           lookup_toplevel_name t name))
    with
    | Compilation_error.Compilation_error { kind = Name_error; _ } -> None
  ;;

  let peek_value_name_internal t name : (Mir_name.t * Extern_info.t) option =
    match Map.find t.expr_local_names name with
    | Some mir_name -> Some (mir_name, Local)
    | None -> peek_toplevel_name_internal t name
  ;;

  let add_value_name t name =
    let path = Name_bindings.current_path t.name_bindings in
    let name = path, name in
    let mir_name = Mir_name.create_value_name t.name_table name in
    ( { t with expr_local_names = Map.set t.expr_local_names ~key:name ~data:mir_name }
    , mir_name )
  ;;

  let find_value_name t name : Mir_name.t * Extern_info.t =
    match peek_value_name_internal t name with
    | Some ((name', _) as entry) ->
      (match t.find_override name name' with
       | Some name_override -> name_override, Local
       | None -> entry)
    | None ->
      compiler_bug
        [%message "Name missing from context" (name : Value_name.Absolute.t) (t : t)]
  ;;

  let peek_value_name t name = peek_value_name_internal t name |> Option.map ~f:fst

  let find_value_name_assert_internal t name ~extern_info_matches ~expected =
    let name, extern_info =
      find_value_name t (Name_bindings.current_path t.name_bindings, name)
    in
    if not (extern_info_matches extern_info)
    then
      compiler_bug
        [%message
          "Unexpected extern info value"
            (name : Mir_name.t)
            (extern_info : Extern_info.t)
            (expected : string)];
    name
  ;;

  let find_value_name_assert_local =
    find_value_name_assert_internal ~expected:"local" ~extern_info_matches:(function
      | Local | Bool_intrinsic _ -> true
      | External _ -> false)
  ;;

  let find_value_name_assert_external =
    find_value_name_assert_internal ~expected:"external" ~extern_info_matches:(function
      | External _ | Bool_intrinsic _ -> true
      | Local -> false)
  ;;

  let with_find_override t ~f =
    { t with
      find_override =
        (fun name name' ->
          match t.find_override name name' with
          | Some _ as name -> name
          | None -> f name name')
    }
  ;;

  let create ~names:name_bindings ~name_table =
    { expr_local_names = Value_name.Absolute.Map.empty
    ; toplevel_names = Value_name.Absolute.Table.create ()
    ; module_path = Name_bindings.current_path name_bindings
    ; name_bindings
    ; name_table
    ; find_override = (fun _ _ -> None)
    }
  ;;

  let cnstr_info_lookup_failed type_ =
    compiler_bug
      [%message
        "Constructor info lookup failed" (type_ : Module_path.absolute Type_scheme.type_)]
  ;;

  let rec find_cnstr_info_internal t (type_ : Module_path.absolute Type_scheme.type_) =
    match type_ with
    | Type_app (type_name, _args) ->
      let decl =
        snd
          (Name_bindings.find_absolute_type_decl
             ~defs_only:true
             t.name_bindings
             type_name)
      in
      find_cnstr_info_from_decl t decl ~follow_aliases:true
    | Tuple args -> Some (Cnstr_info.of_tuple args)
    | Function _ | Var _ -> cnstr_info_lookup_failed type_
    | Union _ | Intersection _ ->
      failwith "TODO: handle cnstr info lookup for union and intersection types"

  and find_cnstr_info_from_decl t decl ~follow_aliases =
    match (decl : _ Type_decl.decl) with
    | Alias alias -> if follow_aliases then find_cnstr_info_internal t alias else None
    | Variants variants -> Some (Cnstr_info.of_variants variants)
    | Abstract | Record _ -> None
  ;;

  let find_cnstr_info t type_ =
    Option.value_or_thunk (find_cnstr_info_internal t type_) ~default:(fun () ->
      compiler_bug
        [%message
          "Constructor info lookup failed"
            (type_ : Module_path.absolute Type_scheme.type_)])
  ;;
end

module Simple_pattern : sig
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr.t * t list
  [@@deriving sexp, variants]

  val flatten_typed_pattern : Typed.Pattern.t -> t Nonempty.t
  val flatten_typed_pattern_no_unions : Typed.Pattern.t -> label:string -> t
  val names : t -> Value_name.Set.t

  module Coverage : sig
    type simple_pattern = t
    type t

    val of_pattern : simple_pattern -> t
    val of_patterns : simple_pattern Nonempty.t -> t
    val combine : t -> t -> t

    val missing_cases
      :  t
      -> ctx:Context.t
      -> input_type:Module_path.absolute Type_scheme.type_
      -> simple_pattern list
  end
end = struct
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr.t * t list
  [@@deriving sexp, variants]

  let names =
    let rec loop acc t =
      match t with
      | Constant _ | Catch_all None -> acc
      | Catch_all (Some name) -> Set.add acc name
      | As (t, name) -> loop (Set.add acc name) t
      | Cnstr_appl (_, ts) -> List.fold ts ~init:acc ~f:loop
    in
    fun t -> loop Value_name.Set.empty t
  ;;

  let flatten_typed_pattern pattern =
    let rec loop : Typed.Pattern.t -> t Nonempty.t = function
      | Constant lit -> [ Constant lit ]
      | Catch_all name -> [ Catch_all name ]
      | As (pattern, name) -> Nonempty.map (loop pattern) ~f:(Fn.flip as_ name)
      | Cnstr_appl ((_, cnstr_name), args) -> of_cnstr_appl (Cnstr.Named cnstr_name) args
      | Tuple fields -> of_cnstr_appl Tuple fields
      | Record _fields ->
        (*let field_names, field_patterns =
          List.fold
            fields
            ~init:(Value_name.Set.empty, [])
            ~f:(fun (field_names, field_patterns) (name, pattern) ->
            let pattern =
              Option.value pattern ~default:(Catch_all (Some name))
            in
            Set.add field_names name, pattern :: field_patterns)
        in
        of_cnstr_appl (record_cnstr_name field_names) (List.rev field_patterns)*)
        failwith "TODO: flatten_typed_pattern: record patterns"
      | Union (pat1, pat2) -> Nonempty.(loop pat1 @ loop pat2)
      | Type_annotation _ -> .
    and of_cnstr_appl cnstr args =
      match Nonempty.of_list args with
      | None -> [ Cnstr_appl (cnstr, []) ]
      | Some args ->
        Nonempty.map args ~f:loop
        |> Nonempty.cartesian_product_all
        |> Nonempty.map ~f:(fun args -> Cnstr_appl (cnstr, Nonempty.to_list args))
    in
    loop pattern
  ;;

  let flatten_typed_pattern_no_unions pattern ~label =
    match flatten_typed_pattern pattern with
    | [ arg ] -> arg
    | _ :: _ :: _ ->
      let msg = [%string "Pattern unions in %{label} are not supported"] in
      Compilation_error.raise Mir_error ~msg:[%message msg (pattern : Typed.Pattern.t)]
  ;;

  module Coverage = struct
    type simple_pattern = t

    type t =
      (* TODO: We use `largest_seen` to keep track of an example of a literal value not
         included in a union of literal patterns. But this won't work if you match on the
         max value of an int and also match on the lowest value, since we will report
         `Int.max_value + 1 == Int.min_value` as the not-covered value, even though it was
         covered. There are similar issues for chars/floats. For ints/floats/chars, we
         can probably do something like keep track of the covered ranges. *)
      | Inexhaustive : { largest_seen : Literal.t } -> t
      | Exhaustive
      | By_cnstr : t list Cnstr.Map.t -> t

    let rec of_pattern : simple_pattern -> t = function
      | Constant lit -> Inexhaustive { largest_seen = lit }
      | Catch_all _ -> Exhaustive
      | As (pat, _) -> of_pattern pat
      | Cnstr_appl (cnstr, args) ->
        By_cnstr (Cnstr.Map.singleton cnstr (List.map ~f:of_pattern args))
    ;;

    let rec combine coverage coverage' =
      match coverage, coverage' with
      | Exhaustive, _ | _, Exhaustive -> Exhaustive
      | Inexhaustive { largest_seen = x }, Inexhaustive { largest_seen = y } ->
        Inexhaustive
          { largest_seen =
              (match x, y with
               | Int i, Int i' -> Int (max i i')
               | Float x, Float x' -> Float (Float.max x x')
               | Char c, Char c' -> Char (Uchar.max c c')
               | String s, String s' -> String (Ustring.max s s')
               | _ ->
                 compiler_bug
                   [%message
                     "Incompatible literals in pattern" (x : Literal.t) (y : Literal.t)])
          }
      | Inexhaustive _, (By_cnstr _ as by_cnstr)
      | (By_cnstr _ as by_cnstr), Inexhaustive _ -> by_cnstr
      | By_cnstr coverage_by_cnstr, By_cnstr coverage_by_cnstr' ->
        merge_by_cnstr coverage_by_cnstr coverage_by_cnstr'

    and merge_by_cnstr coverage_by_cnstr coverage_by_cnstr' =
      By_cnstr
        (Map.merge_skewed coverage_by_cnstr coverage_by_cnstr' ~combine:(fun ~key:_ ->
           List.map2_exn ~f:combine))
    ;;

    let add_pattern coverage pattern =
      match coverage with
      | Exhaustive -> Exhaustive
      | Inexhaustive _ -> of_pattern pattern
      | By_cnstr coverage_by_cnstr ->
        (match of_pattern pattern with
         | Exhaustive -> Exhaustive
         | Inexhaustive _ -> coverage
         | By_cnstr coverage_by_cnstr' ->
           merge_by_cnstr coverage_by_cnstr coverage_by_cnstr')
    ;;

    let of_patterns Nonempty.(pattern :: patterns) =
      List.fold_until patterns ~init:(of_pattern pattern) ~f:(fun coverage pattern ->
        match add_pattern coverage pattern with
        | Exhaustive -> Stop Exhaustive
        | coverage -> Continue coverage)
      |> Fold_action.id
    ;;

    let asterisk = Ustring.of_string_exn "*"

    let rec missing_cases coverage ~ctx ~input_type =
      match coverage with
      | Exhaustive -> []
      | Inexhaustive { largest_seen } ->
        [ Constant
            (match largest_seen with
             | Int i -> Int (i + 1)
             | Float x -> Float (x +. 1.)
             | Char c -> Char (Option.value (Uchar.succ c) ~default:Uchar.min_value)
             | String s -> String (Ustring.( ^ ) asterisk s))
        ]
      | By_cnstr coverage_by_cnstr ->
        let cnstr_info = Context.find_cnstr_info ctx input_type in
        let all_cnstrs = Cnstr_info.cnstrs cnstr_info |> Cnstr.Set.of_list in
        let arity = Set.length all_cnstrs in
        let missing_cnstrs =
          Set.diff all_cnstrs (Map.key_set coverage_by_cnstr)
          |> Set.to_list
          |> List.map ~f:(fun cnstr ->
               Cnstr_appl (cnstr, List.init arity ~f:(const (Catch_all None))))
        in
        let missing_in_args =
          Map.to_alist coverage_by_cnstr
          |> List.concat_map ~f:(fun (cnstr, args) ->
               match Nonempty.of_list args with
               | None -> []
               | Some args ->
                 let missing_cases_per_arg =
                   Nonempty.mapi args ~f:(fun i arg ->
                     let input_type =
                       Cnstr_info.arg_type cnstr_info cnstr (Block_index.of_int i)
                     in
                     missing_cases ~ctx ~input_type arg)
                 in
                 if Nonempty.for_all ~f:List.is_empty missing_cases_per_arg
                 then []
                 else
                   Nonempty.map missing_cases_per_arg ~f:(function
                     | x :: xs -> Nonempty.(x :: xs)
                     | [] -> [ Catch_all None ])
                   |> Nonempty.cartesian_product_all
                   |> Nonempty.map ~f:(fun args ->
                        Cnstr_appl (cnstr, Nonempty.to_list args))
                   |> Nonempty.to_list)
        in
        missing_cnstrs @ missing_in_args
    ;;
  end
end

module Expr = struct
  type t =
    | Primitive of Literal.t
    | Name of Mir_name.t
    (* TODO: recursive lets? Mutual recursion? (will surely need a rec flag at least)
       Can maybe handle that in toplevel function definitions. *)
    | Let of Mir_name.t * t * t
    | Fun_call of Mir_name.t * t Nonempty.t
    | Make_block of
        { tag : Cnstr_tag.t
        ; fields : t list [@sexp.omit_nil]
        }
    | Get_block_field of Block_index.t * t
    | Cond_assign of
        { vars : Mir_name.t list [@sexp.omit_nil]
        ; conds : (cond * (t list[@sexp.omit_nil])) Nonempty.t
        ; body : t
        ; if_none_matched : cond_if_none_matched
        }

  and cond =
    (* TODO: In practice, the expressions in conditions have to be simple names. Maybe we
       should enforce that in the type here. *)
    | Equals of t * Literal.t
    | Constant_tag_equals of t * Cnstr_tag.t
    | Non_constant_tag_equals of t * Cnstr_tag.t
    | And of cond * cond

  and cond_if_none_matched =
    | Otherwise of t
    | Use_bindings of t list
  [@@deriving sexp_of]

  let rec map t ~f =
    match (f t : _ Map_action.t) with
    | Halt t -> t
    | Retry t -> map t ~f
    | Defer t ->
      (match t with
       | Name _ | Primitive _ -> t
       | Let (name, t, t') -> Let (name, map t ~f, map t' ~f)
       | Fun_call (fun_name, args) -> Fun_call (fun_name, Nonempty.map args ~f:(map ~f))
       | Make_block { tag; fields } ->
         Make_block { tag; fields = List.map fields ~f:(map ~f) }
       | Get_block_field (index, t) -> Get_block_field (index, map t ~f)
       | Cond_assign { vars; conds; body; if_none_matched } ->
         Cond_assign
           { vars
           ; conds = Nonempty.map conds ~f:(Tuple2.map_snd ~f:(List.map ~f:(map ~f)))
           ; body = map body ~f
           ; if_none_matched =
               (match if_none_matched with
                | Otherwise t -> Otherwise (map t ~f)
                | Use_bindings bindings -> Use_bindings (List.map bindings ~f:(map ~f)))
           })
  ;;

  module Fun_def = struct
    type nonrec t =
      { fun_name : Mir_name.t
      ; args : Mir_name.t Nonempty.t
      ; body : t
      }
    [@@deriving sexp_of]
  end

  (** Atomic expressions are those that are syntactically guaranteed to have no
      side-effects, not even allocating memory. This allows it to be safely duplicated
      when processing the AST. *)
  let is_atomic : t -> bool = function
    | Primitive _ | Name _ | Make_block { tag = _; fields = [] } | Get_block_field _ ->
      true
    | Fun_call _ | Let _ | Make_block { tag = _; fields = _ :: _ } | Cond_assign _ ->
      false
  ;;

  (* TODO: consider merging making bindings with making conditions. If we extended
     [Coverage.t] to include some notion of what conditions have been tested as well, it
     could help us avoid unnecessary checks. *)

  (** [fold_pattern_bindings] folds over the variable bindings formed by a pattern
      associated with an expression. The expression passed in should be atomic as it will
      be duplicated when generating bindings. *)
  let fold_pattern_bindings =
    let rec loop ~ctx ~add_let ~add_name acc pat mir_expr type_ =
      match (pat : Simple_pattern.t) with
      | Catch_all None -> ctx, acc
      | Catch_all (Some name) ->
        let ctx, name = add_name ctx name in
        ctx, add_let acc name mir_expr
      | As (pattern, name) ->
        let ctx, name = add_name ctx name in
        let acc = add_let acc name mir_expr in
        loop ~ctx ~add_let ~add_name acc pattern (Name name) type_
      | Cnstr_appl (cnstr, args) ->
        let cnstr_info = Context.find_cnstr_info ctx type_ in
        List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
          let arg_index = Block_index.of_int i in
          let arg_type = Cnstr_info.arg_type cnstr_info cnstr arg_index in
          let arg_expr = Get_block_field (arg_index, mir_expr) in
          loop ~ctx ~add_let ~add_name acc arg arg_expr arg_type)
      | Constant _ -> ctx, acc
    in
    fun ~ctx ~init:acc ~add_name ~add_let pat mir_expr type_ ->
      loop ~ctx ~add_let ~add_name acc pat mir_expr type_
  ;;

  let make_atomic ~ctx ~default ~add_name ~add_let ~binding_name mir_expr =
    if is_atomic mir_expr
    then ctx, default, mir_expr
    else (
      let ctx_for_body, expr_name = add_name ctx binding_name in
      let acc = add_let expr_name mir_expr in
      ctx_for_body, acc, Name expr_name)
  ;;

  let rec condition_of_pattern ~ctx ~input_expr ~input_type pattern =
    match (pattern : Simple_pattern.t) with
    | Catch_all _ -> None
    | As (pattern, _) -> condition_of_pattern ~ctx ~input_expr ~input_type pattern
    | Constant lit -> Some (Equals (input_expr, lit))
    | Cnstr_appl (cnstr, args) ->
      let cnstr_info = Context.find_cnstr_info ctx input_type in
      let tag = Cnstr_info.tag cnstr_info cnstr in
      if List.is_empty args
      then Some (Constant_tag_equals (input_expr, tag))
      else (
        let tag_cond = Non_constant_tag_equals (input_expr, tag) in
        let conds =
          List.filter_mapi args ~f:(fun i arg ->
            let arg_index = Block_index.of_int i in
            (* FIXME: Cnstr info contains the general type of the constructor, but it may
               have a more concrete type at this application. We either need to:
               
               1) Grab the type from the AST (make the previous passes put it there!)
               2) Instantiate the type ourselves (sounds rough) *)
            let arg_type = Cnstr_info.arg_type cnstr_info cnstr arg_index in
            let arg_expr = Get_block_field (arg_index, input_expr) in
            condition_of_pattern ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
        in
        Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond'))))
  ;;

  (* TODO: switch statement optimization
     See:
     - https://github.com/ocaml/ocaml/blob/trunk/lambda/matching.ml
     - https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching  *)

  module Just_bound = struct
    type t =
      | Rec of
          { this_name : Mir_name.t
          ; other_names : Mir_name.Set.t
          }
      | Nonrec of { this_pattern_names : Mir_name.Set.t }
    [@@deriving sexp_of]
  end

  let add_let_bindings ~bindings ~body =
    List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
      match mir_expr with
      | Name name' when Mir_name.equal name name' ->
        (* TODO: Consider getting rid of this name eliding logic. We do it to give
           functions in MIR more readable names, and to avoid extra indirection from
           defining a function with a name like `Foo.*fun.1` and then having to define
           a value `Foo.foo.1` and a function `Foo.foo` to call it. There's got to be a
           better way than passing down the names we bind and them sometimes returning one
           of them back to elide the let binding though. The current setup is quite
           fragile and hard to maintain. *)
        (* Avoid genereating code that looks like let x.0 = x.0 *)
        body
      | _ -> Let (name, mir_expr, body))
  ;;

  let rec check_rec_binding_expr expr =
    match (expr : _ Typed.Expr.t) with
    | Lambda _ -> ()
    | Let { body; bindings = _; rec_ = _ } ->
      Node.with_value body ~f:check_rec_binding_expr
    | Match (_, _, arms) ->
      Nonempty.iter arms ~f:(fun (_, expr) ->
        Node.with_value expr ~f:check_rec_binding_expr)
    | Handle _
    | Name _
    | Tuple _
    | Record_literal _
    | Fun_call _
    | Record_update _
    | Record_field_access _ ->
      (* TODO: Consider relaxing this to allow more kinds of expressions e.g. function
         calls which don't mention the recursive names.
         See: https://v2.ocaml.org/manual/letrecvalues.html *)
      Compilation_error.raise
        Mir_error
        ~msg:
          [%message
            "Recursive let bindings can only be used to define functions"
              (expr : _ Typed.Expr.t)]
    | Literal _ ->
      compiler_bug
        [%message "Impossible expr in recursive binding" (expr : _ Typed.Expr.t)]
  ;;

  let check_rec_binding_pattern pat =
    match (pat : Typed.Pattern.t) with
    | Catch_all _ -> ()
    | _ ->
      Compilation_error.raise
        Mir_error
        ~msg:
          [%message
            "Only variables are allowed as the left-hand side of recursive let bindings"
              (pat : Typed.Pattern.t)]
  ;;

  let generate_let_bindings
    ~ctx
    ~ctx_for_body
    ~rec_
    ~init
    ~add_let
    ~extract_binding
    ~process_expr
    bindings
    =
    let all_names_bound =
      lazy
        (Nonempty.fold
           bindings
           ~init:Mir_name.Set.empty
           ~f:(fun all_names_bound (_, names_bound) ->
           Set.union all_names_bound names_bound))
    in
    let ctx = if rec_ then ctx_for_body else ctx in
    (* TODO: Warn about let bindings which bind no names and are pure. *)
    Nonempty.fold
      bindings
      ~init:(ctx_for_body, init)
      ~f:(fun (ctx_for_body, acc) (binding, names_bound) ->
      let pat, expr, ((typ, _constraints) : _ Type_scheme.t) = extract_binding binding in
      let just_bound : Just_bound.t =
        if rec_
        then (
          Node.with_value expr ~f:check_rec_binding_expr;
          Node.with_value pat ~f:check_rec_binding_pattern;
          assert_or_compiler_bug ~here:[%here] (Set.length names_bound = 1);
          let this_name = Set.choose_exn names_bound in
          Rec { this_name; other_names = Set.remove (force all_names_bound) this_name })
        else Nonrec { this_pattern_names = names_bound }
      in
      (* TODO: support unions in let bindings. For the non-rec case we should
         just be able to convert to a match *)
      let pat' =
        Node.with_value pat ~f:(fun pat ->
          let pat' =
            Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"let bindings"
          in
          let missing_cases =
            Simple_pattern.Coverage.(
              of_pattern pat' |> missing_cases ~ctx ~input_type:typ)
          in
          if not (List.is_empty missing_cases)
          then
            Compilation_error.raise
              Mir_error
              ~msg:
                [%message
                  "The pattern in this let binding is not exhaustive"
                    ~pattern:(pat : Typed.Pattern.t)
                    (missing_cases : Simple_pattern.t list)];
          pat')
      in
      let acc, mir_expr = process_expr acc ~just_bound ~ctx expr typ in
      let add_name ctx name = ctx, Context.find_value_name_assert_local ctx name in
      let binding_name, add_binding_name =
        match pat' with
        | Catch_all (Some name) | As (_, name) -> name, add_name
        | Catch_all None -> Constant_names.underscore, Context.add_value_name
        | Constant _ | Cnstr_appl _ -> Constant_names.binding, Context.add_value_name
      in
      Node.with_value mir_expr ~f:(fun mir_expr ->
        let ctx_for_body, acc, mir_expr =
          make_atomic
            ~ctx:ctx_for_body
            ~default:acc
            ~add_let:(fun name mir_expr -> add_let acc name mir_expr typ)
            ~binding_name
            ~add_name:add_binding_name
            mir_expr
        in
        fold_pattern_bindings
          ~ctx:ctx_for_body
          pat'
          mir_expr
          typ
          ~init:acc
          ~add_let:(fun acc name mir_expr -> add_let acc name mir_expr typ)
          ~add_name))
  ;;

  let try_rewriting_partial_application
    ~fun_
    ~(fun_type : _ Type_scheme.t)
    ~args_and_types
    ~current_path
    =
    match fst fun_type with
    | Type_app _ | Tuple _ | Var _ ->
      compiler_bug [%message "Non-funtion type in function call"]
    | Union _ | Intersection _ ->
      failwith
        "TODO: handle rewriting partial application for union and intersection types"
    | Function
        ( fun_arg_types
        , (_effects : _ Type_scheme.effects)
        , (_return_type : _ Type_scheme.type_) ) ->
      (match snd (Nonempty.zip fun_arg_types args_and_types) with
       | Same_length -> `Already_fully_applied
       | Right_trailing _ ->
         compiler_bug [%message "Over-application of arguments to function"]
       | Left_trailing unapplied_arg_types ->
         let fun_name = Constant_names.fun_ in
         let applied_args, bindings =
           Nonempty.mapi args_and_types ~f:(fun i (arg_expr, arg_type) ->
             let name = Constant_names.synthetic_arg i in
             let arg_pat_and_type =
               Node.dummy_span (Pattern.Catch_all (Some name), arg_type)
             in
             let arg_name = Node.dummy_span (Typed.Expr.Name (current_path, name)) in
             (arg_name, arg_type), (arg_pat_and_type, None, arg_expr))
           |> Nonempty.unzip
         in
         let bindings =
           Nonempty.cons
             (Node.dummy_span (Pattern.Catch_all (Some fun_name), fun_type), None, fun_)
             bindings
         in
         let n_applied_args = Nonempty.length applied_args in
         let unapplied_args, lambda_args =
           Nonempty.mapi unapplied_arg_types ~f:(fun i arg_type ->
             let i = i + n_applied_args in
             let name = Constant_names.synthetic_arg i in
             let arg_pattern = Node.dummy_span (Pattern.Catch_all (Some name)) in
             let arg_name = Node.dummy_span (Typed.Expr.Name (current_path, name)) in
             (* Setting zero constraints on [arg_type] is a bit dodgy, but we don't use
                the constraints so it's fine. *)
             (arg_name, (arg_type, [])), arg_pattern)
           |> Nonempty.unzip
         in
         let args = Nonempty.append applied_args unapplied_args in
         let expr_as_lambda : _ Typed.Expr.t =
           Let
             { rec_ = false
             ; bindings
             ; body =
                 Node.dummy_span
                   (Typed.Expr.Lambda
                      ( lambda_args
                      , Node.dummy_span
                          (Typed.Expr.Fun_call
                             ( Node.dummy_span (Typed.Expr.Name (current_path, fun_name))
                             , fun_type
                             , args )) ))
             }
         in
         `Rewritten_partial_application expr_as_lambda)
  ;;

  let of_typed_expr
    ~just_bound:outer_just_bound
    ~ctx:outer_ctx
    ~(add_fun_def : Fun_def.t -> unit)
    ~(add_fun_decl : Fun_decl.t -> unit)
    outer_expr
    outer_type
    =
    let add_fun_def, is_local_fun_def =
      let local_fun_defs = Mir_name.Hash_set.create () in
      ( (fun (fun_def : Fun_def.t) ->
          Hash_set.add local_fun_defs fun_def.fun_name;
          add_fun_def fun_def)
      , Hash_set.mem local_fun_defs )
    in
    let rec of_typed_expr ?just_bound ~ctx expr expr_type =
      match
        ( (expr : Module_path.absolute Type_scheme.t Typed.Expr.t)
        , (expr_type : _ Type_scheme.type_) )
      with
      | Literal lit, _ -> Primitive lit
      | Name name, _ ->
        let name, extern_info = Context.find_value_name ctx name in
        (match extern_info with
         | Local -> Name name
         | External { arity } ->
           add_fun_decl { name; arity };
           Name name
         | Bool_intrinsic { tag } -> Make_block { tag; fields = [] })
      | Fun_call (fun_, fun_type, args_and_types), body_type ->
        (match
           try_rewriting_partial_application
             ~fun_
             ~fun_type
             ~args_and_types
             ~current_path:(Context.current_path ctx)
         with
         | `Rewritten_partial_application expr_as_lambda ->
           of_typed_expr ?just_bound ~ctx expr_as_lambda expr_type
         | `Already_fully_applied ->
           let fun_call fun_ =
             let arg_types = Nonempty.map ~f:snd args_and_types in
             let fun_type : _ Type_scheme.type_ =
               (* Effect information is not preserved here. This is ~fine because we don't
                  actually use it. *)
               Function (Nonempty.map arg_types ~f:fst, Effect_union [], body_type)
             in
             let fun_ = of_typed_expr ~ctx fun_ fun_type in
             let args =
               Nonempty.map args_and_types ~f:(fun (arg, arg_type) ->
                 Node.with_value arg ~f:(fun arg -> of_typed_expr ~ctx arg (fst arg_type)))
             in
             match fun_ with
             | Name fun_name -> Fun_call (fun_name, args)
             | Let _ | Fun_call _ | Get_block_field _ | Cond_assign _ ->
               let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
               Let (fun_name, fun_, Fun_call (fun_name, args))
             | Primitive _ | Make_block _ ->
               compiler_bug [%message "Invalid function expression" (fun_ : t)]
           in
           Node.with_value fun_ ~f:(fun fun_ ->
             match fun_ with
             | Name (_, name) ->
               (* TODO: I think there's no need for this special-casing actually: we can
                  just use the constructor functions directly. *)
               (* Special-case constructor applications to make use of [Make_block]. *)
               (match Value_name.to_cnstr_name name with
                | Ok cnstr_name ->
                  let cnstr_info = Context.find_cnstr_info ctx expr_type in
                  let tag = Cnstr_info.tag cnstr_info (Named cnstr_name) in
                  let fields, field_types =
                    List.unzip (Nonempty.to_list args_and_types)
                  in
                  let field_types = List.map field_types ~f:fst in
                  make_block ~ctx ~tag ~fields ~field_types
                | Error _ -> fun_call fun_)
             | _ -> fun_call fun_))
      | Lambda (args, body), Function (arg_types, _effect_row, body_type) ->
        add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound
      | Match (expr, input_type, arms), output_type ->
        let input_expr =
          Node.with_value expr ~f:(fun expr -> of_typed_expr ~ctx expr (fst input_type))
        in
        if is_atomic input_expr
        then
          (* Skip binding [match_expr_name] when matching on an atomic expression. *)
          handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
        else (
          let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
          let body =
            let input_expr = Name match_expr_name in
            handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
          in
          Let (match_expr_name, input_expr, body))
      | Handle _, _ -> failwith "Handle in MIR"
      | Let { rec_; bindings; body }, body_type ->
        let ctx_for_body, bindings =
          Nonempty.fold_map
            bindings
            ~init:ctx
            ~f:(fun ctx_for_body ((pat_and_type, _fixity, _expr) as binding) ->
            let ctx_for_body, names_bound =
              Node.with_value pat_and_type ~f:(fun (pattern, _) ->
                Pattern.Names.fold
                  pattern
                  ~init:(ctx_for_body, Mir_name.Set.empty)
                  ~f:(fun (ctx_for_body, names_bound) name ->
                  let ctx, name = Context.add_value_name ctx_for_body name in
                  ctx, Set.add names_bound name))
            in
            ctx_for_body, (binding, names_bound))
        in
        let ctx, bindings =
          generate_let_bindings
            ~ctx
            ~ctx_for_body
            ~rec_
            ~init:[]
            ~add_let:(fun bindings
                          name
                          mir_expr
                          (_ : Module_path.absolute Type_scheme.type_) ->
              (name, mir_expr) :: bindings)
            ~extract_binding:(fun (pat_and_type, (_ : Fixity.t option), expr) ->
              Node.map pat_and_type ~f:fst, expr, Node.with_value pat_and_type ~f:snd)
            ~process_expr:(fun bindings ~just_bound ~ctx expr typ ->
              ( bindings
              , Node.map expr ~f:(fun expr -> of_typed_expr ~just_bound ~ctx expr typ) ))
            bindings
        in
        let body =
          (* Pass through [just_bound], but only if we generated no bindings. It 
             wouldn't be correct to use it otherwise, as we might not be able to elide
             the resulting binding if it is inside some nested `let` bindings. *)
          let just_bound =
            match just_bound with
            | None -> None
            | Some _ ->
              if List.for_all bindings ~f:(fun (name, mir_expr) ->
                   match mir_expr with
                   | Name mir_name -> Mir_name.equal name mir_name
                   | _ -> false)
              then just_bound
              else None
          in
          Node.with_value body ~f:(fun body ->
            of_typed_expr ?just_bound ~ctx body body_type)
        in
        add_let_bindings ~bindings ~body
      | Tuple fields, Tuple field_types ->
        make_block ~ctx ~tag:Cnstr_tag.default ~fields ~field_types
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message
            "Incompatible expr and type"
              (expr : _ Type_scheme.t Typed.Expr.t * _ Type_scheme.type_)]
      | _, (Union _ | Intersection _) ->
        failwith "TODO: handle mir conversion for union and intersection types"
    and add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound =
      (* Keep track of the parent context before binding any variables. This lets us
         check which variables are captured by closures later on. *)
      let parent_ctx = ctx in
      let (ctx, bindings), args =
        Nonempty.zip_exn args arg_types
        |> Nonempty.fold_map ~init:(ctx, []) ~f:(fun (ctx, bindings) (arg, arg_type) ->
             Node.with_value arg ~f:(fun arg ->
               let arg =
                 Simple_pattern.flatten_typed_pattern_no_unions
                   arg
                   ~label:"function argument patterns"
               in
               match arg with
               | Catch_all (Some arg_name) ->
                 (* Special-case named catch-all patterns (the dominant case) to skip the
                    [lambda_arg] step and use the name directly. *)
                 let ctx, arg_name = Context.add_value_name ctx arg_name in
                 (ctx, bindings), arg_name
               | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
                 let ctx, arg_name =
                   Context.add_value_name ctx Constant_names.lambda_arg
                 in
                 let add_let acc name mir_expr = (name, mir_expr) :: acc in
                 let ctx, bindings =
                   fold_pattern_bindings
                     ~ctx
                     arg
                     (Name arg_name)
                     arg_type
                     ~init:bindings
                     ~add_let
                     ~add_name:Context.add_value_name
                 in
                 (ctx, bindings), arg_name))
      in
      let recursively_bound_names =
        match just_bound with
        | Some (Rec { this_name; other_names }) -> Set.add other_names this_name
        | Some (Nonrec _) | None -> Mir_name.Set.empty
      in
      let closed_over = ref Mir_name.Map.empty in
      let close_over_name mir_name =
        match Map.find !closed_over mir_name with
        | Some name -> name
        | None ->
          let new_name = Context.copy_name ctx mir_name in
          closed_over := Map.set !closed_over ~key:mir_name ~data:new_name;
          new_name
      in
      let ctx =
        let from_which_context name mir_name =
          let in_context ctx =
            Context.peek_value_name ctx name
            |> Option.value_map ~default:false ~f:(Mir_name.equal mir_name)
          in
          if in_context outer_ctx || is_local_fun_def mir_name
          then `From_toplevel
          else if Set.mem recursively_bound_names mir_name
          then `Recursively_bound
          else if in_context parent_ctx
          then `Closed_over_from_parent
          else `Newly_bound
        in
        (* Determine if names looked up were closed over from the parent context. *)
        Context.with_find_override ctx ~f:(fun name mir_name ->
          match from_which_context name mir_name with
          | `Newly_bound | `Recursively_bound | `From_toplevel -> None
          | `Closed_over_from_parent -> Some (close_over_name mir_name))
      in
      let body =
        Node.with_value body ~f:(fun body -> of_typed_expr ~ctx body body_type)
      in
      let body = add_let_bindings ~bindings ~body in
      (* TODO: Consider having closures share an environment instead of closing over other
         mutually recursive closures. *)
      let fun_name =
        (* If we are a closure, [fun_name] can't be the same as the name we bind, since we
           aren't returning [Name fun_name] and relying on the assignment getting elided. *)
        match just_bound with
        | Some (Rec { this_name; _ }) when Map.is_empty !closed_over -> this_name
        | Some (Nonrec { this_pattern_names }) when Set.length this_pattern_names = 1 ->
          let name = Set.choose_exn this_pattern_names in
          if Map.is_empty !closed_over then name else Context.copy_name ctx name
        | Some (Rec _ | Nonrec _) | None ->
          snd (Context.add_value_name ctx Constant_names.fun_)
      in
      let (fun_def : Fun_def.t), fun_or_closure =
        if Map.is_empty !closed_over
        then { fun_name; args; body }, Name fun_name
        else (
          let (_ : Context.t), closure_env_name =
            Context.add_value_name ctx Constant_names.closure_env
          in
          let closure_env = Name closure_env_name in
          let body =
            match just_bound with
            | Some (Nonrec _) | None -> body
            | Some (Rec { this_name; other_names }) ->
              (* Fix up the body of closures so they close over other recursively bound
                 closures, and refer to themselves with a direct function call passing in
                 the closure environment. *)
              map body ~f:(function
                | Fun_call (name, args) when Mir_name.equal name this_name ->
                  Halt (Fun_call (fun_name, Nonempty.cons closure_env args))
                | Name name when Set.mem other_names name ->
                  Halt (Name (close_over_name name))
                | expr -> Defer expr)
          in
          let closed_over = !closed_over in
          let args = Nonempty.cons closure_env_name args in
          let (_ : int), body =
            Map.fold closed_over ~init:(1, body) ~f:(fun ~key:_ ~data:name (i, body) ->
              i + 1, Let (name, Get_block_field (Block_index.of_int i, closure_env), body))
          in
          let closure =
            Make_block
              { tag = Cnstr_tag.closure
              ; fields =
                  Name fun_name
                  :: List.map (Map.keys closed_over) ~f:(fun name -> Name name)
              }
          in
          { fun_name; args; body }, closure)
      in
      add_fun_def fun_def;
      fun_or_closure
    and make_block ~ctx ~tag ~fields ~field_types =
      let fields =
        List.map2_exn fields field_types ~f:(fun field_expr field_type ->
          Node.with_value field_expr ~f:(fun field_expr ->
            of_typed_expr ~ctx field_expr field_type))
      in
      Make_block { tag; fields }
    and handle_match_arms
      ~ctx
      ~input_expr
      ~input_type:((input_type, _constraints) : _ Type_scheme.t)
      ~output_type
      arms
      =
      let rec loop_one_arm ~pattern ~output_expr ~coverage arms =
        let patterns = Node.with_value pattern ~f:Simple_pattern.flatten_typed_pattern in
        let coverage' = Simple_pattern.Coverage.of_patterns patterns in
        let coverage =
          match coverage with
          | None -> coverage'
          | Some coverage -> Simple_pattern.Coverage.combine coverage coverage'
        in
        let fallback =
          if List.is_empty arms then None else Some (fun () -> loop ~coverage arms)
        in
        handle_single_arm
          ~ctx
          ~input_expr
          ~input_type
          ~output_expr
          ~output_type
          ~fallback
          patterns
      and loop ~coverage arms =
        match arms with
        | [] ->
          (match Simple_pattern.Coverage.missing_cases ~ctx ~input_type coverage with
           | [] ->
             compiler_bug [%message "Pattern coverage/condition checking is out of sync"]
           | missing_cases ->
             Compilation_error.raise
               Mir_error
               ~msg:
                 [%message
                   "This pattern match is not exhaustive"
                     (missing_cases : Simple_pattern.t list)])
        | (pattern, output_expr) :: arms ->
          loop_one_arm ~pattern ~output_expr ~coverage:(Some coverage) arms
      in
      let ((pattern, output_expr) :: arms) = arms in
      loop_one_arm ~pattern ~output_expr ~coverage:None arms
    and handle_single_arm
      ~ctx
      ~input_expr
      ~input_type
      ~output_expr
      ~output_type
      ~fallback
      patterns
      =
      let ctx, vars =
        List.fold_map
          (Simple_pattern.names (Nonempty.hd patterns) |> Set.to_list)
          ~init:ctx
          ~f:Context.add_value_name
      in
      let ctx, wrapping_binding, input_expr =
        make_atomic
          ~ctx
          ~default:None
          ~add_let:(fun name expr -> Some (name, expr))
          ~add_name:Context.add_value_name
          ~binding_name:Constant_names.binding
          input_expr
      in
      let body =
        Node.with_value output_expr ~f:(fun output_expr ->
          of_typed_expr ~ctx output_expr output_type)
      in
      let fold_result =
        (* TODO: Consider writing this is a regular recursive function. It might actually
           be easier to understand that way, and this version duplicates work by always
           generating the condition for the last pattern and throwing it away. *)
        Nonempty.fold_map_until ~init:() patterns ~f:(fun () pattern ->
          (* TODO: this should skip underscore bindings (bindings for no actual variables) *)
          let cond = condition_of_pattern ~ctx ~input_expr ~input_type pattern in
          let (_ : Context.t), bindings =
            (* Names are pre-added to the context above so they get the same unique names
               in different patterns. *)
            let add_name ctx name = ctx, Context.find_value_name_assert_local ctx name in
            let add_let bindings name expr = (name, expr) :: bindings in
            fold_pattern_bindings
              ~ctx
              pattern
              input_expr
              input_type
              ~init:[]
              ~add_let
              ~add_name
          in
          let bindings =
            (* Bindings must be sorted by their names to match up with [vars] above, which
               are also in sorted order. *)
            List.sort bindings ~compare:[%compare: Mir_name.t * _] |> List.map ~f:snd
          in
          match cond with
          | None ->
            (* TODO: warn about unused patterns (the rest of the patterns and following
               arms) *)
            Stop bindings
          | Some cond -> Continue ((), (cond, bindings)))
      in
      let add_last_unconditional_bindings ~conds ~last_bindings =
        match Nonempty.of_list conds with
        | Some conds ->
          Cond_assign { vars; conds; body; if_none_matched = Use_bindings last_bindings }
        | None ->
          (* If there are no other conditions, the result doesn't need [Cond_assign] and
             can be a regular unconditional expression. *)
          List.fold2_exn vars last_bindings ~init:body ~f:(fun acc name expr ->
            Let (name, expr, acc))
      in
      let result_expr =
        match fold_result, fallback with
        | Continue ((), conds), Some fallback ->
          (* Didn't hit an unconditional pattern; fallback to later match arms. *)
          Cond_assign { vars; conds; body; if_none_matched = Otherwise (fallback ()) }
        | Continue ((), conds), None ->
          (* Didn't hit an unconditional pattern, but this is the last match arm. Due to
             separate checks in [Simple_pattern.Coverage] we know the pattern is
             exhaustive, so we can elide the last condition. *)
          let conds, ((_ : cond), last_bindings) = Nonempty.split_last conds in
          add_last_unconditional_bindings ~conds ~last_bindings
        | Stop (last_bindings, conds), _ ->
          (* Found an unconditional pattern. *)
          (* TODO: Warn if [fallback] is [Some], since we're ignoring following match arms. *)
          add_last_unconditional_bindings ~conds ~last_bindings
      in
      match wrapping_binding with
      | None -> result_expr
      | Some (name, expr) -> Let (name, expr, result_expr)
    in
    of_typed_expr ~just_bound:outer_just_bound ~ctx:outer_ctx outer_expr outer_type
  ;;
end

module Stmt = struct
  (* TODO: Consider passing through information about whether the values/functions are
     exposed to other files so we can decide on proper LLVM linkage for them. *)
  type t =
    | Value_def of Mir_name.t * Expr.t
    | Fun_def of Expr.Fun_def.t
    | Fun_decl of Fun_decl.t
    | Extern_decl of Extern_decl.t
  [@@deriving sexp_of, variants]
end

type t = Stmt.t list [@@deriving sexp_of]

let of_typed_module =
  let handle_let_bindings
    ~ctx
    ~names
    ~stmts
    ~rec_
    ~fun_decls
    (bindings :
      (Typed.Pattern.t Node.t * Fixity.t option * Typed.Expr.generalized Node.t)
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
        let arity = arity_of_type ~names typ in
        if arity > 0
        then (
          (* For function types, we need to ensure the final definition is also a function.
             Create one which just forwards the call to the expression. Importantly, the
             name used for the function must be the original name, which should be a
             proper name from the source (its id should be 0). This lets code in other
             files link with it properly. *)
          let name' = Context.copy_name ctx name in
          let args =
            Nonempty.init arity ~f:(fun i ->
              snd (Context.add_value_name ctx (Constant_names.synthetic_arg i)))
          in
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
            Pattern.Names.fold
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
  in
  let generate_variant_constructor_values ~ctx ~stmts decl =
    match Context.find_cnstr_info_from_decl ctx decl ~follow_aliases:false with
    | None -> stmts
    | Some cnstr_info ->
      Cnstr_info.fold cnstr_info ~init:stmts ~f:(fun stmts cnstr tag args ->
        match cnstr with
        | Tuple -> stmts
        | Named cnstr_name ->
          let name =
            Context.find_value_name_assert_local ctx (Value_name.of_cnstr_name cnstr_name)
          in
          let stmt : Stmt.t =
            let arg_count = List.length args in
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
  in
  let rec loop ~ctx ~names ~stmts ~fun_decls (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, stmts) ~f:(fun (ctx, stmts) def ->
      Node.with_value def ~f:(function
        | Let { rec_; bindings } ->
          handle_let_bindings ~ctx ~names ~stmts ~rec_ ~fun_decls bindings
        | Module (module_name, _sigs, defs) ->
          Context.with_module ctx module_name ~f:(fun ctx ->
            loop ~ctx ~names ~stmts ~fun_decls defs)
        | Trait _ | Impl _ -> failwith "TODO: MIR traits/impls"
        | Common_def (Type_decl ((_ : Type_name.t), ((_, decl) : _ Type_decl.t))) ->
          ctx, generate_variant_constructor_values ~ctx ~stmts decl
        | Common_def (Extern (value_name, (_ : Fixity.t option), type_, extern_name)) ->
          let name = Context.find_value_name_assert_external ctx value_name in
          ( ctx
          , Extern_decl { name; extern_name; arity = arity_of_type ~names (fst type_) }
            :: stmts )
        | Common_def (Effect _ | Import _) -> ctx, stmts))
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    let names = Name_bindings.into_module names module_name ~place:`Def in
    let ctx = Context.create ~names ~name_table:(Mir_name.Name_table.create ()) in
    let fun_decls = Mir_name.Hash_set.create () in
    Compilation_error.try_with' (fun () ->
      let (_ : Context.t), stmts = loop ~ctx ~names ~stmts:[] ~fun_decls defs in
      List.rev stmts)
;;
