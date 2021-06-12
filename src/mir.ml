open Import
open Names

exception Mir_error of Sexp.t

let mir_error msg = raise (Mir_error msg)

module Unique_name : sig
  include General_name

  val of_ustring : Ustring.t -> t
end = struct
  include Ustring
  module Id = Unique_id.Int ()

  let slash = Ustring.of_string_exn "/"

  let of_ustring ustr =
    Ustring.concat [ ustr; slash; Ustring.of_string_exn (Id.to_string (Id.create ())) ]
  ;;
end

module Value_kind = struct
  type immediate =
    [ `Int64
    | `Float64
    | `Char (* Unicode scalar value: 4 bytes *)
    ]
  [@@deriving sexp_of]

  type pointer = [ `Block ] [@@deriving sexp_of]

  type t =
    [ immediate
    | pointer
    ]
  [@@deriving sexp_of]

  (*let of_primitive_type (path, type_name) =
    (* Note that [Bool] is not abstract and so doesn't need to be given here
       We should probably not need to give [String] either *)
    match path with
    | [] ->
      if Type_name.(type_name = Core.Int.name)
      then Some `Int64
      else if Type_name.(type_name = Core.Float.name)
      then Some `Float64
      else if Type_name.(type_name = Core.Char.name)
      then Some `Char
      else if Type_name.(type_name = Core.String.name)
      then Some `Block
      else None
    | _ :: _ -> None
  ;;

  let rec of_type_scheme ~names : Type.Scheme.t -> t = function
    | Var _ -> (* TODO: monomorphization *) `Block
    | Type_app (type_name, _args) ->
      option_or_default (of_primitive_type type_name) ~f:(fun () ->
        match snd (Name_bindings.find_type_decl ~defs_only:true names type_name) with
        | Alias scheme -> of_type_scheme ~names scheme
        | Variants _ | Record _ -> `Block
        | Abstract ->
          (* TODO: should be able to break the abstraction boundary to see the
             implementation *)
          raise_s
            [%message
              "TODO: of_type_scheme: Abstract" (type_name : Type_name.Qualified.t)])
    | Tuple _ -> `Block
    | Function (arg, result) ->
      (* TODO: what to do with functions? [Function_def] as a statement? *)
      let rec loop args result =
        match (result : Type.Scheme.t) with
        | Function (arg, result) ->
          loop (Nonempty.cons (of_type_scheme ~names arg) args) result
        | _ -> Closure (Nonempty.rev args, of_type_scheme ~names result)
      in
      loop [ of_type_scheme ~names arg ] result
  ;;*)
end

module Cnstr_tag : sig
  (** Constructor tags are represented as follows:
      - For constant constructors (i.e. constructors with no arguments), the tag is given
        inline as a 64-bit integer where the least significant bit is always set to 1.
        This is identical to the OCaml representation.
      - For non-constant constructors (i.e. those with arguments), the tag is given in a
        block header as the first 16 bits. In that case, as with any block, the pointer to
        the block will have its least signficiant bit set to 0. *)

  type t [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end = struct
  include Int

  let of_int t = t
  let to_int t = t
end

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.t -> t * Unique_name.t
  val find_value_name : t -> Value_name.Qualified.t -> Unique_name.t

  (* TODO: consider removing *)
  val add_empty : t -> t * Unique_name.t
  val find_empty : t -> Unique_name.t
  val cnstr_tag : t -> Type.Scheme.t -> Cnstr_name.t -> Cnstr_tag.t
  val cnstr_arg_type : t -> Type.Scheme.t -> Cnstr_name.t -> int -> Type.Scheme.t
end = struct
  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t
    }
  [@@deriving sexp_of]

  let empty_name = [], Value_name.empty

  let add t name =
    let name = Value_name.Qualified.to_ustring name in
    let name' = Unique_name.of_ustring name in
    { t with names = Map.set t.names ~key:name ~data:name' }, name'
  ;;

  let add_value_name t name =
    let path = Name_bindings.(current_path t.name_bindings |> Path.to_module_path) in
    add t (path, name)
  ;;

  let add_empty t = add t empty_name

  let find { names; _ } name =
    match Map.find names (Value_name.Qualified.to_ustring name) with
    | Some name -> name
    | None ->
      compiler_bug
        [%message
          "Name missing from context"
            (name : Value_name.Qualified.t)
            (names : Unique_name.t Ustring.Map.t)]
  ;;

  let find_value_name t name =
    let name =
      try Name_bindings.absolutify_value_name t.name_bindings name with
      | Name_bindings.Name_error _ ->
        Name_bindings.(current_path t.name_bindings |> Path.to_module_path), snd name
    in
    find t name
  ;;

  let find_empty t = find t empty_name

  let of_name_bindings name_bindings =
    let t = { names = Ustring.Map.empty; name_bindings } in
    Name_bindings.fold_local_names name_bindings ~init:t ~f:(fun t name _entry ->
      fst (add t name))
  ;;

  let rec lookup_cnstr ({ name_bindings; _ } as t) typ cnstr_name =
    let lookup_failed typ cnstr_name =
      compiler_bug
        [%message
          "Constructor lookup failed" (typ : Type.Scheme.t) (cnstr_name : Cnstr_name.t)]
    in
    match (typ : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      (match snd (Name_bindings.find_type_decl name_bindings type_name) with
      | Alias scheme -> lookup_cnstr t scheme cnstr_name
      | Variants variants ->
        List.fold_until
          variants
          ~init:(0, 0)
          ~f:(fun (constant_i, non_constant_i) (cnstr_name', args) ->
            if Cnstr_name.(cnstr_name = cnstr_name')
            then Stop ((if List.is_empty args then constant_i else non_constant_i), args)
            else Continue (constant_i + 1, non_constant_i + 1))
          ~finish:(fun _ -> lookup_failed typ cnstr_name)
      | Abstract | Record _ -> lookup_failed typ cnstr_name)
    | Var _ | Function _ | Tuple _ -> lookup_failed typ cnstr_name
  ;;

  let cnstr_tag t typ cnstr_name =
    let index, _ = lookup_cnstr t typ cnstr_name in
    Cnstr_tag.of_int index
  ;;

  let cnstr_arg_type t typ cnstr_name arg_index =
    let _, args = lookup_cnstr t typ cnstr_name in
    List.nth_exn args arg_index
  ;;
end

module Constant_names = struct
  let underscore = Value_name.of_string_unchecked "_"
  let match_ = Value_name.of_string_unchecked "match"
  let lambda_arg = Value_name.of_string_unchecked "lambda_arg"
end

module Expr = struct
  (* TODO: can put tags in the pointer to the block e.g. constructor tag,
     see https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/pointer-tagging
     GHC uses 3 bits (on 64-bit architectures) to store up to 7 constructors (0-6). 
     The highest tag value (7) indicates that the constructor tag must be looked up from
     the info table. Not sure if we want to have info tables, so in that case maybe it
     should be another field on the object. We could also do what OCaml does and take up
     some bits in the block header. *)
  (* For now, lets just store tags in the block itself as another field
     Later, we can store them in the pointer or block header *)
  (* Block representation: header + fields (fields must all be 64 bits (1 word) in length)
     Header describes which fields are pointers for the GC: i32, i32 for pointers, non-pointers
     - TODO: where should constructor tags go? Get their own field?
     - This is also doesn't quite work for things like strings
     - Haskell does this, but also has a pointer to a table with info about layout
     See:
     - https://dev.realworldocaml.org/runtime-memory-layout.html
     - https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects *)
  (* TODO: support for strings (literal is a weird place probably) as well as arrays *)
  (* TODO: are these types actually giving us the proper invariants? *)
  type t =
    | Primitive of Literal.t
    | Name of Unique_name.t
    (* TODO: recursive lets? Mutual recursion? *)
    | Let of Unique_name.t * t * t
    (* TODO: closure env - pass a pointer to a struct with all the free variables *)
    | Closure of Unique_name.t Nonempty.t * func
    | Fun_call of func * t Nonempty.t
    | Make_block of t list
    | Get_block_field of int * t
    | If of cond * t * t
    (* TODO: enforce that all switch cases have the same type + support switch on blocks
       - actually I'm not sure if this makes sense - variants will have different
         Value_kinds e.g. Int64 vs Block *)
    (*| Switch of
        { expr : t
        ; cases : (Literal.t Nonempty.t * t) Nonempty.t
        ; default : t option
        }*)
    | Catch of t * int
    | Exit of int

  and func =
    { (*args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t*)
      (* TODO: monomorphize functions/types per [Value_kind.t]. For now we can just box
       everything. *)
      (* FIXME: there's like no way I'll be able to compile functions with just this info *)
      arg_num : int
    ; body : t
    }

  and block =
    | Pointers_first of
        { pointers : t list
        ; immediates : t list
        }

  and cond =
    | Equals of t * Literal.t
    | Constant_tag_equals of t * Cnstr_tag.t
    | Non_constant_tag_equals of t * Cnstr_tag.t
    (*| Or of cond * cond*)
    | And of cond * cond
  [@@deriving sexp_of]

  let rec fold_let_pattern ~ctx ~init:acc ~add_let pat mir_expr =
    (* TODO: warn/error if bindings are not exhaustive *)
    match (pat : Typed.Pattern.t) with
    | Catch_all None ->
      (* TODO: warn about unused expressions. NOTE: we can only elide the bound expression
         as we are currently assuming purity. Later we should check for effects. *)
      ctx, acc
    | Catch_all (Some name) ->
      let ctx, name = Context.add_value_name ctx name in
      ctx, add_let acc name mir_expr
    | As (pattern, name) ->
      let ctx, name = Context.add_value_name ctx name in
      let acc = add_let acc name mir_expr in
      fold_let_pattern ~ctx ~init:acc ~add_let pattern (Name name)
    | Cnstr_appl (_, args) | Tuple args ->
      (* TODO: Need to assert that the constructor actually matched and throw an
         exception otherwise *)
      let ctx, name = Context.add_empty ctx in
      let acc = add_let acc name mir_expr in
      List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
        fold_let_pattern ~ctx ~init:acc ~add_let arg (Get_block_field (i, Name name)))
    | Constant _ ->
      (* Assert equality between the result and the constant *)
      (* FIXME: integrate this with cond generation and assert exhaustiveness, then give
         a compile error if it fails *)
      failwith "TODO: constant pattern bindings"
    | Union (_, _) ->
      (* Assert both cases can be bound to *)
      failwith "TODO: union pattern bindings"
    | Record _ ->
      (* Assert the result can be destructed, then destruct it, binding to the names *)
      failwith "TODO: record pattern bindings"
    | Type_annotation _ -> .
  ;;

  module Simple_pattern = struct
    (*module Category = struct
      type t =
        [ `Cnstr_appl
        | `Constant
        | `Tuple
        | `Record
        ]
      [@@deriving sexp]

      let raise_incompatible category1 category2 =
        compiler_bug
          [%message
            "Conflicting pattern categories in match arms" (category1 : t) (category2 : t)]
      ;;

      let rec of_pattern : Typed.Pattern.t -> t option = function
        | Cnstr_appl _ -> Some `Cnstr_appl
        | Constant _ -> Some `Constant
        | Catch_all _ -> None
        | As (pat, _) -> of_pattern pat
        | Tuple _ -> Some `Tuple
        | Record _ -> Some `Record
        | Union (pat, pat') ->
          (match of_pattern pat, of_pattern pat' with
          | Some category, Some category' ->
            if Poly.( = ) category category'
            then Some category
            else raise_incompatible category category'
          | None, (Some _ as category) | (Some _ as category), None -> category
          | None, None -> None)
        | Type_annotation _ -> .
      ;;
    end*)

    type t =
      | Catch_all of Value_name.t option
      | Constants of Literal.t Nonempty.t
      | Cnstr_appls of (Cnstr_name.Qualified.t * t list) Nonempty.t
      | Tuples of t list Nonempty.t
      | Records of t option Value_name.Map.t Nonempty.t
    [@@deriving sexp]

    let rec of_pattern : Typed.Pattern.t -> t = function
      | Catch_all name -> Catch_all name
      | Constant literal -> Constants [ literal ]
      | Cnstr_appl (cnstr_name, args) ->
        Cnstr_appls [ cnstr_name, List.map args ~f:of_pattern ]
      | Tuple fields -> Tuples [ List.map fields ~f:of_pattern ]
      | Record fields -> Records [ convert_record_fields fields ]
      | As (pattern, _) -> of_pattern pattern
      | Union (pat1, pat2) ->
        let combine_union pat1 pat2 =
          let ( @ ) = Nonempty.( @ ) in
          match pat1, pat2 with
          | Catch_all name, _ | _, Catch_all name -> Catch_all name
          | Constants constants1, Constants constants2 ->
            Constants (constants1 @ constants2)
          | Cnstr_appls appls1, Cnstr_appls appls2 -> Cnstr_appls (appls1 @ appls2)
          | Tuples tuples1, Tuples tuples2 ->
            (* FIXME: as in the `(5, x) | (x, t)` example, which union is picked can
               change how the names are bound - code for the bindings needs to be inserted
               e.g. x = Get_block_field _ 0 (or 1) - this is different for each branch *)
            Tuples (tuples1 @ tuples2)
          | Records records1, Records records2 -> Records (records1 @ records2)
          | _ ->
            compiler_bug [%message "Incompatible patterns in union" (pat1 : t) (pat2 : t)]
        in
        combine_union (of_pattern pat1) (of_pattern pat2)
      | Type_annotation _ -> .

    and convert_record_fields fields =
      Value_name.Map.of_alist_exn fields |> Map.map ~f:(Option.map ~f:of_pattern)
    ;;

    (* TODO: probably refactor this to be less heavy. Trying to enforce constraints in the 
       types is very low-value because they will only be broken with a bug in the type
       checker, and we just raise when they fail. *)
    (*module Match_arms = struct
      type 'expr patterns =
        | Constants of (Literal.t Nonempty.t * 'expr) Nonempty.t
        | Cnstr_appls of ((Cnstr_name.Qualified.t * t list) Nonempty.t * 'expr) Nonempty.t
        | Tuples of (t list Nonempty.t * 'expr) Nonempty.t
        | Records of (t option Value_name.Map.t Nonempty.t * 'expr) Nonempty.t
      [@@deriving sexp]

      type simple_pattern = t [@@deriving sexp]

      type 'expr t =
        | Patterns of 'expr patterns * 'expr option
        | Trivial of 'expr

      let single_arm (pat, expr) =
        match (pat : simple_pattern) with
        | Constants constants -> `Patterns (Constants [ constants, expr ])
        | Cnstr_appls appls -> `Patterns (Cnstr_appls [ appls, expr ])
        | Tuples tuples -> `Patterns (Tuples [ tuples, expr ])
        | Records records -> `Patterns (Records [ records, expr ])
        | Catch_all -> `Catch_all expr
      ;;

      let rev : 'a patterns -> 'a patterns = function
        | Constants xs -> Constants (Nonempty.rev xs)
        | Cnstr_appls xs -> Cnstr_appls (Nonempty.rev xs)
        | Tuples xs -> Tuples (Nonempty.rev xs)
        | Records xs -> Records (Nonempty.rev xs)
      ;;

      let create (arm :: arms : (simple_pattern * _) Nonempty.t) =
        match single_arm arm with
        | `Catch_all expr -> Trivial expr
        | `Patterns patterns ->
          let rec loop patterns = function
            | [] -> Patterns (rev patterns, None)
            | (pat, expr) :: arms ->
              (match patterns, (pat : simple_pattern) with
              | Constants constants, Constants new_constants ->
                loop (Constants (Nonempty.cons (new_constants, expr) constants)) arms
              | Cnstr_appls appls, Cnstr_appls new_appls ->
                loop (Cnstr_appls (Nonempty.cons (new_appls, expr) appls)) arms
              | Tuples tuples, Tuples new_tuples ->
                loop (Tuples (Nonempty.cons (new_tuples, expr) tuples)) arms
              | Records records, Records new_records ->
                loop (Records (Nonempty.cons (new_records, expr) records)) arms
              | _, Catch_all -> Patterns (rev patterns, Some expr)
              | _ -> compiler_bug [%message "Mismatching pattern" (pat : simple_pattern)])
          in
          loop patterns arms
      ;;*)
  end

  (*let categorize_arms ((first_pattern, _) :: _ as arms : _ Nonempty.t) =
      let rec loop category default = function
        | [] -> category, default
        | (pat, expr) :: rest ->
          (match category, Category.of_pattern pat with
          | None, Some _ -> loop category default rest
          | None, None | Some _, None ->
            let default =
              (* TODO: Warn about unused match arms: catch-alls in this case *)
              match default with
              | Some _ -> default
              | None -> Some expr
            in
            loop category default rest
          | Some category1, Some category2 ->
            if Poly.( = ) category1 category2
            then loop category default rest
            else Category.raise_incompatible category1 category2)
      in
      loop (Category.of_pattern first_pattern) None (Nonempty.to_list arms)
    ;;
  end*)

  (* TODO: switch statement optimization
         See:
         - https://github.com/ocaml/ocaml/blob/trunk/lambda/matching.ml
         - https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching  *)
  (*let rec switch_case ~ctx
        : Pattern.t -> Context.t * Value_kind.t Literal.t list
        = function
        | Cnstr_appl ((_, cnstr_name), []) ->
          let cnstr_index = Context.cnstr_index ctx expr_type cnstr_name in
          ctx, [ Int cnstr_index ]
        | Catch_all None -> ctx, []
        | Catch_all (Some name) -> fst (Context.add_value_name ctx name), []
        | As (pattern, name) ->
          switch_case ~ctx:(fst (Context.add_value_name ctx name)) pattern
        | (Cnstr_appl _ | Constant _ | Tuple _ | Record _ | Union (_, _)) as pat ->
          raise_s
            [%message
              "TODO: unimplemented match to switch pattern" (pat : Pattern.t)]
      in
      (* TODO: Warn about unused cases *)
      let cases, default =
        Nonempty.fold_right arms ~init:([], None) ~f:(fun (pat, expr) (cases, default) ->
          if Option.is_some default
          then cases, default
          else (
            (* FIXME: need to bind variable names from the switch case when making this *)
            let ctx, switch_cases = switch_case ~ctx pat in
            let expr = of_typed_expr ~ctx (expr, match_type) in
            match switch_cases with
            | [] -> cases, Some expr
            | case :: rest -> (Nonempty.(case :: rest), expr) :: cases, default))
      in*)
  (*let rec loop_switch ~ctx expr cases default = function
      | [] -> Switch { expr; cases; default }
      | arm :: arms -> ()
    in
    match arm with
    | Cnstr_appl _ | Constant _ -> loop_switch ~ctx expr [] None (arm :: arms)
    | Catch_all (Some name) -> ()
    | As (pattern, name) ->
      switch_case ~ctx:(fst (Context.add_value_name ctx name)) pattern
    | (Tuple _ | Record _ | Union (_, _)) as pat -> ()*)

  let split_up_pattern_unions pattern =
    let open Nonempty.Let_syntax in
    let rec loop pattern =
      match (pattern : Typed.Pattern.t) with
      | Constant _ | Catch_all _ -> return pattern
      | As (pattern, name) -> loop pattern >>| Fn.flip Pattern.as_ name
      | Cnstr_appl (cnstr_name, args) ->
        map_multiple pattern args ~f:(Pattern.cnstr_appl cnstr_name)
      | Tuple fields -> map_multiple pattern fields ~f:Pattern.tuple
      | Record _fields ->
        (* TODO: record pattern option should probably just be replaced with Name blah *)
        (*(match Nonempty.of_list fields with
        | None -> return pattern
        | Some fields ->
          let field_unions = fields >>| loop in
          loop_multiple arg_unions >>| Pattern.cnstr_appl cnstr_name)*)
        failwith "TODO: MIR: splitting up record patterns"
      | Union (pat1, pat2) -> Nonempty.(loop pat1 @ loop pat2)
      | Type_annotation _ -> .
    and map_multiple pattern fields ~f =
      let rec loop_multiple Nonempty.(field_union :: field_unions) =
        let%bind field = field_union in
        match Nonempty.of_list field_unions with
        | None -> return [ field ]
        | Some field_unions ->
          let%bind rest = loop_multiple field_unions in
          return (field :: rest)
      in
      match Nonempty.of_list fields with
      | None -> return pattern
      | Some fields -> loop_multiple (fields >>| loop) >>| f
    in
    loop pattern
  ;;

  let rec of_typed_expr ~ctx : Typed.Expr.generalized -> t = function
    | Literal lit, _ -> Primitive lit
    | Name name, _ -> Name (Context.find_value_name ctx name)
    (* FIXME: we've discarded all but the result type - is that bad? How can we easily
       get the argument types? - can add them to the typed expr in the AST if needed
       What is happening here? This seems wrong *)
    | Fun_call (f, arg, arg_type), result_type ->
      let rec loop (f, f_type) arg args arg_num =
        match (f : _ Typed.Expr.t) with
        | Fun_call (f', arg', arg_type') ->
          let f' = f', Type.Expr.Function (arg_type', result_type) in
          loop f' (arg', arg_type') (of_typed_expr ~ctx arg :: args) (arg_num + 1)
        | Name _ | Lambda _ | Match _ | Let _ ->
          let func = { arg_num; body = of_typed_expr ~ctx (f, f_type) } in
          Fun_call (func, of_typed_expr ~ctx arg :: args)
        | Literal _ | Tuple _ | Record_literal _ | Record_update _ | Record_field_access _
          ->
          (* TODO: should be able to find where this is from a Node.t or something *)
          mir_error
            [%message
              "This is not a function; it cannot be applied"
                (f : Type.Scheme.t Typed.Expr.t)]
      in
      loop (f, Function (arg_type, result_type)) (arg, arg_type) [] 1
    | Lambda (arg, body), Function (_, body_type) ->
      (* TODO: Can I tweak the fold_let_pattern interface to be less awkward? It seems to
         only really work well for the original use case with statements - maybe a
         specialized implementation for exprs would be in order
         Note also that mir_expr is used only to pass into add_let *)
      (* TODO: Should assert that the pattern is exhaustive in this and let bindings
         (exhaustive vs unconditional?) *)
      let rec bind_arg ~ctx arg_names arg_num ~arg ~body ~body_type =
        let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
        let add_let acc name mir_expr = (name, mir_expr) :: acc in
        let ctx, bindings = fold_let_pattern ~ctx arg (Name arg_name) ~init:[] ~add_let in
        let arg_names, arg_num, body =
          loop ~ctx Nonempty.(arg_name :: arg_names) (arg_num + 1) (body, body_type)
        in
        let body =
          List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
            Let (name, mir_expr, body))
        in
        arg_names, arg_num, body
      and loop ~ctx arg_names arg_num : Typed.Expr.generalized -> _ = function
        | Lambda (arg, body), Function (_, body_type) ->
          bind_arg ~ctx (Nonempty.to_list arg_names) arg_num ~arg ~body ~body_type
        | body_and_type ->
          Nonempty.rev arg_names, arg_num, of_typed_expr ~ctx body_and_type
      in
      let arg_names, arg_num, body = bind_arg ~ctx [] 0 ~arg ~body ~body_type in
      Closure (arg_names, { arg_num; body })
    | Match (expr, expr_type, arms), match_type ->
      make_match ~ctx (expr, expr_type) match_type arms
    | Let { rec_; bindings; body }, body_type ->
      (* TODO: let statements in expressions should be able to be made into global statements
         (e.g. to define static functions/values) - not all lets should be global though e.g.
         for simple expressions like `let y = x + x; (y, y)` *)
      if rec_
      then failwith "TODO: let rec in MIR expr"
      else (
        let ctx, acc =
          Nonempty.fold bindings ~init:(ctx, []) ~f:(fun (ctx, acc) ((pat, typ), expr) ->
            let mir_expr = of_typed_expr ~ctx (expr, typ) in
            let add_let acc name mir_expr = (name, mir_expr) :: acc in
            fold_let_pattern ~ctx pat mir_expr ~init:acc ~add_let)
        in
        let body = of_typed_expr ~ctx (body, body_type) in
        List.fold acc ~init:body ~f:(fun body (name, mir_expr) ->
          Let (name, mir_expr, body)))
    | Tuple fields, Tuple field_types ->
      let fields =
        List.map2_exn fields field_types ~f:(fun field typ ->
          of_typed_expr ~ctx (field, typ))
      in
      (* FIXME: we can't work out the Value_kind from the type, as it can vary freely 
         at runtime due to variants. This means that the position of fields in a record
         can change based on their value, which is kind of nonsense. We may need to
         abandon pointers/non-pointer separation and just use the bitfield.*)
      Make_block fields
    | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
      failwith "TODO: records in MIR exprs"
    | ( Lambda _, (Var _ | Type_app _ | Tuple _)
      | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
      compiler_bug [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]

  (* FIXME: remove all this stuff, including Simple_pattern, pattern categories,
     and these simplications *)
  and simplify_match_arms ~ctx match_type (arm :: arms : _ Nonempty.t) =
    let make_arm (pattern, arm_expr) =
      let arm_expr =
        of_typed_expr
          (arm_expr, match_type)
          ~ctx:
            (Pattern.Names.fold pattern ~init:ctx ~f:(fun ctx name ->
               fst (Context.add_value_name ctx name)))
      in
      let pattern = Simple_pattern.of_pattern pattern in
      pattern, arm_expr
    in
    List.fold_until
      arms
      ~init:([ make_arm arm ] : _ Nonempty.t)
      ~f:(fun cases ((pattern, _) as arm) ->
        let cases = Nonempty.cons (make_arm arm) cases in
        match pattern with
        | Catch_all _ -> Stop cases
        | _ -> Continue cases)
      ~finish:Fn.id
    |> Nonempty.rev

  and make_match
    ~(ctx : Context.t)
    ((input_expr, input_type) : Typed.Expr.generalized)
    (output_type : Type.Scheme.t)
    (arms : (Typed.Pattern.t * Type.Scheme.t Typed.Expr.t) Nonempty.t)
    : t
    =
    (* TODO: switch statement optimization
         See:
         - https://github.com/ocaml/ocaml/blob/trunk/lambda/matching.ml
         - https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching *)
    (*match Simple_pattern.categorize_arms arms with
    | Some category, default ->
      let invalid pattern =
        compiler_bug
          [%message
            "Invalid pattern for category"
              (pattern : Typed.Pattern.t)
              (category : Simple_pattern.Category.t)]
      in
      (*let handle_cnstrs ~ctx ~expr ~default arms =
          (* TODO: Turn into a switch on variant tag
             - need to split up constant vs argumented constructors
             - constants can be in the switch - on specific int values
             - NOTE: might be a good idea to forget switch and all that stuff and just
               make the simplest thing possible, just using if *)
          let constant_cnstrs, non_constant_cnstrs =
            List.fold
              arms
              ~init:(Cnstr_tag.Map.empty, Cnstr_tag.Map.empty)
              ~f:(fun (constant_cnstrs, non_constant_cnstrs) (pattern, scheme, expr) ->
              match pattern with
              | Cnstr_appl ((_, cnstr_name), args) ->
                (* FIXME: have to match on args *)
                let tag = Context.cnstr_tag ctx scheme cnstr_name in
                if List.is_empty args
                then
                  (* FIXME: how does ordering work for multi maps? - order should be
                     reversed, which is wrong*)
                  ( Map.add_multi constant_cnstrs ~key:tag ~data:(pattern, expr)
                  , non_constant_cnstrs )
                else
                  ( constant_cnstrs
                  , Map.add_multi non_constant_cnstrs ~key:tag ~data:(pattern, expr) )
              | _ -> invalid pattern)
          in
          (*let to_switch_cases cnstrs =
            Map.to_alist cnstrs
            |> List.map ~f:(fun (tag, ))
          in*)
          Switch
            { expr
            ; cases = to_switch_cases constant_cnstrs
            ; default =
                Some
                  (Switch
                     { expr = Get_block_tag expr
                     ; cases = to_switch_cases non_constant_cnstrs
                     ; default
                     })
            }
        in*)
      let mir_expr = of_typed_expr ~ctx (expr, expr_type) in
      let convert_arm_expr ~ctx pattern arm_expr =
        of_typed_expr
          arm_expr
          ~ctx:
            (Pattern.Names.fold pattern ~init:ctx ~f:(fun ctx name ->
               fst (Context.add_value_name ctx name)))
      in
      (* TODO: is this some nice way to avoid code duplication between these branches? *)
      (match category with
      | `Constant ->
        (*let cases, default =
          Nonempty.fold_until
            arms
            ~init:[]
            ~f:(fun cases (pattern, arm_expr) ->
              let arm_expr = convert_arm_expr ~ctx pattern arm_expr in
              (* FIXME: doesn't work on unions *)
              match pattern with
              | Constant lit -> Continue ((Primitive.of_literal lit, arm_expr) :: cases)
              | Catch_all _ -> Stop (List.rev cases, Some arm_expr)
              | _ -> invalid pattern)
            ~finish:(fun cases -> List.rev cases, None)
        in
        Switch { expr = mir_expr; cases = Nonempty.of_list_exn cases; default }*)
        ()
      | `Tuple -> ()
      | `Cnstr_appl | `Record -> failwith "TODO: Match cases MIR")
    | None, Some default -> of_typed_expr ~ctx (default, match_type)
    | None, None ->
      compiler_bug
        [%message "Match classification failed" (expr : Type.Scheme.t Typed.Expr.t)]*)
    (* TODO: Simplify match arms, then iterate over for each *)
    (*let invalid pattern =
      compiler_bug [%message "Invalid pattern" (pattern : Simple_pattern.t)]
    in*)
    (* TODO: simplifying first breaks some things because we can't recover the names 
       added to lost contexts *)
    (*let ((pattern, expr) :: arms) = simplify_match_arms ~ctx match_type arms in
    match pattern with
    | Catch_all name ->
      (match name with
      | Some name -> 
      | None -> expr)
    | Constants constants ->
      let convert = Nonempty.map ~f:Primitive.of_literal in
      List.fold_until
        arms
        ~init:([ convert constants, expr ] : _ Nonempty.t)
        ~f:(fun cases (pattern, expr) ->
          match pattern with
          | Constants _ -> Continue (Nonempty.cons (convert constants, expr) cases)
          | Catch_all ->
            Stop
              (Switch
                 { expr = original_expr; cases = Nonempty.rev cases; default = Some expr })
          | _ -> invalid pattern)
        ~finish:(fun cases ->
          Switch { expr = original_expr; cases = Nonempty.rev cases; default = None })
    | Tuples tuples ->
      List.fold
        arms
        ~init:(Nonempty.map tuples ~f:Nonempty.singleton)
        ~f:(fun cases tuples ->
        Nonempty.map2 tuples cases (fun tuple case -> tuple :: case))*)
    (* FIXME: before I do this I should really split up unions - they are super annoying
       - revise Simple_pattern to return multiple patterns on seeing unions (?) *)
    (* TODO: I should make these all at once
       - split on union and continue with the arm_expr *)
    let rec make_condition ~ctx ~input_expr ~input_type pattern =
      match (pattern : Typed.Pattern.t) with
      | Catch_all _ | As _ -> None
      | Constant lit -> Some (Equals (input_expr, lit))
      | Cnstr_appl ((_, cnstr_name), args) ->
        let tag = Context.cnstr_tag ctx input_type cnstr_name in
        let tag_cond =
          if List.is_empty args
          then Constant_tag_equals (input_expr, tag)
          else Non_constant_tag_equals (input_expr, tag)
        in
        let conds =
          List.filter_mapi args ~f:(fun i arg ->
            let arg_expr = Get_block_field (i, input_expr) in
            let arg_type = Context.cnstr_arg_type ctx input_type cnstr_name i in
            make_condition ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
        in
        Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond')))
      | Tuple fields ->
        (match fields, input_type with
        | [], Tuple [] -> None
        | field :: fields, Tuple (field_type :: field_types) ->
          Nonempty.zip (field :: fields) (field_type :: field_types)
          |> Nonempty.to_list
          |> List.filter_mapi ~f:(fun i (field, field_type) ->
               let field_expr = Get_block_field (i, input_expr) in
               make_condition ~ctx ~input_expr:field_expr ~input_type:field_type field)
          |> List.reduce ~f:(fun cond cond' -> And (cond, cond'))
        | _ ->
          compiler_bug
            [%message "Invalid tuple type" (input_expr : t) (input_type : Type.Scheme.t)])
      | Record _fields ->
        (* TODO: need to canonicalize record fields first - change to a map, probably *)
        failwith "TODO: record type pattern conditions"
      | Union _ -> compiler_bug [%message "Union remaining in split up patterns"]
      | Type_annotation _ -> .
    in
    let rec handle_arms
      ~ctx
      ~input_expr
      ~input_type
      ~output_type
      ((pattern, output_expr) :: arms : _ Nonempty.t)
      =
      let output_expr =
        let add_let bindings name expr = (name, expr) :: bindings in
        let ctx, bindings = fold_let_pattern ~ctx pattern input_expr ~init:[] ~add_let in
        List.fold_right
          bindings
          ~init:(of_typed_expr ~ctx (output_expr, output_type))
          ~f:(fun (name, expr) body -> Let (name, expr, body))
      in
      match make_condition ~ctx ~input_expr ~input_type pattern with
      | None ->
        (* TODO: warn about unused patterns (the other arms) *)
        output_expr
      | Some cond ->
        (match Nonempty.of_list arms with
        | None -> output_expr
        | Some arms ->
          If
            (cond, output_expr, handle_arms ~ctx ~input_expr ~input_type ~output_type arms))
      (*and loop ~ctx input_expr input_type = function
      | (pat, arm_expr) :: rest ->
        (* FIXME: handle binding names in patterns - need to insert definitions
           e.g. aliases or getting out fields *)
        let add_let bindings name expr = (name, expr) :: bindings in
        let arm_ctx, bindings = fold_let_pattern ~ctx ~init:[] ~add_let pat input_expr in
        let arm_expr_with_bindings =
          List.fold_right
            bindings
            ~init:(of_typed_expr ~ctx:arm_ctx (arm_expr, output_type))
            ~f:(fun (name, binding_expr) arm_expr -> Let (name, binding_expr, arm_expr))
        in
        (match make_condition pat input_expr input_type with
        | None -> arm_expr_with_bindings
        | Some cond ->
          If (cond, arm_expr_with_bindings, loop ~ctx input_expr input_type rest))
      | [] -> compiler_bug [%message "Ran off end of match arms (inexhaustive patterns?)"]*)
    in
    let input_expr = of_typed_expr ~ctx (input_expr, input_type) in
    let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
    (* TODO: could we encode pattern splitting at the type level? Either that or not do it *)
    (* FIXME: remove duplication of exprs using catch/exit - not sure exactly how tho *)
    let arms =
      Nonempty.concat_map arms ~f:(fun (pat, expr) ->
        split_up_pattern_unions pat |> Nonempty.map ~f:(fun pat -> pat, expr))
    in
    Let
      ( match_expr_name
      , input_expr
      , handle_arms ~ctx ~input_expr:(Name match_expr_name) ~input_type ~output_type arms
      )
  ;;
end

module Stmt = struct
  (* TODO: closures require creating functions on the fly
     - I suppose we can model this by creating a struct/record with all the used
       parameters and passing that in to a global function *)
  type t = Value_def of Unique_name.t * Expr.t
  (* TODO: implement static function definitions *)
  (*| Fun_def of Unique_name.t * Function.t*)
  [@@deriving sexp_of]

  (* TODO: Functions can be computed at runtime e.g.
     `let f = if true then fun x -> x * 2 else fun x -> x * 3`
     so functions sometimes just have to be runtime-constructed values anyway *)
end

(* TODO: need to handle ordering? function definitions can have side effects *)
type t = Stmt.t list [@@deriving sexp_of]

let of_typed_module =
  let loop ~ctx (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, []) ~f:(fun (ctx, stmts) def ->
      match def.Node.node with
      | Let bindings ->
        List.fold
          bindings
          ~init:(ctx, stmts)
          ~f:(fun (ctx, stmts) { node = pattern, expr; _ } ->
          let mir_expr = Expr.of_typed_expr ~ctx expr in
          let add_let stmts name mir_expr = Stmt.Value_def (name, mir_expr) :: stmts in
          Expr.fold_let_pattern ~ctx pattern mir_expr ~init:stmts ~add_let)
      | Module (_, _, _) ->
        (* TODO: remember to update the name_bindings current_path *)
        failwith "TODO: MIR submodules"
      | Trait (_, _, _, _) | Impl (_, _, _, _) -> failwith "TODO: MIR traits/impls"
      | Common_def _ -> ctx, stmts)
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    let names = Name_bindings.into_module names module_name ~place:`Def in
    let _ctx, stmts = loop ~ctx:(Context.of_name_bindings names) defs in
    List.rev stmts
;;

(* Goals should be:
   - Remove unnecessary type information (definitions, etc.)
   - Flatten out function definitions/calls (?)
     - idk if OCaml lambda form does this
       - Actually it did seem to have lists of argument types for functions
     - What's the cost of currying?
     - Surely we don't want all our functions in LLVM to be single-argument?
   - Move all function definitions to toplevel, with unique names
     (Other values can stay inside functions (?))
     (Dynamically created functions may have to represented as closures e.g. like in OCaml
     partial application even if they don't bind any variables)
   *)
