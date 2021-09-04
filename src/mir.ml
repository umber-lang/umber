open Import
open Names

exception Mir_error of Sexp.t [@@deriving sexp]

let mir_error msg = raise (Mir_error msg)

module Unique_name : sig
  type t [@@deriving sexp_of]

  val of_ustring : Ustring.t -> t
  val of_extern_name : Extern_name.t -> t
  val map_id : t -> f:(int -> int) -> t
end = struct
  module Id = Unique_id.Int ()

  type t = Ustring.t * Id.t

  let of_ustring ustr = ustr, Id.create ()
  let of_extern_name extern_name = of_ustring (Extern_name.to_ustring extern_name)
  let map_id t ~f = Tuple2.map_snd t ~f:(Id.of_int_exn << f << Id.to_int_exn)

  (*let sexp_of_t (ustr, _) = Ustring.sexp_of_t ustr*)

  let sexp_of_t (ustr, id) =
    Sexp.Atom (String.concat [ Ustring.to_string ustr; "/"; Id.to_string id ])
  ;;

  (* TODO: cleanup *)

  (*let extern_prefix = of_string_exn "extern:"
  let of_extern_name extern_name =
    if Extern_name.is_prim_op extern_name
    then (* Prim_op names are guaranteed to be unique *)
      Extern_name.to_ustring extern_name
    else extern_prefix ^ of_ustring (Extern_name.to_ustring extern_name)
  ;;

  let sexp_of_t t = Sexp.Atom (String.take_while (to_string t) ~f:(Char.( <> ) '/'))*)
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

  let of_primitive_type (path, type_name) =
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
    | Function (_arg, _result) ->
      (* TODO: what to do with functions? [Function_def] as a statement? *)
      (*let rec loop args result =
        match (result : Type.Scheme.t) with
        | Function (arg, result) ->
          loop (Nonempty.cons (of_type_scheme ~names arg) args) result
        | _ -> Closure (Nonempty.rev args, of_type_scheme ~names result)
      in
      loop [ of_type_scheme ~names arg ] result*)
      `Block
  ;;
end

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

  module Tag : sig
    (** Constructor tags are represented as follows:
    - For constant constructors (i.e. constructors with no arguments), the tag is given
      inline as a 64-bit integer where the least significant bit is always set to 1.
      This is identical to the OCaml representation.
    - For non-constant constructors (i.e. those with arguments), the tag is given in a
      block header as the first 16 bits. In that case, as with any block, the pointer to
      the block will have its least signficiant bit set to 0. *)

    (* TODO: what about putting constructor tags in the pointer sometimes? On 64-bit
       platforms we should have 3 free bits. *)

    type t [@@deriving compare, equal, hash, sexp]

    include Comparable.S with type t := t

    val of_int : int -> t
    val to_int : t -> int
    val default : t
  end = struct
    include Int

    let of_int t = t
    let to_int t = t
    let default = 0
  end
end

module Constant_names = struct
  (* NOTE: none of these can be valid value names a user could enter. *)
  let empty = Value_name.empty
  let match_ = Value_name.of_string_unchecked "match"
  let lambda_arg = Value_name.of_string_unchecked "*lambda_arg"

  let mem =
    let constant_names = Value_name.Hash_set.of_list [ empty; match_; lambda_arg ] in
    Hash_set.mem constant_names
  ;;
end

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.t -> t * Unique_name.t
  val find_value_name : t -> Value_name.Qualified.t -> Unique_name.t
  val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
  val cnstr_tag : t -> Type.Scheme.t -> Cnstr.t -> Cnstr.Tag.t
  val cnstr_arg_type : t -> Type.Scheme.t -> Cnstr.t -> int -> Type.Scheme.t
  val cnstrs : t -> Type.Scheme.t -> Cnstr.Set.t
  val name_bindings : t -> Name_bindings.t
end = struct
  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t [@sexp_drop_if const true]
    }
  [@@deriving sexp_of]

  let name_bindings t = t.name_bindings

  let with_module t module_name ~f =
    let name_bindings =
      Name_bindings.into_module t.name_bindings module_name ~place:`Def
    in
    let t, x = f { t with name_bindings } in
    { t with name_bindings = Name_bindings.into_parent name_bindings }, x
  ;;

  let add t name =
    let name = Value_name.Qualified.to_ustring name in
    let name' = Unique_name.of_ustring name in
    { t with names = Map.set t.names ~key:name ~data:name' }, name'
  ;;

  let add_value_name t name =
    if Constant_names.mem name
    then add t ([], name)
    else (
      let path = Name_bindings.(current_path t.name_bindings |> Path.to_module_path) in
      add t (path, name))
  ;;

  let find { names; name_bindings } name =
    match Map.find names (Value_name.Qualified.to_ustring name) with
    | Some _ as name -> name
    | None ->
      (match Name_bindings.find_entry name_bindings name with
      | exception Name_bindings.Name_error _ -> None
      | entry ->
        Name_bindings.Name_entry.extern_name entry
        |> Option.map ~f:Unique_name.of_extern_name)
  ;;

  let find_value_name' t name =
    match name with
    | [], name when Constant_names.mem name -> find t ([], name)
    | _ ->
      let name =
        try Name_bindings.absolutify_value_name t.name_bindings name with
        | Name_bindings.Name_error _ ->
          Name_bindings.(current_path t.name_bindings |> Path.to_module_path), snd name
      in
      find t name
  ;;

  let find_value_name t name =
    option_or_default (find_value_name' t name) ~f:(fun () ->
      compiler_bug
        [%message
          "Name missing from context"
            (name : Value_name.Qualified.t)
            (t.names : Unique_name.t Ustring.Map.t)])
  ;;

  let of_name_bindings name_bindings =
    let t = { names = Ustring.Map.empty; name_bindings } in
    Name_bindings.fold_local_names name_bindings ~init:t ~f:(fun t name _entry ->
      fst (add t name))
  ;;

  let cnstr_lookup_failed ?cnstr_name typ =
    compiler_bug
      [%message
        "Constructor lookup failed"
          (typ : Type.Scheme.t)
          (cnstr_name : Cnstr_name.t option)]
  ;;

  let rec get_cnstr_info ({ name_bindings; _ } as t) typ =
    match (typ : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      (match snd (Name_bindings.find_type_decl name_bindings type_name) with
      | Alias scheme -> get_cnstr_info t scheme
      | Variants variants -> `Variants variants
      | Abstract | Record _ -> cnstr_lookup_failed typ)
    | Tuple args -> `Tuple args
    | Var _ | Function _ -> cnstr_lookup_failed typ
  ;;

  let lookup_cnstr t typ cnstr =
    match get_cnstr_info t typ, (cnstr : Cnstr.t) with
    | `Variants variants, Named cnstr_name ->
      `Variants
        (List.fold_until
           variants
           ~init:(0, 0)
           ~f:(fun (constant_i, non_constant_i) (cnstr_name', args) ->
             if Cnstr_name.(cnstr_name = cnstr_name')
             then Stop ((if List.is_empty args then constant_i else non_constant_i), args)
             else Continue (constant_i + 1, non_constant_i + 1))
           ~finish:(fun _ -> cnstr_lookup_failed typ ~cnstr_name))
    | `Tuple args, Tuple -> `Tuple args
    | _cnstr_info, cnstr ->
      compiler_bug [%message "Incompatible cnstr info" (cnstr : Cnstr.t)]
  ;;

  let cnstr_tag t typ cnstr =
    match lookup_cnstr t typ cnstr with
    | `Variants (index, _) -> Cnstr.Tag.of_int index
    | `Tuple _ -> Cnstr.Tag.of_int 0
  ;;

  let cnstr_arg_type t typ cnstr_name arg_index =
    let (`Variants (_, args) | `Tuple args) = lookup_cnstr t typ cnstr_name in
    List.nth_exn args arg_index
  ;;

  let cnstrs t typ =
    match get_cnstr_info t typ with
    | `Variants variants -> List.map variants ~f:(Cnstr.named << fst) |> Cnstr.Set.of_list
    | `Tuple _ -> Cnstr.Set.singleton Tuple
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

  module Coverage : sig
    type simple_pattern = t
    type t

    val of_pattern : simple_pattern -> t
    val of_patterns : simple_pattern Nonempty.t -> t
    val combine : t -> t -> t

    val missing_cases
      :  t
      -> ctx:Context.t
      -> input_type:Type.Scheme.t
      -> simple_pattern list
  end
end = struct
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr.t * t list
  [@@deriving sexp, variants]

  (* TODO: remove *)
  (*let tuple_cnstr_name arity = Cnstr_name.of_string_unchecked (sprintf "Tuple/%d" arity)

  let record_cnstr_name field_names =
    let buf = Buffer.create (5 * Set.length field_names) in
    Buffer.add_string buf "Record";
    Set.iter field_names ~f:(fun name ->
      Buffer.add_char buf '/';
      Ustring.add_to_buffer buf (Value_name.to_ustring name));
    Cnstr_name.of_string_unchecked (Buffer.contents buf)
  ;;*)

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
      mir_error [%message msg (pattern : Typed.Pattern.t)]
  ;;

  module Coverage = struct
    type simple_pattern = t

    type t =
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
      List.fold_until
        patterns
        ~init:(of_pattern pattern)
        ~f:(fun coverage pattern ->
          match add_pattern coverage pattern with
          | Exhaustive -> Stop Exhaustive
          | coverage -> Continue coverage)
        ~finish:Fn.id
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
        let all_cnstrs = Context.cnstrs ctx input_type in
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
                     let input_type = Context.cnstr_arg_type ctx input_type cnstr i in
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
  module Break_label : sig
    type t [@@deriving sexp]

    val make_creator_for_whole_expression : unit -> (unit -> t) Staged.t
  end = struct
    type t = int [@@deriving sexp]

    let make_creator_for_whole_expression () =
      let counter = ref (-1) in
      stage (fun () ->
        incr counter;
        !counter)
    ;;
  end

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
  (* TODO: since llvm requires type annotations everywhere, I'll probably have to add type
     information to this at some point. Seems easy to do when needed. *)
  type t =
    | Primitive of Literal.t
    | Name of Unique_name.t
    (* TODO: recursive lets? Mutual recursion? (will surely need a rec flag at least)
       Can maybe handle that in toplevel function definitions. *)
    | Let of Unique_name.t * t * t
    (* TODO: closure env - pass a pointer to a struct with all the free variables *)
    | Closure of func
    | Fun_call of t * t Nonempty.t
    | Constant_cnstr of Cnstr.Tag.t
    | Make_block of Cnstr.Tag.t * t Nonempty.t
    | Get_block_field of int * t
    | If of
        { cond : cond
        ; then_ : t
        ; else_ : t
        }
    (* TODO: enforce that all switch cases have the same type + support switch on blocks
       - actually I'm not sure if this makes sense - variants will have different
         Value_kinds e.g. Int64 vs Block *)
    (*| Switch of
        { expr : t
        ; cases : (Literal.t Nonempty.t * t) Nonempty.t
        ; default : t option
        }*)
    (* TODO: Apparently catch/exit can pass bindings around? Is it basically a function
       call at that point? Note: we could just have bindings that go through the exit 
       - do llvm bindings work like this? *)
    | Catch of
        { label : Break_label.t
        ; body : t
        ; with_ : t
        }
    | Break of Break_label.t

  (* TODO: monomorphize functions/types per [Value_kind.t]. For now we can just box
       everything. *)
  and func =
    { args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t
    ; body : t
    }

  and block =
    | Pointers_first of
        { pointers : t list
        ; immediates : t list
        }

  and cond =
    | Equals of t * Literal.t
    | Constant_tag_equals of t * Cnstr.Tag.t
    | Non_constant_tag_equals of t * Cnstr.Tag.t
    (*| Or of cond * cond*)
    | And of cond * cond
  [@@deriving sexp_of]

  (* FIXME: We must integrate with [make_condition]:
      Example: `let ((5, x) | (x, _)) = (1, 2)`
      Other options/ideas:
      - Use flat patterns (make that a type) and ignore unions
      - Convert non-rec lets into match expressions (at the code should be shared)
      - Ban patterns beside names in let rec <- bad
      Wait, since type-checking is done, let rec is the same as match with just
      the names in the pattern bound in the body e.g. 
      `let f x = if x <= 0 then 1 else f (x / 2); f 7`
      is equivalent to
      `match fun x -> if x <= 0 then 1 else f (x / 2) | f -> f 7`
      supposing that `f` is also bound in the matched expression *)
  (* TODO: consider merging making bindings with making conditions *)
  let rec fold_pattern_bindings
    ?(add_name = Context.add_value_name)
    ~ctx
    ~init:acc
    ~add_let
    pat
    mir_expr
    =
    (* TODO: warn/error if bindings are not exhaustive *)
    match (pat : Simple_pattern.t) with
    | Catch_all None ->
      (* TODO: warn about unused expressions. NOTE: we can only elide the bound expression
         as we are currently assuming purity. Later we should check for effects. *)
      ctx, acc
    | Catch_all (Some name) ->
      let ctx, name = add_name ctx name in
      ctx, add_let acc name mir_expr
    | As (pattern, name) ->
      let ctx, name = add_name ctx name in
      let acc = add_let acc name mir_expr in
      fold_pattern_bindings ~ctx ~init:acc ~add_let pattern (Name name)
    | Cnstr_appl (_, args) ->
      let ctx, name, acc =
        match mir_expr with
        | Name name -> ctx, name, acc
        | _ ->
          let ctx, name = add_name ctx Constant_names.empty in
          let acc = add_let acc name mir_expr in
          ctx, name, acc
      in
      List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
        fold_pattern_bindings ~ctx ~init:acc ~add_let arg (Get_block_field (i, Name name)))
    | Constant _ -> ctx, acc
  ;;

  let rec condition_of_pattern ~ctx ~input_expr ~input_type pattern =
    match (pattern : Simple_pattern.t) with
    | Catch_all _ -> None
    | As (pattern, _) -> condition_of_pattern ~ctx ~input_expr ~input_type pattern
    | Constant lit -> Some (Equals (input_expr, lit))
    | Cnstr_appl (cnstr, args) ->
      let tag = Context.cnstr_tag ctx input_type cnstr in
      let tag_cond =
        if List.is_empty args
        then Constant_tag_equals (input_expr, tag)
        else Non_constant_tag_equals (input_expr, tag)
      in
      let conds =
        List.filter_mapi args ~f:(fun i arg ->
          let arg_expr = Get_block_field (i, input_expr) in
          let arg_type = Context.cnstr_arg_type ctx input_type cnstr i in
          condition_of_pattern ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
      in
      Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond')))
  ;;

  (*let fold_let_pattern ~ctx ~init ~add_let ~pattern ~input_type ~mir_expr =
    (* FIXME: this is wrong, we need to insert conditional bindings
       How the binding is done can depend on runtime values e.g.
       `let ((5, x) | (x, _)) = (4, 5)`
       We can take nonrecursive `let`s and make them the same as match statements.
       Recursive lets seem a little harder - actually wait, you just need the parent
       names to be bound in the child contexts (anything else?)
       - Actually, recursion in our let statements is really tricky, since it's possible
         to create non-terminating values e.g. `let x = x`
         Some options:
         - Restrict recursive lets to just values/functions and then restrict the
           right-hand side like OCaml does. This is very sad as we are rec by default.
         - Work out if the value is *actually* recursive (referencing itself) - we can
           have the type checker do this and remove the rec flag - and only apply the
           restrictions if this is the case. This sounds pretty good. We may also be able
           to remove some restrictions later. *)
    Simple_pattern.flatten_typed_pattern pattern
    |> Nonempty.fold ~init ~f:(fun init pattern ->
         fold_simple_pattern ~ctx ~init ~add_let ~pattern ~input_type ~mir_expr)
  ;;*)

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

  let of_typed_expr ~ctx:whole_expression_ctx whole_expression_expr =
    let create_break_label = unstage (Break_label.make_creator_for_whole_expression ()) in
    let rec of_typed_expr ~ctx : Typed.Expr.generalized -> t = function
      | Literal lit, _ -> Primitive lit
      | Name name, _ -> Name (Context.find_value_name ctx name)
      | Fun_call (f, arg, arg_type), result_type ->
        (* TODO: catch constructor applications - can either replace the function call in
           the mir or add definitions for all the functions *)
        let rec loop (f, f_type) arg args =
          match (f : _ Typed.Expr.t) with
          | Fun_call (f', arg', arg_type') ->
            let f' = f', Type.Expr.Function (arg_type', result_type) in
            loop f' (arg', arg_type') (of_typed_expr ~ctx arg :: args)
          | Name _ | Lambda _ | Match _ | Let _ ->
            Fun_call (of_typed_expr ~ctx (f, f_type), of_typed_expr ~ctx arg :: args)
          | Literal _
          | Tuple _
          | Record_literal _
          | Record_update _
          | Record_field_access _ ->
            (* TODO: should be able to find where this is from a Node.t or something *)
            mir_error
              [%message
                "This is not a function; it cannot be applied"
                  (f : Type.Scheme.t Typed.Expr.t)]
        in
        loop (f, Function (arg_type, result_type)) (arg, arg_type) []
      | Lambda (arg, body), Function (arg_type, body_type) ->
        (* TODO: Can I tweak the fold_let_pattern interface to be less awkward? It seems to
         only really work well for the original use case with statements - maybe a
         specialized implementation for exprs would be in order
         Note also that mir_expr is used only to pass into add_let *)
        (* TODO: Should assert that the pattern is exhaustive in this and let bindings
         (exhaustive vs unconditional?) *)
        let rec bind_arg ~ctx ~arg ~arg_type ~body ~body_type args =
          let arg =
            Simple_pattern.flatten_typed_pattern_no_unions
              arg
              ~label:"function argument patterns"
          in
          let arg_name, (ctx, bindings) =
            match arg with
            | Catch_all (Some arg_name) ->
              (* Special-case named catch-all patterns (the dominant case) to skip the
               [lambda_arg] step and use the name directly. *)
              let ctx, arg_name = Context.add_value_name ctx arg_name in
              arg_name, (ctx, [])
            | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
              let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
              let add_let acc name mir_expr = (name, mir_expr) :: acc in
              arg_name, fold_pattern_bindings ~ctx arg (Name arg_name) ~init:[] ~add_let
          in
          let arg =
            ( arg_name
            , Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) arg_type )
          in
          let args, body = loop ~ctx Nonempty.(arg :: args) (body, body_type) in
          let body =
            List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
              Let (name, mir_expr, body))
          in
          args, body
        and loop ~ctx args : Typed.Expr.generalized -> _ = function
          | Lambda (arg, body), Function (arg_type, body_type) ->
            bind_arg ~ctx (Nonempty.to_list args) ~arg ~arg_type ~body ~body_type
          | body_and_type -> Nonempty.rev args, of_typed_expr ~ctx body_and_type
        in
        let args, body = bind_arg ~ctx ~arg ~arg_type ~body ~body_type [] in
        Closure
          { args
          ; body
          ; returns =
              Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) body_type
          }
      | Match (expr, expr_type, arms), output_type ->
        let expr = of_typed_expr ~ctx (expr, expr_type) in
        (match expr with
        | Name _ ->
          (* Skip binding [match_expr_name] when matching on a single variable *)
          handle_match_arms ~ctx ~input_expr:expr ~input_type:expr_type ~output_type arms
        | _ ->
          let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
          let input_expr = Name match_expr_name in
          Let
            ( match_expr_name
            , expr
            , handle_match_arms ~ctx ~input_expr ~input_type:expr_type ~output_type arms
            ))
      | Let { rec_; bindings; body }, body_type ->
        (* TODO: let statements in expressions should be able to be made into global statements
         (e.g. to define static functions/values) - not all lets should be global though e.g.
         for simple expressions like `let y = x + x; (y, y)` *)
        if rec_
        then failwith "TODO: let rec in MIR expr"
        else (
          let ctx, acc =
            Nonempty.fold
              bindings
              ~init:(ctx, [])
              ~f:(fun (ctx, acc) ((pat, typ), expr) ->
              let mir_expr = of_typed_expr ~ctx (expr, typ) in
              (* TODO: support unions in let bindings. For the non-rec case we should just
               be able to convert to a match *)
              let pat =
                Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"let bindings"
              in
              let add_let acc name mir_expr = (name, mir_expr) :: acc in
              fold_pattern_bindings ~ctx pat mir_expr ~init:acc ~add_let)
          in
          let body = of_typed_expr ~ctx (body, body_type) in
          List.fold acc ~init:body ~f:(fun body (name, mir_expr) ->
            Let (name, mir_expr, body)))
      | Tuple fields, Tuple field_types ->
        let fields =
          List.map2_exn fields field_types ~f:(fun field typ ->
            of_typed_expr ~ctx (field, typ))
        in
        (match Nonempty.of_list fields with
        | Some fields -> Make_block (Cnstr.Tag.default, fields)
        | None -> Constant_cnstr Cnstr.Tag.default)
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]
    and handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms =
      let rec loop_one_arm ~pattern ~output_expr ~coverage arms =
        let patterns = Simple_pattern.flatten_typed_pattern pattern in
        let coverage' = Simple_pattern.Coverage.of_patterns patterns in
        let coverage =
          match coverage with
          | None -> coverage'
          | Some coverage -> Simple_pattern.Coverage.combine coverage coverage'
        in
        let output = output_expr, output_type in
        let fallback =
          if List.is_empty arms then None else Some (fun () -> loop ~coverage arms)
        in
        handle_match_arm ~ctx ~input_expr ~input_type ~output ?fallback patterns
      and loop ~coverage = function
        | [] ->
          (match Simple_pattern.Coverage.missing_cases ~ctx ~input_type coverage with
          | [] ->
            compiler_bug [%message "Pattern coverage/condition checking is out of sync"]
          | missing_cases ->
            mir_error
              [%message
                "This pattern match is not exhaustive"
                  (missing_cases : Simple_pattern.t list)])
        | (pattern, output_expr) :: arms ->
          loop_one_arm ~pattern ~output_expr ~coverage:(Some coverage) arms
      in
      let ((pattern, output_expr) :: arms) = arms in
      loop_one_arm ~pattern ~output_expr ~coverage:None arms
    and handle_match_arm ~ctx ~input_expr ~input_type ~output ?fallback patterns =
      (* FIXME: sharing of break labels between patterns in an arm is just broken *)
      let label = create_break_label () in
      let rec loop Nonempty.(pattern :: patterns) =
        let add_let body name expr = Let (name, expr, body) in
        fold_pattern_bindings ~ctx pattern input_expr ~init:(Break label) ~add_let
        |> Tuple2.map_snd ~f:(fun output_expr ->
             match condition_of_pattern ~ctx ~input_expr ~input_type pattern with
             | None ->
               (* TODO: warn about unused patterns (the other arms) *)
               output_expr
             | Some cond ->
               (match Nonempty.of_list patterns with
               | Some patterns ->
                 If { cond; then_ = output_expr; else_ = snd (loop patterns) }
               | None ->
                 (match fallback with
                 | None ->
                   (* This is the last case, so just elide the condition *)
                   assert_or_compiler_bug ~here:[%here] (List.is_empty patterns);
                   output_expr
                 | Some fallback -> fallback ())))
      in
      (* NOTE: All patterns in a union must bind the same names with the same types, so we
       can use any one of the [Context.t]s to create the output expression as they will
       all be equivalent. *)
      let ctx, body = loop patterns in
      Catch { label; body; with_ = of_typed_expr ~ctx output }
    in
    of_typed_expr ~ctx:whole_expression_ctx whole_expression_expr
  ;;

  let rec map_names expr ~f =
    match expr with
    | Primitive _ | Break _ | Constant_cnstr _ -> expr
    | Name name -> Name (f name)
    | Let (name, expr, body) ->
      let name = f name in
      let expr = map_names expr ~f in
      let body = map_names body ~f in
      Let (name, expr, body)
    | Closure { args; body; returns } ->
      let args = Nonempty.map args ~f:(Tuple2.map_fst ~f) in
      let body = map_names body ~f in
      Closure { args; body; returns }
    | Fun_call (func, args) ->
      let func = map_names func ~f in
      let args = Nonempty.map args ~f:(map_names ~f) in
      Fun_call (func, args)
    | Make_block (tag, fields) -> Make_block (tag, Nonempty.map fields ~f:(map_names ~f))
    | Get_block_field (i, expr) -> Get_block_field (i, map_names expr ~f)
    | If { cond; then_; else_ } ->
      let cond = map_cond_names cond ~f in
      let then_ = map_names then_ ~f in
      let else_ = map_names else_ ~f in
      If { cond; then_; else_ }
    | Catch { label; body; with_ } ->
      let body = map_names body ~f in
      let with_ = map_names with_ ~f in
      Catch { label; body; with_ }

  and map_cond_names cond ~f =
    match cond with
    | Equals (expr, lit) -> Equals (map_names expr ~f, lit)
    | Constant_tag_equals (expr, tag) -> Constant_tag_equals (map_names expr ~f, tag)
    | Non_constant_tag_equals (expr, tag) ->
      Non_constant_tag_equals (map_names expr ~f, tag)
    | And (cond, cond') ->
      let cond = map_cond_names cond ~f in
      let cond' = map_cond_names cond' ~f in
      And (cond, cond')
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

  let map_names (Value_def (name, expr)) ~f =
    let name = f name in
    let expr = Expr.map_names expr ~f in
    Value_def (name, expr)
  ;;
end

(* TODO: need to handle ordering? function definitions can have side effects *)
type t = Stmt.t list [@@deriving sexp_of]

let of_typed_module =
  let handle_let_bindings ~ctx ~stmts (bindings : _ Node.t list) =
    let ctx =
      List.fold bindings ~init:ctx ~f:(fun ctx { node = pattern, _; _ } ->
        Pattern.Names.fold pattern ~init:ctx ~f:(fun ctx name ->
          fst (Context.add_value_name ctx name)))
    in
    (* TODO: elide toplevel let bindings which bind no names (and are pure) *)
    (* TODO: warn about eliding toplevel let bindings *)
    List.fold
      bindings
      ~init:(ctx, stmts)
      ~f:(fun (ctx, stmts) { node = pattern, ((_, input_type) as expr); _ } ->
      let mir_expr = Expr.of_typed_expr ~ctx expr in
      let add_let stmts name mir_expr = Stmt.Value_def (name, mir_expr) :: stmts in
      (* TODO: Support pattern unions in toplevel let bindings - should work roughly
                the same as recursive bindings in expressions *)
      let pattern' =
        Simple_pattern.flatten_typed_pattern_no_unions
          pattern
          ~label:"toplevel let bindings"
      in
      let missing_cases =
        Simple_pattern.Coverage.(of_pattern pattern' |> missing_cases ~ctx ~input_type)
      in
      if not (List.is_empty missing_cases)
      then
        mir_error
          [%message
            "The pattern in this let binding is not exhaustive"
              (pattern : Typed.Pattern.t)
              (missing_cases : Simple_pattern.t list)];
      let add_name ctx name =
        (* TODO: this seems error-prone, especially since we use names like
           [Value_name.empty] elsewhere in the AST, e.g. for match variables.
           Might be a good idea to add a variant for constant names, or something. *)
        if Constant_names.mem name
        then
          (* Constant names are always made-up anew *)
          (* FIXME: let bindings for these will not be added in the right place, I think *)
          Context.add_value_name ctx name
        else
          (* Look up non-constant names added at the beginning *)
          ctx, Context.find_value_name ctx ([], name)
      in
      Expr.fold_pattern_bindings ~add_name ~ctx pattern' mir_expr ~init:stmts ~add_let)
  in
  let rec loop ~ctx ~stmts (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, stmts) ~f:(fun (ctx, stmts) def ->
      match def.Node.node with
      | Let bindings -> handle_let_bindings ~ctx ~stmts bindings
      | Module (module_name, _sigs, defs) ->
        Context.with_module ctx module_name ~f:(fun ctx -> loop ~ctx ~stmts defs)
      | Trait (_, _, _, _) | Impl (_, _, _, _) -> failwith "TODO: MIR traits/impls"
      | Common_def _ -> ctx, stmts)
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    try
      let names = Name_bindings.into_module names module_name ~place:`Def in
      let _ctx, stmts = loop ~ctx:(Context.of_name_bindings names) ~stmts:[] defs in
      Ok (List.rev stmts)
    with
    | Compilation_error.Compilation_error error -> Error error
    | Mir_error msg -> Error (Compilation_error.create Mir_error ~msg)
;;

let map_names stmts ~f = List.map stmts ~f:(Stmt.map_names ~f)

let renumber_ids stmts =
  let counter = ref (-1) in
  let id_table = Int.Table.create () in
  map_names stmts ~f:(fun name ->
    Unique_name.map_id name ~f:(fun id ->
      Hashtbl.find_or_add id_table id ~default:(fun () ->
        incr counter;
        !counter)))
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
