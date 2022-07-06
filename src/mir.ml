open Import
open Names

exception Mir_error of Sexp.t [@@deriving sexp]

let mir_error msg = raise (Mir_error msg)

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
       platforms we should have 3 free bits. This could be especially helpful for
       implementing unboxed options or similar types. *)

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

module Block_index : sig
  type t [@@deriving sexp]

  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int [@@deriving sexp]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Constant_names : sig
  val empty : Value_name.t
  val fun_ : Value_name.t
  val lambda_arg : Value_name.t
  val match_ : Value_name.t
  val synthetic_arg : int -> Value_name.t
  val mem : Value_name.t -> bool
end = struct
  (* NOTE: none of these can be valid value names a user could enter. *)

  let empty = Value_name.empty
  let fun_ = Value_name.of_string_unchecked "*fun"
  let match_ = Value_name.of_string_unchecked "match"
  let lambda_arg = Value_name.of_string_unchecked "*lambda_arg"

  let constant_names_table =
    Value_name.Hash_set.of_list [ empty; fun_; match_; lambda_arg ]
  ;;

  let synthetic_arg i =
    let argi = Value_name.of_string_exn [%string "arg%{i#Int}"] in
    Hash_set.add constant_names_table argi;
    argi
  ;;

  let mem = Hash_set.mem constant_names_table
end

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.t -> t * Unique_name.t
  val find_value_name : t -> Value_name.Qualified.t -> Unique_name.t
  val find_value_name' : t -> Value_name.Qualified.t -> Unique_name.t option

  type find_observer := Value_name.Qualified.t -> Unique_name.t -> unit

  val with_find_observer : t -> f:(find_observer -> find_observer) -> t
  val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
  val cnstr_arg_type : t -> Type.Scheme.t -> Cnstr.t -> Block_index.t -> Type.Scheme.t
  val cnstrs : t -> Type.Scheme.t -> Cnstr.Set.t
  val cnstr_tag : t -> Type.Scheme.t -> Cnstr.t -> Cnstr.Tag.t
  val cnstr_type : t -> Type.Scheme.t -> Cnstr.t -> Block_index.t -> Type.Scheme.t
  val name_bindings : t -> Name_bindings.t
end = struct
  module Cnstr_info = struct
    type t =
      | Variants of
          { constant_cnstrs : Cnstr_name.t list
          ; non_constant_cnstrs : (Cnstr_name.t * Type.Scheme.t list) list
          }
      | Tuple of Type.Scheme.t list
    [@@deriving sexp_of]

    let of_variants variants =
      let constant_cnstrs, non_constant_cnstrs =
        List.partition_map variants ~f:(fun (cnstr_name, args) ->
          if List.is_empty args then First cnstr_name else Second (cnstr_name, args))
      in
      Variants { constant_cnstrs; non_constant_cnstrs }
    ;;
  end

  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t [@sexp.opaque]
    ; cnstr_info_cache : Cnstr_info.t Type.Scheme.Table.t
    ; find_observer : Value_name.Qualified.t -> Unique_name.t -> unit [@sexp.opaque]
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
    let name' = Unique_name.create name in
    { t with names = Map.set t.names ~key:name ~data:name' }, name'
  ;;

  let add_value_name t name =
    if Constant_names.mem name
    then add t ([], name)
    else (
      let path = Name_bindings.(current_path t.name_bindings |> Path.to_module_path) in
      add t (path, name))
  ;;

  let find { names; name_bindings; _ } name =
    match Map.find names (Value_name.Qualified.to_ustring name) with
    | Some _ as name -> name
    | None ->
      (match Name_bindings.find_entry name_bindings name with
      | exception Name_bindings.Name_error _ -> None
      | entry ->
        Name_bindings.Name_entry.extern_name entry
        |> Option.map ~f:(Unique_name.create << Extern_name.to_ustring))
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
    match find_value_name' t name with
    | Some name' ->
      t.find_observer name name';
      name'
    | None ->
      compiler_bug
        [%message
          "Name missing from context"
            (name : Value_name.Qualified.t)
            (t.names : Unique_name.t Ustring.Map.t)]
  ;;

  let with_find_observer t ~f = { t with find_observer = f t.find_observer }

  let of_name_bindings name_bindings =
    let t =
      { names = Ustring.Map.empty
      ; name_bindings
      ; cnstr_info_cache = Type.Scheme.Table.create ()
      ; find_observer = (fun _ _ -> ())
      }
    in
    Name_bindings.fold_local_names name_bindings ~init:t ~f:(fun t name _entry ->
      fst (add t name))
  ;;

  let cnstr_lookup_failed ?cnstr_name type_ =
    compiler_bug
      [%message
        "Constructor lookup failed"
          (type_ : Type.Scheme.t)
          (cnstr_name : Cnstr_name.t option)]
  ;;

  let rec get_cnstr_info t type_ =
    match (type_ : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      Hashtbl.find_or_add t.cnstr_info_cache type_ ~default:(fun () ->
        match
          snd (Name_bindings.find_type_decl ~defs_only:true t.name_bindings type_name)
        with
        | Alias alias -> get_cnstr_info t alias
        | Variants variants -> Cnstr_info.of_variants variants
        | Abstract | Record _ -> cnstr_lookup_failed type_)
    | Tuple args ->
      Hashtbl.find_or_add t.cnstr_info_cache type_ ~default:(fun () ->
        Cnstr_info.Tuple args)
    | Function _ | Partial_function _ | Var _ -> cnstr_lookup_failed type_
  ;;

  let lookup_cnstr t type_ cnstr =
    match get_cnstr_info t type_, (cnstr : Cnstr.t) with
    | Variants { constant_cnstrs; non_constant_cnstrs }, Named cnstr_name ->
      (match List.findi constant_cnstrs ~f:(fun _ -> Cnstr_name.( = ) cnstr_name) with
      | Some (i, _) -> `Variants (Cnstr.Tag.of_int i, [])
      | None ->
        (match
           List.findi non_constant_cnstrs ~f:(fun _ -> Cnstr_name.( = ) cnstr_name << fst)
         with
        | Some (i, (_, args)) -> `Variants (Cnstr.Tag.of_int i, args)
        | None -> cnstr_lookup_failed ~cnstr_name type_))
    | Tuple args, Tuple -> `Tuple args
    | cnstr_info, cnstr ->
      compiler_bug
        [%message "Incompatible cnstr info" (cnstr : Cnstr.t) (cnstr_info : Cnstr_info.t)]
  ;;

  let cnstr_type t typ cnstr_name arg_index =
    let (`Variants (_, args) | `Tuple args) = lookup_cnstr t typ cnstr_name in
    List.nth_exn args (Block_index.to_int arg_index)
  ;;

  let cnstr_tag t typ cnstr =
    match lookup_cnstr t typ cnstr with
    | `Variants (index, _) -> index
    | `Tuple _ -> Cnstr.Tag.default
  ;;

  (* TODO: remove awkward split/duplication between cached monomorphic code and uncached
     polymorphic code. *)
  (*let cnstrs t typ =
    match get_cnstr_info t typ with
    | Variants { constant_cnstrs; non_constant_cnstrs } ->
      Cnstr.Set.of_list
        (List.map constant_cnstrs ~f:Cnstr.named
        @ List.map non_constant_cnstrs ~f:(Cnstr.named << fst))
    | Tuple _ -> Cnstr.Set.singleton Tuple
  ;;*)

  let cnstr_lookup_failed ?cnstr_name scheme =
    compiler_bug
      [%message
        "Constructor lookup failed"
          (scheme : Type.Scheme.t)
          (cnstr_name : Cnstr_name.t option)]
  ;;

  let rec get_cnstr_info t scheme =
    match (scheme : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      let _params, decl =
        Name_bindings.find_type_decl ~defs_only:true t.name_bindings type_name
      in
      (match decl with
      | Alias alias -> get_cnstr_info t alias
      | Variants variants -> `Variants variants
      | Abstract | Record _ -> cnstr_lookup_failed scheme)
    | Tuple args -> `Tuple args
    | Function _ | Partial_function _ | Var _ -> cnstr_lookup_failed scheme
  ;;

  let cnstrs t scheme =
    match get_cnstr_info t scheme with
    | `Variants variants -> List.map variants ~f:(Cnstr.named << fst) |> Cnstr.Set.of_list
    | `Tuple _ -> Cnstr.Set.singleton Tuple
  ;;

  let cnstr_arg_type t scheme cnstr index =
    let index = Block_index.to_int index in
    match get_cnstr_info t scheme, (cnstr : Cnstr.t) with
    | `Variants variants, Named cnstr_name ->
      List.find_map_exn variants ~f:(fun (cnstr_name', args) ->
        if Cnstr_name.(cnstr_name = cnstr_name')
        then Some (List.nth_exn args index)
        else None)
    | `Tuple fields, Tuple -> List.nth_exn fields index
    | _ ->
      compiler_bug [%message "Context.cnstr_arg_type: invalid cnstr" (cnstr : Cnstr.t)]
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
                     let input_type =
                       Context.cnstr_arg_type ctx input_type cnstr (Block_index.of_int i)
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
    | Fun_call of Unique_name.t * t Nonempty.t
    | Make_block of
        { tag : Cnstr.Tag.t
        ; fields : t list [@sexp.omit_nil]
        }
    | Get_block_field of Block_index.t * t
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

  and cond =
    (* TODO: In practice, the expressions in conditions have to be simple names. Maybe we
       should enforce that in the type here. *)
    | Equals of t * Literal.t
    | Constant_tag_equals of t * Cnstr.Tag.t
    | Non_constant_tag_equals of t * Cnstr.Tag.t
    | And of cond * cond
  [@@deriving sexp_of]

  module Fun_def = struct
    type nonrec t =
      { fun_name : Unique_name.t
      ; closed_over : Unique_name.Set.t
      ; args : Unique_name.t Nonempty.t
      ; body : t
      }
    [@@deriving sexp_of]
  end

  (* TODO: consider merging making bindings with making conditions. If we extended
     [Coverage.t] to include some notion of what conditions have been tested as well, it
     could help us avoid unnecessary checks. *)
  (* TODO: I think this should probably just collect the bindings into a list and give
     them to you - I think most of the uses do this anyway. We could also try to do
     clever stuff like noticing if a pattern binds only the empty name and no "real"
     names, and then returning no bindings. *)
  let fold_pattern_bindings =
    let rec loop ~ctx ~add_let ~add_name acc pat mir_expr type_ =
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
        loop ~ctx ~add_let ~add_name acc pattern (Name name) type_
      | Cnstr_appl (cnstr, args) ->
        let ctx, name, acc =
          match mir_expr with
          | Name name -> ctx, name, acc
          | _ ->
            (* When adding an empty name, this should always create a new entry in the
               context, so we bypass `add_name`. *)
            let ctx, name = Context.add_value_name ctx Constant_names.empty in
            let acc = add_let acc name mir_expr in
            ctx, name, acc
        in
        List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
          let arg_index = Block_index.of_int i in
          let arg_type = Context.cnstr_arg_type ctx type_ cnstr arg_index in
          let arg_expr = Get_block_field (arg_index, Name name) in
          loop ~ctx ~add_let ~add_name acc arg arg_expr arg_type)
      | Constant _ -> ctx, acc
    in
    fun ?(add_name = Context.add_value_name) ~ctx ~init ~add_let pat mir_expr type_ ->
      loop ~ctx ~add_let ~add_name init pat mir_expr type_
  ;;

  let rec condition_of_pattern ~ctx ~input_expr ~input_type pattern =
    match (pattern : Simple_pattern.t) with
    | Catch_all _ -> None
    | As (pattern, _) -> condition_of_pattern ~ctx ~input_expr ~input_type pattern
    | Constant lit -> Some (Equals (input_expr, lit))
    | Cnstr_appl (cnstr, args) ->
      let tag = Context.cnstr_tag ctx input_type cnstr in
      if List.is_empty args
      then Some (Constant_tag_equals (input_expr, tag))
      else (
        let tag_cond = Non_constant_tag_equals (input_expr, tag) in
        let conds =
          List.filter_mapi args ~f:(fun i arg ->
            let arg_index = Block_index.of_int i in
            let arg_type = Context.cnstr_arg_type ctx input_type cnstr arg_index in
            let arg_expr = Get_block_field (arg_index, input_expr) in
            condition_of_pattern ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
        in
        Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond'))))
  ;;

  (* TODO: We need to insert conditional bindings in match statements
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

  (* TODO: switch statement optimization
     See:
     - https://github.com/ocaml/ocaml/blob/trunk/lambda/matching.ml
     - https://www.researchgate.net/publication/2840783_Optimizing_Pattern_Matching  *)

  (* TODO: add tests for/consider supporting local let generalization (?) *)

  module Just_bound = struct
    type t =
      { rec_ : bool
      ; names_bound : Unique_name.Set.t
      }
  end

  let add_let_bindings ~bindings ~body =
    List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
      match mir_expr with
      | Name name' when Unique_name.equal name name' ->
        (* Avoid genereating code that looks like let x.0 = x.0 *)
        body
      | _ -> Let (name, mir_expr, body))
  ;;

  let rec check_rec_binding_expr expr =
    match (expr : _ Typed.Expr.t) with
    | Lambda _ | Tuple _ | Record_literal _ -> ()
    | Let { body; bindings = _; rec_ = _ } -> check_rec_binding_expr body
    | Match (_, _, arms) ->
      Nonempty.iter arms ~f:(fun (_, expr) -> check_rec_binding_expr expr)
    | Name _ | Fun_call _ | Record_update _ | Record_field_access _ ->
      (* TODO: Consider relaxing this to allow more kinds of expressions e.g. function
         calls which don't mention the recursive names.
         See: https://v2.ocaml.org/manual/letrecvalues.html *)
      Compilation_error.raise
        Mir_error
        ~msg:
          [%message
            "This kind of expression is not allowed on the right-hand side of a \
             recursive let binding"
              (expr : _ Typed.Expr.t)]
    | Literal _ ->
      compiler_bug
        [%message "Impossible expr in recursive binding" (expr : _ Typed.Expr.t)]
  ;;

  let generate_let_bindings
    ~ctx
    ~rec_
    ~init
    ~add_let
    ~extract_binding
    ~process_expr
    bindings
    =
    let ctx_for_body, bindings =
      Nonempty.fold_map bindings ~init:ctx ~f:(fun ctx_for_body binding ->
        let pattern, _, _ = extract_binding binding in
        let ctx_for_body, names_bound =
          Pattern.Names.fold
            pattern
            ~init:(ctx_for_body, Unique_name.Set.empty)
            ~f:(fun (ctx_for_body, names_bound) name ->
            let ctx, name = Context.add_value_name ctx_for_body name in
            ctx, Set.add names_bound name)
        in
        ctx_for_body, (binding, names_bound))
    in
    let ctx = if rec_ then ctx_for_body else ctx in
    (* TODO: Warn about let bindings which bind no names and are pure. *)
    Nonempty.fold
      bindings
      ~init:(ctx_for_body, init)
      ~f:(fun (ctx_for_body, acc) (binding, names_bound) ->
      let pat, typ, expr = extract_binding binding in
      if rec_ then check_rec_binding_expr expr;
      (* TODO: support unions in let bindings. For the non-rec case we should
         just be able to convert to a match *)
      let pat' =
        Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"let bindings"
      in
      let missing_cases =
        Simple_pattern.Coverage.(of_pattern pat' |> missing_cases ~ctx ~input_type:typ)
      in
      if not (List.is_empty missing_cases)
      then
        mir_error
          [%message
            "The pattern in this let binding is not exhaustive"
              ~pattern:(pat : Typed.Pattern.t)
              (missing_cases : Simple_pattern.t list)];
      let acc, mir_expr =
        process_expr acc ~just_bound:{ Just_bound.rec_; names_bound } ~ctx expr typ
      in
      let add_name ctx name = ctx, Context.find_value_name ctx ([], name) in
      fold_pattern_bindings
        ~ctx:ctx_for_body
        pat'
        mir_expr
        typ
        ~init:acc
        ~add_let
        ~add_name)
  ;;

  let of_typed_expr
    ~just_bound:outer_just_bound
    ~ctx:outer_ctx
    ~add_fun_def
    outer_expr
    outer_type
    =
    let create_break_label = unstage (Break_label.make_creator_for_whole_expression ()) in
    let rec of_typed_expr ?just_bound ~ctx expr expr_type =
      match (expr : Type.Scheme.t Typed.Expr.t), (expr_type : Type.Scheme.t) with
      | Literal lit, _ -> Primitive lit
      | Name name, _ -> Name (Context.find_value_name ctx name)
      | Fun_call (fun_, args_and_types), body_type ->
        let fun_call () =
          let arg_types = Nonempty.map ~f:snd args_and_types in
          let fun_type = Type.Expr.Function (arg_types, body_type) in
          let fun_ = of_typed_expr ~ctx fun_ fun_type in
          let args =
            Nonempty.map args_and_types ~f:(fun (arg, arg_type) ->
              of_typed_expr ~ctx arg arg_type)
          in
          match fun_ with
          | Name fun_name -> Fun_call (fun_name, args)
          | Let _ | Fun_call _ | Get_block_field _ | If _ | Catch _ | Break _ ->
            (* FIXME: does this make any sense? I suppose this is meant to hit the last
               defined function or something? I think this just seems wrong though? It's
               adding a new name, so it can't refer to some already existing name, right? *)
            let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
            Let (fun_name, fun_, Fun_call (fun_name, args))
          | Primitive _ | Make_block _ ->
            compiler_bug [%message "Invalid function expression" (fun_ : t)]
        in
        (match fun_ with
        | Name (_, name) ->
          (* Special-case constructor applications *)
          (match Value_name.to_cnstr_name name with
          | Ok cnstr_name ->
            let tag = Context.cnstr_tag ctx expr_type (Named cnstr_name) in
            let fields, field_types = List.unzip (Nonempty.to_list args_and_types) in
            make_block ~ctx ~tag ~fields ~field_types
          | Error _ -> fun_call ())
        | _ -> fun_call ())
      | Lambda (args, body), Function (arg_types, body_type) ->
        (* TODO: Still need to try and coalesce lambdas/other function expressions for
           function definitions which are partially applied. See example in
           test/ast/TypeChecking.expected. *)
        Name (add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound)
      | Match (expr, input_type, arms), output_type ->
        let input_expr = of_typed_expr ~ctx expr input_type in
        (match expr with
        | Name _ ->
          (* Skip binding [match_expr_name] when matching on a single variable *)
          handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
        | _ ->
          let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
          let body =
            let input_expr = Name match_expr_name in
            handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
          in
          Let (match_expr_name, input_expr, body))
      | Let { rec_; bindings; body }, body_type ->
        (* TODO: let statements in expressions should be able to be made into global
           statements (e.g. to define static functions/values) - not all lets should be
           global though e.g. for simple expressions like `let y = x + x; (y, y)` *)
        let ctx, bindings =
          generate_let_bindings
            ~ctx
            ~rec_
            ~init:[]
            ~add_let:(fun bindings name mir_expr -> (name, mir_expr) :: bindings)
            ~extract_binding:(fun ((pat, typ), expr) -> pat, typ, expr)
            ~process_expr:(fun bindings ~just_bound ~ctx expr typ ->
              bindings, of_typed_expr ~just_bound ~ctx expr typ)
            bindings
        in
        let body = of_typed_expr ~ctx body body_type in
        add_let_bindings ~bindings ~body
      | Tuple fields, Tuple field_types ->
        make_block ~ctx ~tag:Cnstr.Tag.default ~fields ~field_types
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]
      | _, Partial_function _ -> .
    and add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound =
      (* FIXME: I think this probably doesn't handle [closed_over] interacting with
         [just_bound] in both the recursive and non-recursive cases. *)
      (* Keep track of the parent context before binding any variables. This lets us
         check which variables are captured by closures later on. *)
      let parent_ctx = ctx in
      let (ctx, bindings), args =
        Nonempty.zip_exn args arg_types
        |> Nonempty.fold_map ~init:(ctx, []) ~f:(fun (ctx, bindings) (arg, arg_type) ->
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
               let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
               let add_let acc name mir_expr = (name, mir_expr) :: acc in
               let mir_expr = Name arg_name in
               let ctx, bindings =
                 fold_pattern_bindings ~ctx arg mir_expr arg_type ~init:bindings ~add_let
               in
               (ctx, bindings), arg_name)
      in
      (* FIXME: Shouldn't include [fun_name] in [bound_names] if recursive. *)
      let fun_name =
        match just_bound with
        | Some { names_bound; _ } when Set.length names_bound = 1 ->
          Set.choose_exn names_bound
        | Some _ | None -> snd (Context.add_value_name ctx Constant_names.fun_)
      in
      let closed_over = ref Unique_name.Set.empty in
      let ctx =
        let from_which_context name unique_name =
          let in_context ctx =
            Context.find_value_name' ctx name
            |> Option.value_map ~default:false ~f:(Unique_name.equal unique_name)
          in
          if in_context outer_ctx
          then `From_outer_stmts
          else if in_context parent_ctx
          then `Closed_over_from_parent
          else `Newly_bound
        in
        (* Determine if names looked up were closed over from the parent context. *)
        Context.with_find_observer ctx ~f:(fun parent_observer name unique_name ->
          match from_which_context name unique_name with
          | `Newly_bound | `From_outer_stmts -> ()
          | `Closed_over_from_parent ->
            parent_observer name unique_name;
            closed_over := Set.add !closed_over unique_name)
      in
      let body = of_typed_expr ~ctx body body_type in
      let body = add_let_bindings ~bindings ~body in
      let closed_over =
        match just_bound with
        | Some { rec_ = true; names_bound } ->
          (* Don't close over recursively bound arguments *)
          Set.diff !closed_over names_bound
        | None | Some { rec_ = false; _ } -> !closed_over
      in
      add_fun_def { Fun_def.fun_name; closed_over; args; body };
      fun_name
    and make_block ~ctx ~tag ~fields ~field_types =
      let fields = List.map2_exn fields field_types ~f:(of_typed_expr ~ctx) in
      Make_block { tag; fields }
    and handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms =
      let rec loop_one_arm ~pattern ~output_expr ~coverage arms =
        let patterns = Simple_pattern.flatten_typed_pattern pattern in
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
          ?fallback
          patterns
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
    and handle_single_arm
      ~ctx
      ~input_expr
      ~input_type
      ~output_expr
      ~output_type
      ?fallback
      patterns
      =
      let label = create_break_label () in
      let rec loop Nonempty.(pattern :: patterns) =
        let add_let bindings name expr = (name, expr) :: bindings in
        let ctx', bindings =
          fold_pattern_bindings ~ctx pattern input_expr input_type ~init:[] ~add_let
        in
        (* TODO: this should skip underscore bindings (bindings for no actual variables) *)
        let output_expr = add_let_bindings ~bindings ~body:(Break label) in
        let output_expr' =
          match condition_of_pattern ~ctx ~input_expr ~input_type pattern with
          | None ->
            (* TODO: warn about unused patterns (the other arms) *)
            output_expr
          | Some cond ->
            (match Nonempty.of_list patterns, fallback with
            | Some patterns, _ ->
              let _ctx, else_ = loop patterns in
              If { cond; then_ = output_expr; else_ }
            | None, Some fallback ->
              let else_ = fallback () in
              If { cond; then_ = output_expr; else_ }
            | None, None ->
              (* This is the last case, so just elide the condition *)
              output_expr)
        in
        ctx', output_expr'
      in
      (* NOTE: All patterns in a union must bind the same names with the same types, so we
         can use any one of the [Context.t]s to create the output expression as they will
         all be equivalent. *)
      let ctx, body = loop patterns in
      (* TODO: if there is only one pattern, we could skip the catch/break.
         (pass in a function to create the output_expr) *)
      Catch { label; body; with_ = of_typed_expr ~ctx output_expr output_type }
    in
    of_typed_expr ~just_bound:outer_just_bound ~ctx:outer_ctx outer_expr outer_type
  ;;

  let rec map_names expr ~f =
    match expr with
    | Primitive _ | Break _ -> expr
    | Name name -> Name (f name)
    | Let (name, expr, body) ->
      let name = f name in
      let expr = map_names expr ~f in
      let body = map_names body ~f in
      Let (name, expr, body)
    | Fun_call (fun_name, args) ->
      let fun_name = f fun_name in
      let args = Nonempty.map args ~f:(map_names ~f) in
      Fun_call (fun_name, args)
    | Make_block { tag; fields } ->
      let fields = List.map fields ~f:(map_names ~f) in
      Make_block { tag; fields }
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
  type t =
    | Value_def of Unique_name.t * Expr.t
    | Fun_def of Expr.Fun_def.t
  [@@deriving sexp_of, variants]

  let name = function
    | Value_def (name, _) -> name
    | Fun_def { fun_name; _ } -> fun_name
  ;;

  let map_names stmt ~f =
    match stmt with
    | Value_def (name, expr) ->
      let name = f name in
      let expr = Expr.map_names expr ~f in
      Value_def (name, expr)
    | Fun_def { fun_name; closed_over; args; body } ->
      let fun_name = f fun_name in
      let closed_over = Unique_name.Set.map closed_over ~f in
      let args = Nonempty.map args ~f in
      let body = Expr.map_names body ~f in
      Fun_def { fun_name; closed_over; args; body }
  ;;
end

type t = Stmt.t list [@@deriving sexp_of]

(* TODO: Consider adding some kind of fold over types that follows aliases. *)
let rec type_get_function ~names typ =
  match (typ : Type.Scheme.t) with
  | Function (arg_types, body_type) -> Some (arg_types, body_type)
  | Var _ | Tuple _ -> None
  | Type_app (type_name, _args) ->
    (match snd (Name_bindings.find_type_decl ~defs_only:true names type_name) with
    | Alias alias -> type_get_function ~names alias
    | Variants _ | Record _ | Abstract -> None)
  | Partial_function _ -> .
;;

let of_typed_module =
  let handle_let_bindings
    ~ctx
    ~stmts
    ~rec_
    (bindings : (Typed.Pattern.t * Typed.Expr.generalized) Node.t Nonempty.t)
    =
    let process_expr stmts ~just_bound ~ctx expr typ =
      let stmts = ref stmts in
      let add_fun_def fun_def = stmts := Stmt.Fun_def fun_def :: !stmts in
      let expr = Expr.of_typed_expr ~just_bound ~ctx ~add_fun_def expr typ in
      !stmts, expr
    in
    let add_let stmts name mir_expr =
      match (mir_expr : Expr.t) with
      | Name name' when Unique_name.(name = name') ->
        (* Don't make a Value_def in the case where all we did is make a Fun_def *)
        stmts
      | _ -> Stmt.Value_def (name, mir_expr) :: stmts
    in
    Expr.generate_let_bindings
      bindings
      ~ctx
      ~rec_
      ~init:stmts
      ~add_let
      ~extract_binding:(fun { Node.node = pat, (expr, typ); _ } -> pat, typ, expr)
      ~process_expr
  in
  let rec loop ~ctx ~stmts (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, stmts) ~f:(fun (ctx, stmts) def ->
      match def.node with
      | Let { rec_; bindings } -> handle_let_bindings ~ctx ~stmts ~rec_ bindings
      | Module (module_name, _sigs, defs) ->
        Context.with_module ctx module_name ~f:(fun ctx -> loop ~ctx ~stmts defs)
      | Trait _ | Impl _ -> failwith "TODO: MIR traits/impls"
      (* TODO: Should probably preserve extern declarations *)
      | Common_def
          ( Val _
          | Extern _
          | Type_decl _
          | Trait_sig _
          | Import _
          | Import_with _
          | Import_without _ ) -> ctx, stmts)
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
  let name_table = Ustring.Table.create () in
  map_names stmts ~f:(fun name ->
    Unique_name.map_id name ~f:(fun id ->
      let name = Unique_name.base_name name in
      match Hashtbl.find name_table name with
      | None ->
        let id_table = Int.Table.create () in
        Hashtbl.set id_table ~key:id ~data:0;
        Hashtbl.set name_table ~key:name ~data:id_table;
        0
      | Some id_table ->
        Hashtbl.find_or_add id_table id ~default:(fun () -> Hashtbl.length id_table)))
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
