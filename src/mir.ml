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
end

module Cnstr_tag : sig
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

module Cnstr_info : sig
  type t [@@deriving sexp_of]

  (* TODO: Consider exposing a function to zip with the list of args. I think the
     callsites generally do that, which leads to looking up each constructor separately. *)

  val tag : t -> Cnstr.t -> Cnstr_tag.t
  val arg_type : t -> Cnstr.t -> Block_index.t -> Type.Scheme.t
  val cnstrs : t -> Cnstr.t list

  val fold
    :  t
    -> init:'acc
    -> f:('acc -> Cnstr.t -> Cnstr_tag.t -> Type.Scheme.t list -> 'acc)
    -> 'acc

  val of_variants : (Cnstr_name.t * Type.Scheme.t list) list -> t
  val of_tuple : Type.Scheme.t list -> t
end = struct
  (* TODO: The constant/non-constant split might not be that useful really. Probably get
     rid of it. *)
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

module Extern_decl = struct
  type t =
    { name : Mir_name.t
    ; arity : int
    }
  [@@deriving sexp]
end

(* TODO: This doesn't handle polymorphic types particularly smartly. Should think about
   whether that matters. *)
let rec arity_of_type ~names : Type.Scheme.t -> int = function
  | Var _ | Tuple _ -> 0
  | Type_app (type_name, _) ->
    (match snd (Name_bindings.find_type_decl ~defs_only:true names type_name) with
     | Abstract | Variants _ | Record _ -> 0
     | Alias type_ -> arity_of_type ~names type_)
  | Function (args, _) -> Nonempty.length args
  | Partial_function _ -> .
;;

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.t -> t * Mir_name.t

  val find_value_name
    :  t
    -> Value_name.Qualified.t
    -> add_extern_decl:(Extern_decl.t -> unit)
    -> Mir_name.t

  val find_value_name_assert_local : t -> Value_name.t -> Mir_name.t
  val peek_value_name : t -> Value_name.Qualified.t -> Mir_name.t option

  type find_observer := Value_name.Qualified.t -> Mir_name.t -> unit

  val with_find_observer : t -> f:(find_observer -> find_observer) -> t
  val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
  val find_cnstr_info : t -> Type.Scheme.t -> Cnstr_info.t
  val find_cnstr_info_from_decl : t -> Type.Decl.decl -> Cnstr_info.t option
end = struct
  type extern_info =
    | Local
    | External of { arity : int }
  [@@deriving sexp_of]

  type t =
    { names : (Mir_name.t * extern_info) Value_name.Qualified.Map.t
    ; name_bindings : Name_bindings.t [@sexp.opaque]
    ; find_observer : Value_name.Qualified.t -> Mir_name.t -> unit [@sexp.opaque]
    }
  [@@deriving sexp_of]

  let with_module t module_name ~f =
    let name_bindings =
      Name_bindings.into_module t.name_bindings module_name ~place:`Def
    in
    let t, x = f { t with name_bindings } in
    { t with name_bindings = Name_bindings.into_parent name_bindings }, x
  ;;

  (* TODO: Creating new unique names here means uses of imported names are
     different in different files, which is weird. Maybe [Name_bindings] should be
     responsible for assigning unique names for names in its own file? *)
  let add t name ~extern_name ~extern_info =
    let mir_name =
      match extern_name with
      | None -> Mir_name.create (Value_name.Qualified.to_ustring name)
      | Some extern_name -> Mir_name.of_extern_name extern_name
    in
    { t with names = Map.set t.names ~key:name ~data:(mir_name, extern_info) }, mir_name
  ;;

  let add_value_name t name =
    if Constant_names.mem name
    then add t ([], name) ~extern_name:None ~extern_info:Local
    else (
      let path = Name_bindings.(current_path t.name_bindings |> Path.to_module_path) in
      add t (path, name) ~extern_name:None ~extern_info:Local)
  ;;

  let find { names; _ } name =
    match Map.find names name with
    | Some _ as name -> name
    | None -> None
  ;;

  (* FIXME: What does this case do? I don't think it makes sense. *)
  (* (match Name_bindings.find_entry name_bindings name with
       | exception Name_bindings.Name_error _ -> None
       | entry ->
         Name_bindings.Name_entry.extern_name entry
         |> Option.map ~f:Mir_name.of_extern_name) *)

  let peek_value_name_internal t name =
    match name with
    | [], name when Constant_names.mem name -> find t ([], name)
    | _ ->
      let name =
        try Name_bindings.absolutify_value_name t.name_bindings name with
        | Name_bindings.Name_error error ->
          (* FIXME: Cleanup *)
          if String.is_substring (Ustring.to_string error) ~substring:"List.Cons"
          then
            raise_s
              [%message
                "name error absolutifying value name"
                  (error : Ustring.t)
                  (t.name_bindings : Name_bindings.t)];
          Name_bindings.(current_path t.name_bindings |> Path.to_module_path), snd name
      in
      find t name
  ;;

  let find_value_name_internal t name =
    match peek_value_name_internal t name with
    | Some ((name', _) as entry) ->
      t.find_observer name name';
      entry
    | None ->
      compiler_bug
        [%message
          "Name missing from context"
            (name : Value_name.Qualified.t)
            (t.names : (Mir_name.t * extern_info) Value_name.Qualified.Map.t)]
  ;;

  let peek_value_name t name = peek_value_name_internal t name |> Option.map ~f:fst

  let find_value_name t name ~add_extern_decl =
    let name, extern_info = find_value_name_internal t name in
    (match extern_info with
     | Local -> ()
     | External { arity } -> add_extern_decl { Extern_decl.name; arity });
    name
  ;;

  let find_value_name_assert_local t name =
    let name, extern_info = find_value_name_internal t ([], name) in
    (match extern_info with
     | Local -> ()
     | External _ ->
       compiler_bug [%message "Unexpected non-local value" (extern_info : extern_info)]);
    name
  ;;

  let with_find_observer t ~f = { t with find_observer = f t.find_observer }

  let of_name_bindings name_bindings =
    (* FIXME: We add all these names, but then still allow looking them up from the name
       bindings later. I don't think this makes sense. *)
    let t =
      { names = Value_name.Qualified.Map.empty
      ; name_bindings
      ; find_observer = (fun _ _ -> ())
      }
    in
    let current_path =
      Name_bindings.current_path name_bindings |> Name_bindings.Path.to_module_path
    in
    Name_bindings.fold_local_names
      name_bindings
      ~init:t
      ~f:(fun t ((path, _) as name) entry ->
      let extern_name = Name_bindings.Name_entry.extern_name entry in
      let extern_info =
        if Option.is_none extern_name && Module_path.is_prefix ~prefix:current_path path
        then Local
        else (
          let scheme =
            Option.value_or_thunk
              (Name_bindings.Name_entry.scheme entry)
              ~default:(fun () ->
              compiler_bug
                [%message
                  "Didn't find type scheme for external name entry"
                    (name : Value_name.Qualified.t)
                    (entry : Name_bindings.Name_entry.t)])
          in
          External { arity = arity_of_type ~names:name_bindings scheme })
      in
      fst (add t name ~extern_name ~extern_info))
  ;;

  let cnstr_info_lookup_failed type_ =
    compiler_bug [%message "Constructor info lookup failed" (type_ : Type.Scheme.t)]
  ;;

  let rec find_cnstr_info_internal t type_ =
    match (type_ : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      let decl =
        snd (Name_bindings.find_type_decl ~defs_only:true t.name_bindings type_name)
      in
      find_cnstr_info_from_decl t decl
    | Tuple args -> Some (Cnstr_info.of_tuple args)
    | Function _ | Partial_function _ | Var _ -> cnstr_info_lookup_failed type_

  and find_cnstr_info_from_decl t decl =
    match (decl : Type.Decl.decl) with
    | Alias alias -> find_cnstr_info_internal t alias
    | Variants variants -> Some (Cnstr_info.of_variants variants)
    | Abstract | Record _ -> None
  ;;

  let find_cnstr_info t type_ =
    option_or_default (find_cnstr_info_internal t type_) ~f:(fun () ->
      compiler_bug [%message "Constructor info lookup failed" (type_ : Type.Scheme.t)])
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

  (* TODO: remove or make into a proper thing *)
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

  module Fun_def = struct
    type nonrec t =
      { fun_name : Mir_name.t
      ; closed_over : Mir_name.Set.t
      ; args : Mir_name.t Nonempty.t
      ; body : t
      }
    [@@deriving sexp_of]
  end

  (* TODO: consider merging making bindings with making conditions. If we extended
     [Coverage.t] to include some notion of what conditions have been tested as well, it
     could help us avoid unnecessary checks. *)

  (** [fold_pattern_bindings] folds over the variable bindings formed by a pattern
      associated with an expression. *)
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
        let cnstr_info = Context.find_cnstr_info ctx type_ in
        List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
          let arg_index = Block_index.of_int i in
          let arg_type = Cnstr_info.arg_type cnstr_info cnstr arg_index in
          let arg_expr = Get_block_field (arg_index, mir_expr) in
          loop ~ctx ~add_let ~add_name acc arg arg_expr arg_type)
      | Constant _ -> ctx, acc
    in
    fun ?(add_name = Context.add_value_name) ~ctx ~init:acc ~add_let pat expr_name type_ ->
      loop ~ctx ~add_let ~add_name acc pat (Name expr_name) type_
  ;;

  let convert_expr_to_name
    ?(binding_name = Constant_names.empty)
    ~ctx
    ~default
    ~add_let
    mir_expr
    =
    match mir_expr with
    | Name expr_name -> ctx, default, expr_name
    | _ ->
      let ctx_for_body, expr_name = Context.add_value_name ctx binding_name in
      let acc = add_let expr_name mir_expr in
      ctx_for_body, acc, expr_name
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
            let arg_type = Cnstr_info.arg_type cnstr_info cnstr arg_index in
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
      ; names_bound : Mir_name.Set.t
      }
  end

  let add_let_bindings ~bindings ~body =
    List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
      match mir_expr with
      | Name name' when Mir_name.equal name name' ->
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
            ~init:(ctx_for_body, Mir_name.Set.empty)
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
      let add_name ctx name = ctx, Context.find_value_name_assert_local ctx name in
      let binding_name =
        match pat' with
        | Catch_all (Some name) | As (_, name) -> name
        | Catch_all None -> Value_name.of_string_unchecked "_"
        | Constant _ | Cnstr_appl _ -> Constant_names.empty
      in
      let ctx_for_body, acc, expr_name =
        convert_expr_to_name
          ~ctx:ctx_for_body
          ~default:acc
          ~add_let:(add_let acc)
          ~binding_name
          mir_expr
      in
      fold_pattern_bindings
        ~ctx:ctx_for_body
        pat'
        expr_name
        typ
        ~init:acc
        ~add_let
        ~add_name)
  ;;

  let of_typed_expr
    ~just_bound:outer_just_bound
    ~ctx:outer_ctx
    ~add_fun_def
    ~add_extern_decl
    outer_expr
    outer_type
    =
    let rec of_typed_expr ?just_bound ~ctx expr expr_type =
      match (expr : Type.Scheme.t Typed.Expr.t), (expr_type : Type.Scheme.t) with
      | Literal lit, _ -> Primitive lit
      | Name name, _ -> Name (Context.find_value_name ctx name ~add_extern_decl)
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
          | Let _ | Fun_call _ | Get_block_field _ | Cond_assign _ ->
            let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
            Let (fun_name, fun_, Fun_call (fun_name, args))
          | Primitive _ | Make_block _ ->
            compiler_bug [%message "Invalid function expression" (fun_ : t)]
        in
        (match fun_ with
         | Name (_, name) ->
           (* TODO: I think there's no need for this special-casing actually: we can just
              use the constructor functions directly. *)
           (* Special-case constructor applications *)
           (match Value_name.to_cnstr_name name with
            | Ok cnstr_name ->
              let cnstr_info = Context.find_cnstr_info ctx expr_type in
              let tag = Cnstr_info.tag cnstr_info (Named cnstr_name) in
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
        make_block ~ctx ~tag:Cnstr_tag.default ~fields ~field_types
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]
      | _, Partial_function _ -> .
    and add_lambda ~ctx ~args ~arg_types ~body ~body_type ~just_bound =
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
               let ctx, bindings =
                 fold_pattern_bindings ~ctx arg arg_name arg_type ~init:bindings ~add_let
               in
               (ctx, bindings), arg_name)
      in
      let fun_name =
        match just_bound with
        | Some { names_bound; _ } when Set.length names_bound = 1 ->
          Set.choose_exn names_bound
        | Some _ | None -> snd (Context.add_value_name ctx Constant_names.fun_)
      in
      let closed_over = ref Mir_name.Set.empty in
      let ctx =
        let from_which_context name mir_name =
          let in_context ctx =
            Context.peek_value_name ctx name
            |> Option.value_map ~default:false ~f:(Mir_name.equal mir_name)
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
          ~fallback
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
      ~fallback
      patterns
      =
      let ctx, vars =
        List.fold_map
          (Simple_pattern.names (Nonempty.hd patterns) |> Set.to_list)
          ~init:ctx
          ~f:Context.add_value_name
      in
      let ctx, wrapping_binding, input_expr_name =
        convert_expr_to_name
          ~ctx
          ~default:None
          ~add_let:(fun name expr -> Some (name, expr))
          input_expr
      in
      let body = of_typed_expr ~ctx output_expr output_type in
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
              input_expr_name
              input_type
              ~init:[]
              ~add_let
              ~add_name
          in
          let bindings =
            (* Bindings must be sorted by their names to match up with [vars] above. This
               relies on the [Value_name.t]s and [Mir_name.t]s having the same ordering
               due to the former being a prefix of the latter. *)
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
          (* TODO: Warn if [fallback] is [Some], since we ignoring following match arms. *)
          add_last_unconditional_bindings ~conds ~last_bindings
      in
      match wrapping_binding with
      | None -> result_expr
      | Some (name, expr) -> Let (name, expr, result_expr)
    in
    of_typed_expr ~just_bound:outer_just_bound ~ctx:outer_ctx outer_expr outer_type
  ;;

  let rec map_names expr ~f =
    match expr with
    | Primitive _ -> expr
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
    | Cond_assign { vars; conds; body; if_none_matched } ->
      let vars = List.map vars ~f in
      let conds =
        Nonempty.map conds ~f:(fun (cond, bindings) ->
          let cond = map_cond_names cond ~f in
          let bindings = List.map bindings ~f:(map_names ~f) in
          cond, bindings)
      in
      let body = map_names body ~f in
      let if_none_matched = map_if_none_matched_names if_none_matched ~f in
      Cond_assign { vars; conds; body; if_none_matched }

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

  and map_if_none_matched_names if_none_matched ~f =
    match if_none_matched with
    | Otherwise expr -> Otherwise (map_names expr ~f)
    | Use_bindings bindings -> Use_bindings (List.map bindings ~f:(map_names ~f))
  ;;
end

module Stmt = struct
  (* TODO: Consider passing through information about whether the values/functions are
     exposed to other files so we can decide on proper LLVM linkage for them. *)
  type t =
    | Value_def of Mir_name.t * Expr.t
    | Fun_def of Expr.Fun_def.t
    | Extern_decl of Extern_decl.t
  [@@deriving sexp_of, variants]

  let map_names stmt ~f =
    match stmt with
    | Value_def (name, expr) ->
      let name = f name in
      let expr = Expr.map_names expr ~f in
      Value_def (name, expr)
    | Fun_def { fun_name; closed_over; args; body } ->
      let fun_name = f fun_name in
      let closed_over = Mir_name.Set.map closed_over ~f in
      let args = Nonempty.map args ~f in
      let body = Expr.map_names body ~f in
      Fun_def { fun_name; closed_over; args; body }
    | Extern_decl { name; arity } ->
      let name = f name in
      Extern_decl { name; arity }
  ;;
end

type t = Stmt.t list [@@deriving sexp_of]

let of_typed_module =
  let handle_let_bindings
    ~ctx
    ~stmts
    ~rec_
    ~extern_decls
    (bindings : (Typed.Pattern.t * Typed.Expr.generalized) Node.t Nonempty.t)
    =
    let process_expr stmts ~just_bound ~ctx expr typ =
      let stmts = ref stmts in
      let add_fun_def fun_def = stmts := Stmt.Fun_def fun_def :: !stmts in
      let add_extern_decl (extern_decl : Extern_decl.t) =
        if not (Hash_set.mem extern_decls extern_decl.name)
        then (
          Hash_set.add extern_decls extern_decl.name;
          stmts := Stmt.Extern_decl extern_decl :: !stmts)
      in
      let expr =
        Expr.of_typed_expr ~just_bound ~ctx ~add_fun_def ~add_extern_decl expr typ
      in
      !stmts, expr
    in
    let add_let stmts name mir_expr =
      match (mir_expr : Expr.t) with
      | Name name' when Mir_name.(name = name') ->
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
  let generate_variant_constructor_values ~ctx ~stmts decl =
    match Context.find_cnstr_info_from_decl ctx decl with
    | None -> ctx, stmts
    | Some cnstr_info ->
      Cnstr_info.fold cnstr_info ~init:(ctx, stmts) ~f:(fun (ctx, stmts) cnstr tag args ->
        match cnstr with
        | Tuple -> ctx, stmts
        | Named cnstr_name ->
          let outer_ctx, name =
            Context.add_value_name ctx (Value_name.of_cnstr_name cnstr_name)
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
                ; closed_over = Mir_name.Set.empty
                ; args = arg_names |> Nonempty.of_list_exn
                ; body =
                    Make_block
                      { tag; fields = List.map arg_names ~f:(fun name -> Expr.Name name) }
                })
          in
          outer_ctx, stmt :: stmts)
  in
  let rec loop ~ctx ~names ~stmts ~extern_decls (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, stmts) ~f:(fun (ctx, stmts) def ->
      match def.node with
      | Let { rec_; bindings } ->
        handle_let_bindings ~ctx ~stmts ~rec_ ~extern_decls bindings
      | Module (module_name, _sigs, defs) ->
        Context.with_module ctx module_name ~f:(fun ctx ->
          loop ~ctx ~names ~stmts ~extern_decls defs)
      | Trait _ | Impl _ -> failwith "TODO: MIR traits/impls"
      (* TODO: Should probably preserve extern declarations *)
      | Common_def (Type_decl ((_ : Type_name.t), ((_, decl) : Type.Decl.t))) ->
        generate_variant_constructor_values ~ctx ~stmts decl
      | Common_def
          (Extern ((_ : Value_name.t), (_ : Fixity.t option), type_, extern_name)) ->
        ( ctx
        , Extern_decl
            { name = Mir_name.of_extern_name extern_name
            ; arity = arity_of_type ~names type_
            }
          :: stmts )
      | Common_def (Val _ | Trait_sig _ | Import _ | Import_with _ | Import_without _) ->
        ctx, stmts)
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    let names = Name_bindings.into_module names module_name ~place:`Def in
    let ctx = Context.of_name_bindings names in
    let extern_decls = Mir_name.Hash_set.create () in
    match loop ~ctx ~names ~stmts:[] ~extern_decls defs with
    | (_ : Context.t), stmts -> Ok (List.rev stmts)
    | exception Compilation_error.Compilation_error error -> Error error
    | exception Mir_error msg -> Error (Compilation_error.create Mir_error ~msg)
;;

let map_names stmts ~f = List.map stmts ~f:(Stmt.map_names ~f)

let renumber_ids stmts =
  let name_table = Ustring.Table.create () in
  map_names stmts ~f:(fun name ->
    Mir_name.map_parts name ~f:(fun name id ->
      match Hashtbl.find name_table name with
      | None ->
        let id_table = Int.Table.create () in
        Hashtbl.set id_table ~key:id ~data:0;
        Hashtbl.set name_table ~key:name ~data:id_table;
        0
      | Some id_table ->
        Hashtbl.find_or_add id_table id ~default:(fun () -> Hashtbl.length id_table)))
;;
