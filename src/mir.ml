open Import
open Names

exception Mir_error of Sexp.t [@@deriving sexp]

let mir_error msg = raise (Mir_error msg)

module Value_kind = struct
  type immediate =
    [ `Int64
    | `Float64
    | `Char (* Unicode scalar value: 4 bytes *)
    ]
  [@@deriving compare, equal, hash, sexp]

  type pointer = [ `Block ] [@@deriving compare, equal, hash, sexp]

  type t =
    [ immediate
    | pointer
      (* FIXME: decide if we want to use Any *)
      (*| `Any (* `Any` is used for unused/phantom type variables e.g. the `a` in `None`. *)*)
    ]
  [@@deriving compare, equal, hash, sexp]

  let is_immediate : t -> bool = function
    | #immediate -> true
    | #pointer -> false
  ;;

  let is_pointer : t -> bool = function
    | #pointer -> true
    | #immediate -> false
  ;;

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

  let of_type_scheme ~names:_ _ =
    (* FIXME: change usages of this to handle monomorphization properly *)
    failwith "Value_kind.of_type_scheme"
  ;;
end

module Monomorphic_type : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val create : scheme:Type.Scheme.t -> param_kinds:Value_kind.t Type.Param.Map.t -> t
  val scheme : t -> Type.Scheme.t
  val to_value_kind : names:Name_bindings.t -> t -> Value_kind.t

  val of_type_alias
    :  names:Name_bindings.t
    -> parent:t
    -> alias:Type.Scheme.t
    -> params:Type.Param.t list
    -> args:Type.Scheme.t list
    -> t

  val of_concrete : Type.Concrete.t -> t
  val instantiate_child : t -> Type.Scheme.t -> t
end = struct
  module T = struct
    type t =
      { scheme : Type.Scheme.t
      ; param_kinds : Value_kind.t Type.Param.Map.t
      }
    [@@deriving compare, equal, fields, hash, sexp_of]
  end

  include T

  let create = Fields.create

  let rec to_value_kind ~names ({ scheme; param_kinds } as t) =
    match scheme with
    | Type_app (type_name, args) ->
      option_or_default (Value_kind.of_primitive_type type_name) ~f:(fun () ->
        let decl = Name_bindings.find_type_decl ~defs_only:true names type_name in
        match snd decl with
        | Alias alias ->
          to_value_kind
            ~names
            (of_type_alias ~names ~parent:t ~alias ~params:(fst decl) ~args)
        | Variants _ | Record _ -> `Block
        | Abstract ->
          (* TODO: We could allow abstract types in defs if the [Value_kind] was specified
             in an annotation. We could also have a better error message *)
          mir_error
            [%message
              "Type definition in def is abstract (missing a definition)"
                (decl : Type.Decl.t)])
    | Tuple _ | Function _ | Partial_function _ -> `Block
    | Var param -> Map.find_exn param_kinds param

  and of_type_alias ~names ~parent ~alias ~params ~args =
    let param_kinds =
      List.fold2_exn
        params
        args
        ~init:parent.param_kinds
        ~f:(fun param_kinds' param arg ->
        let arg_kind =
          to_value_kind ~names { param_kinds = parent.param_kinds; scheme = arg }
        in
        Map.set param_kinds' ~key:param ~data:arg_kind)
    in
    { scheme = alias; param_kinds }
  ;;

  let of_concrete concrete =
    { scheme = Type.Concrete.cast concrete; param_kinds = Type.Param.Map.empty }
  ;;

  let instantiate_child t scheme = { t with scheme }

  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
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

module Block_index : sig
  type 'kind t constraint 'kind = [< `Raw | `Processed ] [@@deriving sexp]
  type raw = [ `Raw ] t [@@deriving sexp]
  type processed = [ `Processed ] t [@@deriving sexp]

  val raw : int -> [ `Raw ] t
  val processed : int -> [ `Processed ] t
  val to_int : _ t -> int
end = struct
  type 'kind t = int constraint 'kind = [< `Raw | `Processed ] [@@deriving sexp]
  type raw = [ `Raw ] t [@@deriving sexp]
  type processed = [ `Processed ] t [@@deriving sexp]

  let raw = Fn.id
  let processed = Fn.id
  let to_int = Fn.id
end

module Constant_names = struct
  (* NOTE: none of these can be valid value names a user could enter. *)
  let empty = Value_name.empty
  let fun_ = Value_name.of_string_unchecked "*fun"
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
  val with_add_observer : t -> f:(Unique_name.t -> unit) -> t
  val with_find_observer : t -> f:((Unique_name.t -> unit) -> Unique_name.t -> unit) -> t
  val with_module : t -> Module_name.t -> f:(t -> t * 'a) -> t * 'a
  val cnstr_arg_type : t -> Type.Scheme.t -> Cnstr.t -> Block_index.raw -> Type.Scheme.t
  val cnstrs : t -> Type.Scheme.t -> Cnstr.Set.t
  val cnstr_tag : t -> Monomorphic_type.t -> Cnstr.t -> Cnstr.Tag.t

  val cnstr_arg_info
    :  t
    -> Monomorphic_type.t
    -> Cnstr.t
    -> Block_index.raw
    -> Block_index.processed * Monomorphic_type.t

  val name_bindings : t -> Name_bindings.t
end = struct
  (* FIXME: decide where monomorphization code should live exactly 
     Note that we need to monomorphize type declarations (actually can be done here, lazily)
     as well as function/(value?) defs. Polymorphic function def information should add to
     the context, which can then trigger creation of more monomorphic function defs *)
  module Cnstr_info = struct
    module Arg = struct
      type t = Block_index.processed * Monomorphic_type.t [@@deriving sexp_of]
    end

    type t =
      | Variants of
          { constant_cnstrs : Cnstr_name.t list
          ; non_constant_cnstrs : (Cnstr_name.t * Arg.t list) list
          }
      | Tuple of Arg.t list
    [@@deriving sexp_of]

    let make_arg_list ~names type_ args =
      List.fold_map args ~init:(0, 0) ~f:(fun (pointer_i, immediate_i) arg ->
        let arg = Monomorphic_type.instantiate_child type_ arg in
        let value_kind = Monomorphic_type.to_value_kind ~names arg in
        if Value_kind.is_pointer value_kind
        then (pointer_i + 1, immediate_i), (Block_index.processed pointer_i, arg)
        else (pointer_i, immediate_i + 1), (Block_index.processed immediate_i, arg))
      |> snd
    ;;

    let of_variants ~names type_ variants =
      let constant_cnstrs, non_constant_cnstrs =
        List.fold
          variants
          ~init:([], [])
          ~f:(fun (constant_cnstrs, non_constant_cnstrs) (cnstr_name, args) ->
          if List.is_empty args
          then cnstr_name :: constant_cnstrs, non_constant_cnstrs
          else
            ( constant_cnstrs
            , (cnstr_name, make_arg_list ~names type_ args) :: non_constant_cnstrs ))
      in
      Variants
        { constant_cnstrs = List.rev constant_cnstrs
        ; non_constant_cnstrs = List.rev non_constant_cnstrs
        }
    ;;

    let of_tuple ~names type_ args = Tuple (make_arg_list ~names type_ args)
  end

  (* TODO: should save function defs for polymorphic functions here - just make it a
     hashtable (the parts which should be global can just be mutable)
     - or we could also make it another type, might be a bit nicer to spread the
       responsibility 
       - this is the way to go since we can't use exprs here (circular dependency) *)
  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t [@sexp.opaque]
    ; cnstr_info_cache : Cnstr_info.t Monomorphic_type.Table.t
    ; add_observer : Unique_name.t -> unit
         [@sexp.opaque]
         (* FIXME: add_observer is a confusing name, should be called adding_observer or
            observer_of_adding, observe_adding, observer_of_add, etc. *)
    ; find_observer : Unique_name.t -> unit [@sexp.opaque]
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
    t.add_observer name';
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
        |> Option.map ~f:(Unique_name.of_ustring << Extern_name.to_ustring))
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
    | Some name ->
      t.find_observer name;
      name
    | None ->
      compiler_bug
        [%message
          "Name missing from context"
            (name : Value_name.Qualified.t)
            (t.names : Unique_name.t Ustring.Map.t)]
  ;;

  let with_add_observer t ~f = { t with add_observer = f }
  let with_find_observer t ~f = { t with find_observer = f t.find_observer }

  let of_name_bindings name_bindings =
    let t =
      { names = Ustring.Map.empty
      ; name_bindings
      ; cnstr_info_cache = Monomorphic_type.Table.create ()
      ; add_observer = ignore
      ; find_observer = ignore
      }
    in
    Name_bindings.fold_local_names name_bindings ~init:t ~f:(fun t name _entry ->
      fst (add t name))
  ;;

  let cnstr_lookup_failed ?cnstr_name type_ =
    compiler_bug
      [%message
        "Constructor lookup failed"
          (type_ : Monomorphic_type.t)
          (cnstr_name : Cnstr_name.t option)]
  ;;

  let rec get_cnstr_info t type_ =
    match Monomorphic_type.scheme type_ with
    | Type_app (type_name, args) ->
      Hashtbl.find_or_add t.cnstr_info_cache type_ ~default:(fun () ->
        (* TODO: there is some code duplicated here from `Monomorphic_type.to_value_kind` *)
        let params, decl =
          Name_bindings.find_type_decl ~defs_only:true t.name_bindings type_name
        in
        match decl with
        | Alias alias ->
          get_cnstr_info
            t
            (Monomorphic_type.of_type_alias
               ~names:t.name_bindings
               ~parent:type_
               ~alias
               ~params
               ~args)
        | Variants variants ->
          Cnstr_info.of_variants ~names:t.name_bindings type_ variants
        | Abstract | Record _ -> cnstr_lookup_failed type_)
    | Tuple args ->
      Hashtbl.find_or_add t.cnstr_info_cache type_ ~default:(fun () ->
        Cnstr_info.of_tuple ~names:t.name_bindings type_ args)
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

  let cnstr_arg_info t typ cnstr_name (arg_index : Block_index.raw) =
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

  let rec get_cnstr_info_polymorphic_uncached t scheme =
    match (scheme : Type.Scheme.t) with
    | Type_app (type_name, _args) ->
      let _params, decl =
        Name_bindings.find_type_decl ~defs_only:true t.name_bindings type_name
      in
      (match decl with
      | Alias alias -> get_cnstr_info_polymorphic_uncached t alias
      | Variants variants -> `Variants variants
      | Abstract | Record _ -> cnstr_lookup_failed scheme)
    | Tuple args -> `Tuple args
    | Function _ | Partial_function _ | Var _ -> cnstr_lookup_failed scheme
  ;;

  let cnstrs t scheme =
    match get_cnstr_info_polymorphic_uncached t scheme with
    | `Variants variants -> List.map variants ~f:(Cnstr.named << fst) |> Cnstr.Set.of_list
    | `Tuple _ -> Cnstr.Set.singleton Tuple
  ;;

  let cnstr_arg_type t scheme cnstr index =
    let index = Block_index.to_int index in
    match get_cnstr_info_polymorphic_uncached t scheme, (cnstr : Cnstr.t) with
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
                     let input_type =
                       Context.cnstr_arg_type ctx input_type cnstr (Block_index.raw i)
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
    (* TODO: Can function calls just be to a single [Unique_name.t]? *)
    | Fun_call of Unique_name.t * t Nonempty.t
    | Make_block of
        { tag : Cnstr.Tag.t
        ; pointers : t list [@sexp.omit_nil]
        ; immediates : t list [@sexp.omit_nil]
        }
    | Get_block_field of Block_index.processed * t
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

  (* FIXME: cleanup (remove this) *)
  (* TODO: monomorphize functions/types per [Value_kind.t]. For now we can just box
       everything. *)
  (* TODO: should be able to factor this out, Expr.t doesn't contain it *)
  (*and fun_def =
    { fun_name : Unique_name.t
    ; closed_over : Unique_name.Set.t
    ; args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t
    ; body : t
    }*)
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

  (* TODO: consider merging making bindings with making conditions. If we extended
     [Coverage.t] to include some notion of what conditions have been tested as well, it
     could help us avoid unnecessary checks. *)
  (* TODO: I think this should probably just collect the bindings into a list and give
     them to you - I think most of the uses do this anyway. We could also try to do
     clever stuff like noticing if a pattern binds only the empty name and no "real"
     names, and then returning no bindings. *)
  (*let rec fold_pattern_bindings
    ?(add_name = Context.add_value_name)
    ~ctx
    ~init:acc
    ~add_let
    pat
    mir_expr
    type_
    =
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
      fold_pattern_bindings ~ctx ~init:acc ~add_let pattern (Name name) type_
    | Cnstr_appl (cnstr, args) ->
      let ctx, name, acc =
        match mir_expr with
        | Name name -> ctx, name, acc
        | _ ->
          let ctx, name = add_name ctx Constant_names.empty in
          let acc = add_let acc name mir_expr in
          ctx, name, acc
      in
      (* FIXME: this is definitely incorrect in the pointers-first representation 
         - we need to re-index constructor information in Context.t *)
      List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
        let arg_index, arg_type =
          Context.cnstr_arg_info ctx type_ cnstr (Block_index.raw i)
        in
        let arg_expr = Get_block_field (arg_index, Name name) in
        fold_pattern_bindings ~ctx ~init:acc ~add_let arg arg_expr arg_type)
    | Constant _ -> ctx, acc
  ;;*)

  (*let rec condition_of_pattern ~ctx ~input_expr ~input_type pattern =
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
            let arg_index, arg_type =
              Context.cnstr_arg_info ctx input_type cnstr (Block_index.raw i)
            in
            let arg_expr = Get_block_field (arg_index, input_expr) in
            condition_of_pattern ~ctx ~input_expr:arg_expr ~input_type:arg_type arg)
        in
        Some (List.fold conds ~init:tag_cond ~f:(fun cond cond' -> And (cond, cond'))))
  ;;*)

  (* FIXME: make this work *)
  let fold_pattern_bindings ?add_name:_ ~ctx:_ ~init:_ ~add_let:_ _pat _mir_expr _type_ =
    failwith "fold_pattern_bindings"
  ;;

  let condition_of_pattern ~ctx:_ ~input_expr:_ ~input_type:_ _pattern =
    failwith "condition_of_pattern"
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

  let of_typed_expr
    ?just_bound:outer_just_bound
    ~ctx:outer_ctx
    ~add_lambda
    ~call
    outer_expr
    =
    let create_break_label = unstage (Break_label.make_creator_for_whole_expression ()) in
    (* FIXME: remove just_bound *)
    let rec of_typed_expr ?just_bound:_ ~ctx expr =
      match (expr : Typed.Expr.generalized) with
      | Literal lit, _ -> Primitive lit
      | Name name, _ -> Name (Context.find_value_name ctx name)
      | Fun_call (f, args_and_types), result_type ->
        (* TODO: catch constructor applications - can either replace the function call in
           the mir or add definitions for all the functions *)
        (* FIXME: cleanup *)
        (*let rec loop (f, f_type) arg args ~fun_defs =
          match (f : _ Typed.Expr.t) with
          | Fun_call (f', arg', arg_type') ->
            let f' = f', Type.Expr.Function (arg_type', result_type) in
            let arg, fun_defs = of_typed_expr ~ctx ~fun_defs arg in
            loop f' (arg', arg_type') (arg :: args) ~fun_defs
          | Name _ | Lambda _ | Match _ | Let _ ->
            let fun_, fun_defs = of_typed_expr ~ctx ~fun_defs (f, f_type) in
            let arg, fun_defs = of_typed_expr ~ctx ~fun_defs arg in
            (match fun_ with
            | Name fun_name -> Fun_call (fun_name, arg :: args), fun_defs
            | Let _ | Get_block_field _ | If _ | Catch _ | Break _ ->
              (* TODO: Allow `let`, `match`, `if`, etc. in function expressions *)
              mir_error
                [%message
                  "This kind of expression is not allowed in a function call"
                    (f : Type.Scheme.t Typed.Expr.t)]
            | Primitive _ | Fun_call _ | Make_block _ ->
              compiler_bug [%message "Invalid function expression" (fun_ : t)])
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
        loop (f, Function (arg_type, result_type)) (arg, arg_type) [] ~fun_defs*)
        (* FIXME: maybe args should be used? *)
        let _args, arg_types = Nonempty.unzip args_and_types in
        let fun_ = of_typed_expr ~ctx (f, Function (arg_types, result_type)) in
        let args = Nonempty.map args_and_types ~f:(of_typed_expr ~ctx) in
        (* FIXME: may run into issues with using param_kinds across different type
           definitions, as the param names (e.g. a) may clash. *)
        (match fun_ with
        | Name fun_name -> call ~fun_name ~args ~arg_types
        | Let _ | Fun_call _ | Get_block_field _ | If _ | Catch _ | Break _ ->
          let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
          Let (fun_name, fun_, call ~fun_name ~args ~arg_types)
        | Primitive _ | Make_block _ ->
          compiler_bug [%message "Invalid function expression" (fun_ : t)])
      | Lambda (args, body), Function (arg_types, body_type) ->
        (* TODO: Still need to try and coalesce lambdas/other function expressions for
           function definitions which are partially applied. See example in
           test/ast/TypeChecking.expected. *)
        Name (snd (add_lambda ~ctx ~args ~arg_types ~body ~body_type))
        (*let bound_names = Unique_name.Hash_set.create () in
        let closed_over = ref Unique_name.Set.empty in
        (* FIXME: cleaup. Need to bind all the args in the lambda, then create the body. *)
        (* FIXME: Need to a monomorphic type before doing any of this. For now, let's just
           store all the information we need to compile the function and re-do the
           computation each time it gets instantiated with specific types. *)
        let args, bindings, fun_defs, body_ctx =
          (* TODO: can this just be one ctx passed through? It isn't returned so it's fine *)
          (* Record all of the names bound in the function arguments *)
          let ctx = Context.with_add_observer ctx ~f:(Hash_set.add bound_names) in
          Nonempty.fold2_exn
            args
            arg_types
            ~init:([], [], fun_defs, ctx)
            ~f:(fun (args, bindings, fun_defs) arg arg_type ->
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
                arg_name, (ctx, bindings)
              | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
                let ctx, arg_name =
                  Context.add_value_name ctx Constant_names.lambda_arg
                in
                let add_let acc name mir_expr = (name, mir_expr) :: acc in
                ( arg_name
                , fold_pattern_bindings
                    ~ctx
                    arg
                    (Name arg_name)
                    arg_type
                    ~init:bindings
                    ~add_let )
            in
            let arg =
              ( arg_name
              , Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) arg_type )
            in
            arg :: args, bindings, fun_defs, ctx)
        in
        let body_ctx =
          (* Determine if names looked up were bound as function args or closed over *)
          Context.with_find_observer body_ctx ~f:(fun parent_observer name ->
            if not (Hash_set.mem bound_names name)
            then (
              parent_observer name;
              closed_over := Set.add !closed_over name))
        in
        let body, fun_defs = of_typed_expr ~ctx:body_ctx ~fun_defs (body, body_type) in
        let body =
          List.fold_right bindings ~init:body ~f:(fun (name, mir_expr) body ->
            Let (name, mir_expr, body))
        in
        let returns =
          Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) body_type
        in
        (* TODO: see if we can get rid of `just_bound`? Responsibility can move to Context
           or whatever does function definitions *)
        let fun_name =
          match just_bound with
          | Some name -> Context.find_value_name ctx ([], name)
          | None -> snd (Context.add_value_name ctx Constant_names.fun_)
        in
        let fun_def = { fun_name; closed_over = !closed_over; args; body; returns } in
        Name fun_name, fun_def :: fun_defs
        (*let rec bind_arg ~ctx ~fun_defs ~arg ~arg_type ~body ~body_type args =
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
              ( arg_name
              , fold_pattern_bindings ~ctx arg (Name arg_name) arg_type ~init:[] ~add_let
              )
          in
          let arg =
            ( arg_name
            , Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) arg_type )
          in
          let args, (body, fun_defs) =
            loop ~ctx ~fun_defs Nonempty.(arg :: args) (body, body_type)
          in
          let body =
            List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
              Let (name, mir_expr, body))
          in
          args, (body, fun_defs)
        and loop ~ctx ~fun_defs args : Typed.Expr.generalized -> _ = function
          | Lambda (arg, body), Function (arg_type, body_type) ->
            let args = Nonempty.to_list args in
            bind_arg ~ctx ~fun_defs ~arg ~arg_type ~body ~body_type args
          | body_and_type ->
            let ctx =
              (* Determine if names looked up were bound as function args or closed over *)
              Context.with_find_observer ctx ~f:(fun parent_observer name ->
                if not (Hash_set.mem bound_names name)
                then (
                  parent_observer name;
                  closed_over := Set.add !closed_over name))
            in
            Nonempty.rev args, of_typed_expr ~ctx ~fun_defs body_and_type
        in
        let args, (body, fun_defs) =
          (* Record all of the names bound in the function arguments *)
          let ctx = Context.with_add_observer ctx ~f:(Hash_set.add bound_names) in
          bind_arg ~ctx ~fun_defs ~arg ~arg_type ~body ~body_type []
        in
        let returns =
          Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) body_type
        in
        let fun_name =
          match just_bound with
          | Some name -> Context.find_value_name ctx ([], name)
          | None -> snd (Context.add_value_name ctx Constant_names.fun_)
        in
        let fun_def = { fun_name; closed_over = !closed_over; args; body; returns } in
        Name fun_name, fun_def :: fun_defs*)*)
      | Match (expr, input_type, arms), output_type ->
        let input_expr = of_typed_expr ~ctx (expr, input_type) in
        (match expr with
        | Name _ ->
          (* Skip binding [match_expr_name] when matching on a single variable *)
          handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms
        | _ ->
          let ctx, match_expr_name = Context.add_value_name ctx Constant_names.match_ in
          let input_expr = Name match_expr_name in
          let body = handle_match_arms ~ctx ~input_expr ~input_type ~output_type arms in
          Let (match_expr_name, input_expr, body))
      | Let { rec_; bindings; body }, body_type ->
        (* TODO: let statements in expressions should be able to be made into global
           statements (e.g. to define static functions/values) - not all lets should be
           global though e.g. for simple expressions like `let y = x + x; (y, y)` *)
        if rec_
        then failwith "TODO: let rec in MIR expr"
        else (
          let ctx, bindings =
            Nonempty.fold
              bindings
              ~init:(ctx, [])
              ~f:(fun (ctx, bindings) ((pat, typ), expr) ->
              (* TODO: support unions in let bindings. For the non-rec case we should
                   just be able to convert to a match *)
              let pat =
                Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"let bindings"
              in
              let just_bound =
                match (pat : Simple_pattern.t) with
                | Catch_all name -> name
                | Constant _ | As _ | Cnstr_appl _ -> None
              in
              let mir_expr = of_typed_expr ?just_bound ~ctx (expr, typ) in
              let add_let bindings name mir_expr = (name, mir_expr) :: bindings in
              (* FIXME: Support polmorphism: need to do this conversion further up *)
              let typ =
                Type.Concrete.of_polymorphic_exn typ |> Monomorphic_type.of_concrete
              in
              fold_pattern_bindings ~ctx pat mir_expr typ ~init:bindings ~add_let)
          in
          let body = of_typed_expr ~ctx (body, body_type) in
          List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
            Let (name, mir_expr, body)))
      | Tuple fields, Tuple field_types ->
        let pointers, immediates =
          List.fold2_exn
            fields
            field_types
            ~init:([], [])
            ~f:(fun (pointers, immediates) field typ ->
            let field = of_typed_expr ~ctx (field, typ) in
            if Value_kind.is_pointer
                 (Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) typ)
            then field :: pointers, immediates
            else pointers, field :: immediates)
        in
        Make_block
          { tag = Cnstr.Tag.default
          ; pointers = List.rev pointers
          ; immediates = List.rev immediates
          }
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]
      | _, Partial_function _ -> .
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
      let label = create_break_label () in
      let rec loop Nonempty.(pattern :: patterns) =
        let add_let bindings name expr = (name, expr) :: bindings in
        let ctx', bindings =
          fold_pattern_bindings ~ctx pattern input_expr input_type ~init:[] ~add_let
        in
        (* TODO: this should skip underscore bindings (bindings for no actual variables) *)
        let output_expr =
          List.fold bindings ~init:(Break label) ~f:(fun output_expr (name, mir_expr) ->
            Let (name, mir_expr, output_expr))
        in
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
      Catch { label; body; with_ = of_typed_expr ~ctx output }
    in
    of_typed_expr ?just_bound:outer_just_bound ~ctx:outer_ctx outer_expr
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
    | Make_block { tag; pointers; immediates } ->
      let pointers = List.map pointers ~f:(map_names ~f) in
      let immediates = List.map immediates ~f:(map_names ~f) in
      Make_block { tag; pointers; immediates }
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

module Fun_def = struct
  type t =
    { closed_over : Unique_name.Set.t
    ; args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end

(* FIXME: we probably want to preserve already unified type variables rather than
   re-create them here. *)
(*module Unique_param : sig
  type t [@@deriving compare, equal, hash, sexp]

  val create : unit -> t

include Comparable.S with type t := t
  
  module Kind_map : sig
    type t = Value_kind.t Map.t

    include module type of Map with type _ t := t

    (* TODO: move this into some kind of interface (it's repeated in `Type.Param`) *)
    val hash_fold_t : (Hash.state -> Value_kind.t -> Hash.state) -> Hash.state -> t -> Hash.state
  end
end = struct
  module T = Unique_id.Int ()
  include T

  module Kind_map = struct
    include Map
    include Map.Provide_hash (T)

    type nonrec t = Value_kind.t t
  end
end*)

module Function_factory : sig
  type t

  val create : unit -> t

  val add_lambda
    :  t
    -> ctx:Context.t
    -> args:Typed.Pattern.t Nonempty.t
    -> arg_types:Type.Scheme.t Nonempty.t
    -> body:Type.Scheme.t Typed.Expr.t
    -> body_type:Type.Scheme.t
    -> Context.t * Unique_name.t

  (* FIXME: Problem: we want to call this recursively, with funcion calls inside other
     functions. Whenever we call this, we should be in the scope of some monomorphic
     substition. *)
  val call
    :  t
    -> fun_name:Unique_name.t
    -> args:Expr.t Nonempty.t
    -> arg_types:Type.Scheme.t Nonempty.t
    -> Expr.t

  val pop_fun_defs : t -> Fun_def.t list
end = struct
  (* TODO: things like closed_over, the arg names, etc. are not unique among instances,
     they could be shared in the template *)
  module Instance = struct
    type t =
      { closed_over : Unique_name.Set.t
      ; args : (Unique_name.t * Value_kind.t) Nonempty.t
      ; returns : Value_kind.t
      ; body : t
      }
  end

  module Template = struct
    type t =
      { ctx : Context.t
      ; args : (Typed.Pattern.t * Type.Scheme.t) Nonempty.t
      ; body : Type.Scheme.t Typed.Expr.t
      ; body_type : Type.Scheme.t
      ; instances : Instance.t Type.Param.Table.t
      }

    let create ~ctx ~args ~body ~body_type =
      { ctx; args; body; body_type; instances = Type.Param.Table.create () }
    ;;

    (*let create_fun_def ~ctx ~args ~arg_exprs ~body ~body_type =
      let bound_names = Unique_name.Hash_set.create () in
      let closed_over = ref Unique_name.Set.empty in
      let args, bindings, fun_defs, body_ctx =
        (* TODO: can this just be one ctx passed through? It isn't returned so it's fine.
             Look at the previous code and copy what it does. *)
        (* Record all of the names bound in the function arguments *)
        let ctx = Context.with_add_observer ctx ~f:(Hash_set.add bound_names) in
        Nonempty.fold2_exn
          args
          arg_types
          ~init:([], [], fun_defs, ctx)
          ~f:(fun (args, bindings, fun_defs) arg arg_type ->
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
              arg_name, (ctx, bindings)
            | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
              let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
              let add_let acc name mir_expr = (name, mir_expr) :: acc in
              ( arg_name
              , fold_pattern_bindings
                  ~ctx
                  arg
                  (Name arg_name)
                  arg_type
                  ~init:bindings
                  ~add_let )
          in
          let arg =
            ( arg_name
            , Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) arg_type )
          in
          arg :: args, bindings, fun_defs, ctx)
      in
      let body_ctx =
        (* Determine if names looked up were bound as function args or closed over *)
        Context.with_find_observer body_ctx ~f:(fun parent_observer name ->
          if not (Hash_set.mem bound_names name)
          then (
            parent_observer name;
            closed_over := Set.add !closed_over name))
      in
      let body, fun_defs = of_typed_expr ~ctx:body_ctx ~fun_defs (body, body_type) in
      let body =
        List.fold_right bindings ~init:body ~f:(fun (name, mir_expr) body ->
          Let (name, mir_expr, body))
      in
      let returns =
        Value_kind.of_type_scheme ~names:(Context.name_bindings ctx) body_type
      in
      (* TODO: see if we can get rid of `just_bound`? Responsibility can move to Context
           or whatever does function definitions *)
      (* FIXME: Can we just leave the fun_name out of fun_def? *)
      let fun_name =
        match just_bound with
        | Some name -> Context.find_value_name ctx ([], name)
        | None -> snd (Context.add_value_name ctx Constant_names.fun_)
      in
      { fun_name; closed_over = !closed_over; args; body; returns }
    ;;*)

    (* FIXME: make this work *)
    (*let instantiate { ctx; args; body; body_type; instances } ~args:arg_exprs ~arg_types =
      let param_kinds =
        Nonempty.fold2_exn
          args
          arg_types
          ~init:Param_kind_map.empty
          ~f:(fun param_kinds (_, template_type) instance_type ->
          Map.merge
            param_kinds
            (Type.Scheme.infer_param_map ~template_type ~instance_type)
            ~f:(fun ~key:param -> function
            | `Left kind -> Some kind
            | `Right type_ ->
              (* FIXME: Problem: we may not get back a concrete type. We don't always have
                 enough information to determine the value_kind here. We need help from
                 outer scopes. (Since we may be in a polymorphic context with params
                 passed down from above). We need to determine what those substitutions
                 are. Note: it's not good enough to just rely on the type scheme! The
                 names in there are reused constantly! They can't be used outside of it. *)
              ()))
      in
      Hashtbl.find_or_add instances fun_type ~default:(fun () ->
        create_fun_def ~ctx ~args ~arg_exprs ~body ~body_type ~param_kinds)
    ;;*)

    let instantiate _ ~args:_ ~arg_types:_ = failwith "instantiate"
  end

  type t =
    { templates : Template.t Unique_name.Table.t
    ; fun_defs : Fun_def.t Queue.t
    }

  let create () = { templates = Unique_name.Table.create (); fun_defs = Queue.create () }

  let add_lambda { templates; fun_defs = _ } ~ctx ~args ~arg_types ~body ~body_type =
    let template =
      Template.create ~ctx ~args:(Nonempty.zip_exn args arg_types) ~body ~body_type
    in
    let ctx, fun_name = Context.add_value_name ctx Constant_names.fun_ in
    Hashtbl.add_exn templates ~key:fun_name ~data:template;
    ctx, fun_name
  ;;

  let call { templates; fun_defs } ~fun_name ~args ~arg_types : Expr.t =
    let template =
      option_or_default (Hashtbl.find templates fun_name) ~f:(fun () ->
        compiler_bug [%message "Function template not found" (fun_name : Unique_name.t)])
    in
    let new_fun_def = Template.instantiate template ~args ~arg_types in
    Queue.enqueue fun_defs new_fun_def;
    Fun_call (fun_name, args)
  ;;

  let pop_fun_defs { fun_defs; templates = _ } =
    let fun_defs' = Queue.to_list fun_defs in
    Queue.clear fun_defs;
    fun_defs'
  ;;
end

module Stmt = struct
  type t =
    | Value_def of Unique_name.t * Expr.t
    | Fun_def of Unique_name.t * Fun_def.t
  [@@deriving sexp_of]

  let map_names stmt ~f =
    match stmt with
    | Value_def (name, expr) ->
      let name = f name in
      let expr = Expr.map_names expr ~f in
      Value_def (name, expr)
    | Fun_def (fun_name, { closed_over; args; returns; body }) ->
      let fun_name = f fun_name in
      let closed_over = Unique_name.Set.map closed_over ~f in
      let args = Nonempty.map args ~f:(Tuple2.map_fst ~f) in
      let body = Expr.map_names body ~f in
      Fun_def (fun_name, { closed_over; args; returns; body })
  ;;
end

type t = Stmt.t list [@@deriving sexp_of]

let of_typed_module =
  let handle_let_bindings
    ~ctx
    ~stmts
    ~fun_factory
    (bindings : (Typed.Pattern.t * Typed.Expr.generalized) Node.t Nonempty.t)
    =
    let ctx =
      Nonempty.fold bindings ~init:ctx ~f:(fun ctx { node = pattern, _; _ } ->
        Pattern.Names.fold pattern ~init:ctx ~f:(fun ctx name ->
          fst (Context.add_value_name ctx name)))
    in
    (* TODO: elide toplevel let bindings which bind no names (and are pure) *)
    (* TODO: warn about eliding toplevel let bindings *)
    Nonempty.fold
      bindings
      ~init:(ctx, stmts)
      ~f:(fun (ctx, stmts) { node = pat, ((_, type_) as expr); _ } ->
      let pat' =
        Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"toplevel let bindings"
      in
      let missing_cases =
        Simple_pattern.Coverage.(of_pattern pat' |> missing_cases ~ctx ~input_type:type_)
      in
      if not (List.is_empty missing_cases)
      then
        mir_error
          [%message
            "The pattern in this let binding is not exhaustive"
              ~pattern:(pat : Typed.Pattern.t)
              (missing_cases : Simple_pattern.t list)];
      let just_bound =
        match (pat' : Simple_pattern.t) with
        | Catch_all name -> name
        | Constant _ | As _ | Cnstr_appl _ -> None
      in
      let mir_expr =
        Expr.of_typed_expr
          ?just_bound
          ~ctx
          ~add_lambda:(Function_factory.add_lambda fun_factory)
          ~call:(Function_factory.call fun_factory)
          expr
      in
      (* FIXME: cleanup *)
      (*let stmts =
        List.fold_right fun_defs ~init:stmts ~f:(fun fun_def stmts ->
          Stmt.Fun_def fun_def :: stmts)
      in*)
      let add_let stmts name mir_expr =
        match (mir_expr : Expr.t) with
        | Name name' when Unique_name.(name = name') ->
          (* Don't make a Value_def in the case where all we did is make a Fun_def *)
          stmts
        | _ -> Stmt.Value_def (name, mir_expr) :: stmts
      in
      (* TODO: Support pattern unions in toplevel let bindings - should work roughly
         the same as recursive bindings in expressions *)
      let add_name ctx name =
        (* TODO: this seems error-prone, especially since we use names like
           [Value_name.empty] elsewhere in the AST, e.g. for match variables.
           Might be a good idea to add a variant for constant names, or something. *)
        if Constant_names.mem name
        then (* Constant names are always made-up anew *)
          Context.add_value_name ctx name
        else
          (* Look up non-constant names added at the beginning *)
          ctx, Context.find_value_name ctx ([], name)
      in
      Expr.fold_pattern_bindings ~add_name ~ctx pat' mir_expr type_ ~init:stmts ~add_let)
  in
  let rec loop ~ctx ~stmts ~fun_factory (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, stmts) ~f:(fun (ctx, stmts) def ->
      match def.Node.node with
      | Let bindings -> handle_let_bindings ~ctx ~stmts ~fun_factory bindings
      | Module (module_name, _sigs, defs) ->
        Context.with_module ctx module_name ~f:(fun ctx ->
          loop ~ctx ~stmts ~fun_factory defs)
      | Trait (_, _, _, _) | Impl (_, _, _, _) -> failwith "TODO: MIR traits/impls"
      | Common_def _ -> ctx, stmts)
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    try
      let names = Name_bindings.into_module names module_name ~place:`Def in
      let _ctx, stmts =
        loop
          ~ctx:(Context.of_name_bindings names)
          ~stmts:[]
          ~fun_factory:(Function_factory.create ())
          defs
      in
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
        Hashtbl.set name_table ~key:name ~data:(Int.Table.create ());
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
