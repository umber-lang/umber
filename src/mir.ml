open Import
open Names

exception Mir_error of Sexp.t [@@deriving sexp]

let mir_error msg = raise (Mir_error msg)

module Value_kind = struct
  type immediate =
    [ `Int64
    | `Float64
    | `Char
      (* Unicode scalar value: 4 bytes (u32) *)
      (* TODO: I think to make my runtime representation work, all my values will have to
         be word-sized, so `Char (u32) should probably be promoted to `Int64 in basically
         all cases. I think there's little point in this existing if you can't define
         unboxed tuple/record types.*)
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
end

(* FIXME: rewrite to use `Type.Scheme` so we can do proper mapping. 
   PROBLEM: How will hashing the param_kinds map work? We'd need to restrict it to just
   the right subset of vars. Just using a concrete type seeems like it could be simpler.
   - actually, a concrete type doesn't really work. We don't want a specific type
     substitution - what we want is just the value_kind substituted. I suppose we could
     use a `(Value_kind.t, Nothing.t) Type.Expr.t`. Hmm, actually this seems quite
     reasonable.
   *)
module Monomorphic_type : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val create : scheme:Type.Scheme.t -> param_kinds:Value_kind.t Type.Param.Map.t -> t
  val scheme : t -> Type.Scheme.t
  val param_kinds : t -> Value_kind.t Type.Param.Map.t
  val to_value_kind : names:Name_bindings.t -> t -> Value_kind.t

  val of_type_alias
    :  names:Name_bindings.t
    -> parent:t
    -> alias:Type.Scheme_plain.t
    -> params:Type_param_name.t list
    -> args:Type.Scheme.t list
    -> t

  val of_concrete : Type.Concrete.t -> t
  val instantiate_child : t -> Type.Scheme.t -> t
  val instantiate_child_plain : t -> Type.Scheme_plain.t -> t

  module Param_kinds : sig
    type t = Value_kind.t Type.Param.Map.t

    include Hashable.S_plain with type t := t
  end

  val infer_param_kinds
    :  names:Name_bindings.t
    -> template_type:Type.Scheme.t
    -> instance_type:Type.Scheme.t
    -> Param_kinds.t
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
          of_type_alias ~names ~parent:t ~alias ~params:(fst decl) ~args
          |> to_value_kind ~names
        | Variants _ | Record _ -> `Block
        | Abstract ->
          (* TODO: We could allow abstract types in defs if the [Value_kind] was specified
             in an annotation. We could also have a better error message *)
          mir_error
            [%message
              "Type definition in def is abstract (missing a definition)"
                (decl : Type.Decl.t)])
    | Tuple _ | Function _ | Partial_function _ -> `Block
    | Var param ->
      option_or_default (Map.find param_kinds param) ~f:(fun () ->
        compiler_bug
          [%message
            "Missing var in monomorphic type param_kinds" (param : Type.Param.t) (t : t)])

  and of_type_alias ~names ~parent ~alias ~params ~args =
    let env = Type.Param.Env_to_vars.create () in
    let param_kinds =
      List.fold2_exn
        params
        args
        ~init:Type.Param.Map.empty
        ~f:(fun param_kinds param_name arg ->
        let arg_kind =
          to_value_kind ~names { param_kinds = parent.param_kinds; scheme = arg }
        in
        Map.set param_kinds ~key:(Type.Param.of_name ~env param_name) ~data:arg_kind)
    in
    { scheme = Type.Scheme.of_plain ~env alias; param_kinds }
  ;;

  (* TODO: remove? Is this used/needed? *)
  let of_concrete concrete =
    { scheme = Type.Concrete.cast concrete; param_kinds = Type.Param.Map.empty }
  ;;

  let filter_to_used_params scheme param_kinds ~get_name =
    (* Filter the map down to just the used params to get correct caching behavior *)
    Map.filter_keys param_kinds ~f:(fun param ->
      Type.Expr.exists_var scheme ~f:(fun v ->
        Type_param_name.equal (get_name v) (Type.Param.name param)))
  ;;

  let instantiate_child { scheme = _; param_kinds } scheme =
    { scheme
    ; param_kinds = filter_to_used_params scheme param_kinds ~get_name:Type.Param.name
    }
  ;;

  let instantiate_child_plain { scheme = _; param_kinds } scheme =
    let params_by_name = Type_param_name.Table.create () in
    Map.iter_keys param_kinds ~f:(fun param ->
      Hashtbl.set params_by_name ~key:(Type.Param.name param) ~data:param);
    { scheme =
        Type.Expr.map
          scheme
          ~var:(Hashtbl.find_exn params_by_name)
          ~pf:Nothing.unreachable_code
    ; param_kinds = filter_to_used_params scheme param_kinds ~get_name:Fn.id
    }
  ;;

  module Param_kinds = struct
    module T = struct
      type t = Value_kind.t Type.Param.Map.t [@@deriving compare, hash, sexp_of]
    end

    include T
    include Hashable.Make_plain (T)
  end

  let infer_param_kinds =
    let add_consistent ~names param_kinds ~param ~instance_type =
      let new_kind =
        create ~scheme:instance_type ~param_kinds:Type.Param.Map.empty
        |> to_value_kind ~names
      in
      Map.update param_kinds param ~f:(function
        | None -> new_kind
        | Some existing_kind ->
          if Value_kind.equal existing_kind new_kind
          then existing_kind
          else
            compiler_bug
              [%message
                "Inconsistent type instantiation"
                  (existing_kind : Value_kind.t)
                  (new_kind : Value_kind.t)])
    in
    let rec loop ~names param_kinds template_type instance_type =
      match (template_type : Type.Scheme.t), (instance_type : Type.Scheme.t) with
      | Var param, _ -> add_consistent ~names param_kinds ~param ~instance_type
      | Type_app (type_name, args), Type_app (type_name', args') ->
        assert_or_compiler_bug
          (Type_name.Qualified.equal type_name type_name')
          ~here:[%here];
        List.fold2_exn args args' ~init:param_kinds ~f:(loop ~names)
      | Tuple fields, Tuple fields' ->
        List.fold2_exn fields fields' ~init:param_kinds ~f:(loop ~names)
      | Function (args, body), Function (args', body') ->
        let param_kinds =
          Nonempty.fold2_exn args args' ~init:param_kinds ~f:(loop ~names)
        in
        loop ~names param_kinds body body'
      | (Type_app _ | Tuple _ | Function _), _ ->
        compiler_bug
          [%message
            "infer_param_map: incompatible types"
              (template_type : Type.Scheme.t)
              (instance_type : Type.Scheme.t)]
      | Partial_function _, _ -> .
    in
    fun ~names ~template_type ~instance_type ->
      loop ~names Type.Param.Map.empty template_type instance_type
  ;;

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

    let make_arg_list ~instantiate_child ~names type_ args =
      List.fold_map args ~init:(0, 0) ~f:(fun (pointer_i, immediate_i) arg ->
        let arg = instantiate_child type_ arg in
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
          else (
            let instantiate_child = Monomorphic_type.instantiate_child_plain in
            let args = make_arg_list ~instantiate_child ~names type_ args in
            constant_cnstrs, (cnstr_name, args) :: non_constant_cnstrs))
      in
      Variants
        { constant_cnstrs = List.rev constant_cnstrs
        ; non_constant_cnstrs = List.rev non_constant_cnstrs
        }
    ;;

    let of_tuple ~names type_ args =
      let instantiate_child = Monomorphic_type.instantiate_child in
      Tuple (make_arg_list ~names ~instantiate_child type_ args)
    ;;
  end

  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t [@sexp.opaque]
    ; cnstr_info_cache : Cnstr_info.t Monomorphic_type.Table.t
    ; add_observer : Unique_name.t -> unit [@sexp.opaque]
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
      | Alias alias -> get_cnstr_info_polymorphic_uncached t (Type.Scheme.of_plain alias)
      | Variants variants ->
        let env = Type.Param.Env_to_vars.create () in
        `Variants
          (List.map
             variants
             ~f:(Tuple2.map_snd ~f:(List.map ~f:(Type.Scheme.of_plain ~env))))
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
      List.fold_until patterns ~init:(of_pattern pattern) ~f:(fun coverage pattern ->
        match add_pattern coverage pattern with
        | Exhaustive -> Stop Exhaustive
        | coverage -> Continue coverage)
      |> Fold_action.finish ~f:Fn.id
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

let rec type_get_function ~names typ =
  match (typ : Type.Scheme.t) with
  | Function (args, result) -> Some (args, result)
  | Var _ | Tuple _ -> None
  | Type_app (type_name, _) ->
    (match snd (Name_bindings.find_type_decl ~defs_only:true names type_name) with
    | Alias alias -> type_get_function ~names (Type.Scheme.of_plain alias)
    | Variants _ | Record _ | Abstract -> None)
  | Partial_function _ -> .
;;

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
  let rec fold_pattern_bindings
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
      List.foldi args ~init:(ctx, acc) ~f:(fun i (ctx, acc) arg ->
        let arg_index, arg_type =
          Context.cnstr_arg_info ctx type_ cnstr (Block_index.raw i)
        in
        let arg_expr = Get_block_field (arg_index, Name name) in
        fold_pattern_bindings ~ctx ~init:acc ~add_let arg arg_expr arg_type)
    | Constant _ -> ctx, acc
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
            let arg_index, arg_type =
              Context.cnstr_arg_info ctx input_type cnstr (Block_index.raw i)
            in
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

  let of_typed_expr
    ?just_bound:outer_just_bound
    ~ctx:outer_ctx
    ~add_lambda
    ~call
    outer_expr
    outer_type
    =
    let create_break_label = unstage (Break_label.make_creator_for_whole_expression ()) in
    (* FIXME: remove just_bound *)
    (* FIXME: We can use a different type - all exprs should be monomorphic types, yeah? 
       - roughly, there could be some missing params e.g. `a` from `Option a` in `None`
       - we should be able to detect this somehow - I guess if it doesn't appear bound by
         the monomorphic type, this is just fine. We should not require that all params
         are bound, then. I think this should probably just work as-is, since we will not
         look at `a` when doing `to_value_kind` on `Option a` *)
    let rec of_typed_expr ?just_bound:_ ~ctx expr expr_type =
      match (expr : Type.Scheme.t Typed.Expr.t), Monomorphic_type.scheme expr_type with
      | Literal lit, _ -> Primitive lit
      | Name name, _ -> Name (Context.find_value_name ctx name)
      | Fun_call (fun_, args_and_types), body_type ->
        let fun_call () =
          let arg_types = Nonempty.map ~f:snd args_and_types in
          let fun_type =
            Monomorphic_type.instantiate_child expr_type (Function (arg_types, body_type))
          in
          let fun_ = of_typed_expr ~ctx fun_ fun_type in
          let args =
            Nonempty.map args_and_types ~f:(fun (arg, arg_type) ->
              of_typed_expr
                ~ctx
                arg
                (Monomorphic_type.instantiate_child expr_type arg_type))
          in
          (* Special-case constructor applications *)
          match fun_ with
          | Name fun_name ->
            call ~fun_name ~arg_types ~body_type;
            Fun_call (fun_name, args)
          | Let _ | Fun_call _ | Get_block_field _ | If _ | Catch _ | Break _ ->
            (* FIXME: does this make any sense? I suppose this is meant to hit the last
             defined function or something? *)
            let _, fun_name = Context.add_value_name ctx Constant_names.fun_ in
            call ~fun_name ~arg_types ~body_type;
            Let (fun_name, fun_, Fun_call (fun_name, args))
          | Primitive _ | Make_block _ ->
            compiler_bug [%message "Invalid function expression" (fun_ : t)]
        in
        (match fun_ with
        | Name (_, name) ->
          (match Value_name.to_cnstr_name name with
          | Ok cnstr_name ->
            let tag = Context.cnstr_tag ctx expr_type (Named cnstr_name) in
            let fields, field_types = List.unzip (Nonempty.to_list args_and_types) in
            make_block ~ctx ~expr_type ~tag ~fields ~field_types
          | Error _ -> fun_call ())
        | _ -> fun_call ())
      | Lambda (args, body), Function (arg_types, body_type) ->
        (* TODO: Still need to try and coalesce lambdas/other function expressions for
           function definitions which are partially applied. See example in
           test/ast/TypeChecking.expected. *)
        let param_kinds = Monomorphic_type.param_kinds expr_type in
        Name (snd (add_lambda ~ctx ~args ~arg_types ~body ~body_type ~param_kinds))
      | Match (expr, input_type, arms), output_type ->
        let output_type = Monomorphic_type.instantiate_child expr_type output_type in
        let input_type = Monomorphic_type.instantiate_child expr_type input_type in
        let input_expr = of_typed_expr ~ctx expr input_type in
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
        then
          (* TODO: implement let rec in MIR. I think the only tricky part is doing all the
             Context calls to create unique names up-front, and then keeping track of
             these names somehow instead of having them be shadowed. Actually, didn't we
             already write this code to handle toplevel rec bindings? Should be shared. *)
          failwith "TODO: let rec in MIR expr"
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
              let typ' = Monomorphic_type.instantiate_child expr_type typ in
              let mir_expr = of_typed_expr ?just_bound ~ctx expr typ' in
              let add_let bindings name mir_expr = (name, mir_expr) :: bindings in
              fold_pattern_bindings ~ctx pat mir_expr typ' ~init:bindings ~add_let)
          in
          let body =
            of_typed_expr
              ~ctx
              body
              (Monomorphic_type.instantiate_child expr_type body_type)
          in
          List.fold bindings ~init:body ~f:(fun body (name, mir_expr) ->
            Let (name, mir_expr, body)))
      | Tuple fields, Tuple field_types ->
        make_block ~ctx ~expr_type ~tag:Cnstr.Tag.default ~fields ~field_types
      | Record_literal _, _ | Record_update _, _ | Record_field_access _, _ ->
        failwith "TODO: records in MIR exprs"
      | ( Lambda _, (Var _ | Type_app _ | Tuple _)
        | Tuple _, (Var _ | Type_app _ | Function _) ) as expr ->
        compiler_bug
          [%message "Incompatible expr and type" (expr : Typed.Expr.generalized)]
      | _, Partial_function _ -> .
    and make_block ~ctx ~expr_type ~tag ~fields ~field_types =
      (* TODO: isn't this duplicating code from `Context.Arg_info`? *)
      let pointers, immediates =
        List.fold2_exn
          fields
          field_types
          ~init:([], [])
          ~f:(fun (pointers, immediates) field typ ->
          let typ = Monomorphic_type.instantiate_child expr_type typ in
          let field = of_typed_expr ~ctx field typ in
          if Value_kind.is_pointer
               (Monomorphic_type.to_value_kind typ ~names:(Context.name_bindings ctx))
          then field :: pointers, immediates
          else pointers, field :: immediates)
      in
      Make_block { tag; pointers = List.rev pointers; immediates = List.rev immediates }
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
          let input_type = Monomorphic_type.scheme input_type in
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
      Catch { label; body; with_ = of_typed_expr ~ctx output_expr output_type }
    in
    of_typed_expr ?just_bound:outer_just_bound ~ctx:outer_ctx outer_expr outer_type
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
  (* TODO: If all uses of this end up including the fun_name, just put it in here *)
  (* TODO: should we resolve closures in this pass? Split immediate/pointers and just
     make it a block argument? Closure environments will have to GC'd like everything
     else, so it makes sense. That does mean they're affected by monomorphization, though.
     (The Value_kinds matter and need to be stored here.) *)
  type t =
    { closed_over : Unique_name.Set.t
    ; args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end

module Function_factory : sig
  type t

  val create : unit -> t

  val define_fun
    :  t
    -> ctx:Context.t
    -> arg_types:Type.Scheme.t Nonempty.t
    -> body_type:Type.Scheme.t
    -> definition:Type.Scheme.t Typed.Expr.t
    -> Context.t * Unique_name.t

  val add_lambda
    :  t
    -> ctx:Context.t
    -> args:Typed.Pattern.t Nonempty.t
    -> arg_types:Type.Scheme.t Nonempty.t
    -> body:Type.Scheme.t Typed.Expr.t
    -> body_type:Type.Scheme.t
    -> param_kinds:Monomorphic_type.Param_kinds.t
    -> Context.t * Unique_name.t

  val call
    :  t
    -> fun_name:Unique_name.t
    -> arg_types:Type.Scheme.t Nonempty.t
    -> body_type:Type.Scheme.t
    -> unit

  val pop_fun_defs : t -> (Unique_name.t * Fun_def.t) list
end = struct
  module Template = struct
    type t =
      { ctx : Context.t
      ; args : (Typed.Pattern.t * Type.Scheme.t) Nonempty.t
      ; body : Type.Scheme.t Typed.Expr.t
      ; body_type : Type.Scheme.t
      ; instances : (Unique_name.t * Fun_def.t) Monomorphic_type.Param_kinds.Table.t
      }

    let create ~ctx ~args ~body ~body_type =
      let instances = Monomorphic_type.Param_kinds.Table.create () in
      { ctx; args; body; body_type; instances }
    ;;

    let create_fun_def ~ctx ~args ~body ~body_type ~add_lambda ~call =
      (* TODO: Could we just do this ourselves directly, instead of using add_observer? *)
      (* Record all of the names bound in the function arguments *)
      let bound_names = Unique_name.Hash_set.create () in
      let ctx = Context.with_add_observer ctx ~f:(Hash_set.add bound_names) in
      let (ctx, bindings), args =
        Nonempty.fold_map args ~init:(ctx, []) ~f:(fun (ctx, bindings) (arg, arg_type) ->
          let arg =
            Simple_pattern.flatten_typed_pattern_no_unions
              arg
              ~label:"function argument patterns"
          in
          let ctx, bindings, arg_name =
            match arg with
            | Catch_all (Some arg_name) ->
              (* Special-case named catch-all patterns (the dominant case) to skip the
                 [lambda_arg] step and use the name directly. *)
              let ctx, arg_name = Context.add_value_name ctx arg_name in
              ctx, bindings, arg_name
            | Catch_all None | Constant _ | As _ | Cnstr_appl _ ->
              let ctx, arg_name = Context.add_value_name ctx Constant_names.lambda_arg in
              let add_let acc name mir_expr = (name, mir_expr) :: acc in
              let ctx, bindings =
                Expr.fold_pattern_bindings
                  ~ctx
                  arg
                  (Name arg_name)
                  arg_type
                  ~init:bindings
                  ~add_let
              in
              ctx, bindings, arg_name
          in
          let arg =
            ( arg_name
            , Monomorphic_type.to_value_kind arg_type ~names:(Context.name_bindings ctx) )
          in
          (ctx, bindings), arg)
      in
      let closed_over = ref Unique_name.Set.empty in
      let ctx =
        (* Determine if names looked up were bound as function args or closed over *)
        Context.with_find_observer ctx ~f:(fun parent_observer name ->
          if not (Hash_set.mem bound_names name)
          then (
            parent_observer name;
            closed_over := Set.add !closed_over name))
      in
      let body = Expr.of_typed_expr ~ctx ~add_lambda ~call body body_type in
      let body =
        List.fold_right bindings ~init:body ~f:(fun (name, mir_expr) body ->
          Expr.Let (name, mir_expr, body))
      in
      let returns =
        Monomorphic_type.to_value_kind body_type ~names:(Context.name_bindings ctx)
      in
      (* TODO: see if we can get rid of `just_bound`? Responsibility can move to Context
         or whatever does function definitions *)
      (* FIXME: Can we just leave the fun_name out of fun_def? *)
      (*let fun_name =
        match just_bound with
        | Some name -> Context.find_value_name ctx ([], name)
        | None -> snd (Context.add_value_name ctx Constant_names.fun_)
      in*)
      { Fun_def.closed_over = !closed_over; args; returns; body }
    ;;

    let instantiate
      { ctx; args; body; body_type; instances }
      ~param_kinds
      ~add_lambda
      ~call
      =
      Hashtbl.find_or_add instances param_kinds ~default:(fun () ->
        let args =
          Nonempty.map args ~f:(fun (pat, scheme) ->
            pat, Monomorphic_type.create ~scheme ~param_kinds)
        in
        let body_type = Monomorphic_type.create ~scheme:body_type ~param_kinds in
        let fun_def = create_fun_def ~ctx ~args ~body ~body_type ~add_lambda ~call in
        let fun_name =
          Unique_name.of_ustring (Value_name.to_ustring Constant_names.fun_)
        in
        fun_name, fun_def)
    ;;
  end

  type t =
    { templates : Template.t Unique_name.Table.t
    ; fun_defs : (Unique_name.t * Fun_def.t) Queue.t
    }

  let create () = { templates = Unique_name.Table.create (); fun_defs = Queue.create () }

  let define_fun { templates; _ } ~ctx ~arg_types ~body_type ~definition =
    let args, body =
      match (definition : _ Typed.Expr.t) with
      | Lambda (args, body) ->
        (* TODO: What if there are extra args (i.e. the lambda returns a function?) *)
        args, body
      | _ ->
        let synthetic_arg_patterns, synthetic_arg_exprs =
          Nonempty.mapi arg_types ~f:(fun i typ ->
            let name = Constant_names.synthetic_arg i in
            Pattern.Catch_all (Some name), (Typed.Expr.Name ([], name), typ))
          |> Nonempty.unzip
        in
        synthetic_arg_patterns, Fun_call (definition, synthetic_arg_exprs)
    in
    let template =
      Template.create ~ctx ~args:(Nonempty.zip_exn args arg_types) ~body ~body_type
    in
    let ctx, fun_name = Context.add_value_name ctx Constant_names.fun_ in
    Hashtbl.add_exn templates ~key:fun_name ~data:template;
    ctx, fun_name
  ;;

  let rec add_lambda t ~ctx ~args ~arg_types ~body ~body_type ~param_kinds =
    let template =
      Template.create ~ctx ~args:(Nonempty.zip_exn args arg_types) ~body ~body_type
    in
    let fun_name, _ =
      Template.instantiate template ~param_kinds ~add_lambda:(add_lambda t) ~call:(call t)
    in
    Hashtbl.add_exn t.templates ~key:fun_name ~data:template;
    ctx, fun_name

  and call t ~fun_name ~arg_types ~body_type =
    let template =
      option_or_default (Hashtbl.find t.templates fun_name) ~f:(fun () ->
        compiler_bug [%message "Function template not found" (fun_name : Unique_name.t)])
    in
    let param_kinds =
      Monomorphic_type.infer_param_kinds
        ~names:(Context.name_bindings template.ctx)
        ~template_type:(Function (Nonempty.map ~f:snd template.args, template.body_type))
        ~instance_type:(Function (arg_types, body_type))
    in
    let fun_def =
      Template.instantiate template ~param_kinds ~add_lambda:(add_lambda t) ~call:(call t)
    in
    Queue.enqueue t.fun_defs fun_def
  ;;

  let pop_fun_defs { fun_defs; templates = _ } =
    let fun_defs' = Queue.to_list fun_defs in
    Queue.clear fun_defs;
    fun_defs'
  ;;
end

module Stmt = struct
  (* FIXME: how should I represent polymorphic function definitions/templates vs 
     specific function instantiations? *)
  type t =
    | Value_def of Unique_name.t * Expr.t
    | Fun_def of Unique_name.t * Fun_def.t
  [@@deriving sexp_of, variants]

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

(* FIXME: may need to go back and generate more code in a module after finishing it due to
   new monomorphic instances being used in some other module. Damn, all this work just to
   have 64-bit ints/floats? (And I guess custom unboxed types.) Can consider breaking a
   compiled module into values + monomorphic functions vs polymorphic functions.
   [Mir.t] could be the list of toplevel values (and all possible side effects/toplevel
   evaluation), and a list of function instances. The function factory can exist
   separately, similary to how Name_bindings.t and the typed AST work. (If we later need
   to, we can serialize the function factory to disk to share its state between
   processes/cache compilation. This would be separate to the main code targets as
   polymorphic code isn't Mir yet so isn't going to LLVM.)

   Can look at: https://rustc-dev-guide.rust-lang.org/backend/monomorph.html *)
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
    (* TODO: Warn about toplevel let bindings which bind no names and are pure. *)
    Nonempty.fold
      bindings
      ~init:(ctx, stmts)
      ~f:(fun (ctx, stmts) { node = pat, (expr, typ); _ } ->
      let pat' =
        Simple_pattern.flatten_typed_pattern_no_unions pat ~label:"toplevel let bindings"
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
      let just_bound =
        match (pat' : Simple_pattern.t) with
        | Catch_all name -> name
        | Constant _ | As _ | Cnstr_appl _ -> None
      in
      let pop_fun_defs fun_factory =
        Function_factory.pop_fun_defs fun_factory
        |> List.map ~f:(Tuple2.uncurry Stmt.fun_def)
      in
      (* FIXME: where do we monomorphize the type? Should be here I guess. 
         Need to decide if the thing is:
         - A monomorphic value. Just do it.
         - A polymorphic value. Also just do it. Should be able to make a monomorphic type
           that happens to contain some unused type variables. (e.g. `a` in `None`)
         - A monomorphic/polymorphic function definition. Add it to fun factory.
           The polymorphic code will just be correct for monomorphic types.
           
         Summary: Non-function values are always monomorphic. Functions can be treated as
         always polymorphic. *)
      (* TODO: should share code between toplevel let bindings and recursive let bindings *)
      match type_get_function ~names:(Context.name_bindings ctx) typ with
      | Some (arg_types, body_type) ->
        (* FIXME: support not adding a new name and instead using one already bound
           Similar to [just_bound]. I guess I can just keep using [just_bound] actually,
           it seems less hacky maybe? *)
        let ctx, _ =
          Function_factory.define_fun
            fun_factory
            ~ctx
            ~arg_types
            ~body_type
            ~definition:expr
        in
        ctx, pop_fun_defs fun_factory @ stmts
      | None ->
        let typ = Monomorphic_type.create ~scheme:typ ~param_kinds:Type.Param.Map.empty in
        let mir_expr =
          Expr.of_typed_expr
            ?just_bound
            ~ctx
            ~add_lambda:(Function_factory.add_lambda fun_factory)
            ~call:(Function_factory.call fun_factory)
            expr
            typ
        in
        let stmts = pop_fun_defs fun_factory @ stmts in
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
        Expr.fold_pattern_bindings ~add_name ~ctx pat' mir_expr typ ~init:stmts ~add_let)
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
