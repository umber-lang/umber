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
  type t =
    (* TODO: how on earth will Var work? *)
    | Var of Type.Param.t
    | Bool
    | Int64
    | Float64
    | Char (* Unicode scalar value: 4 bytes *)
    | String
    (* TODO: how will functions be represented? As blocks? Names cannot be given to every
       function as you can create arbitrarily many
       - also need to keep track of bound names *)
    | Closure of t Non_empty.t * t
    (* Block representation: header + fields (fields must all be 64 bits (1 word) in length)
       Header describes which fields are pointers for the GC: i32, i32 for pointers, non-pointers
       - TODO: where should constructor tags go? Get their own field?
       - This is also doesn't quite work for things like strings
       - Haskell does this, but also has a pointer to a table with info about layout
       See:
       - https://dev.realworldocaml.org/runtime-memory-layout.html
       - https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects
    *)
    (* TODO: block should include number of fields and what to put in them (?) *)
    | Block
  [@@deriving sexp_of]

  (* TODO: String Should be representable some other way - unless this means an inline string?
     Should just be like Array Char (but packed) unless optimizations based on immutability are done
     See Rust's &str and String *)

  (* TODO: something like `Block` to represent constructor application/records/tuples/arrays *)

  (*let of_primitive_type (path, type_name) =
    match path with
    | [] ->
      if Type_name.(type_name = Core.Bool.name)
      then Some Bool
      else if Type_name.(type_name = Core.Int.name)
      then Some Int64
      else if Type_name.(type_name = Core.Float.name)
      then Some Float64
      else if Type_name.(type_name = Core.Char.name)
      then Some Char
      else if Type_name.(type_name = Core.String.name)
      then Some String
      else None
    | _ :: _ -> None
  ;;

  let rec of_type_scheme ~names : Type.Scheme.t -> t = function
    | Var x -> Var x
    | Type_app (type_name, _args) ->
      option_or_default (of_primitive_type type_name) ~f:(fun () ->
        match snd (Name_bindings.find_type_decl ~defs_only:true names type_name) with
        | Alias scheme -> of_type_scheme ~names scheme
        | Variants _variants -> (* FIXME: *) Block
        | Abstract ->
          raise_s
            [%message
              "TODO: of_type_scheme: Abstract" (type_name : Type_name.Qualified.t)]
        | Record _ -> failwith "TODO: of_type_scheme: Record")
    | Tuple _fields -> Block
    | Function (arg, result) ->
      let rec loop args result =
        match (result : Type.Scheme.t) with
        | Function (arg, result) ->
          loop (Non_empty.cons (of_type_scheme ~names arg) args) result
        | _ -> Closure (Non_empty.rev args, of_type_scheme ~names result)
      in
      loop [ of_type_scheme ~names arg ] result
  ;;*)
end

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.Qualified.t -> t * Unique_name.t
  val find_value_name : t -> Value_name.Qualified.t -> Unique_name.t
  val add_empty : t -> t * Unique_name.t
  val find_empty : t -> Unique_name.t
  val cnstr_index : t -> Type.Scheme.t -> Cnstr_name.t -> int
end = struct
  type t =
    { names : Unique_name.t Ustring.Map.t
    ; name_bindings : Name_bindings.t
    }
  [@@deriving sexp_of]

  let add_value_name t name =
    let name = Value_name.Qualified.to_ustring name in
    let name' = Unique_name.of_ustring name in
    { t with names = Map.set t.names ~key:name ~data:name' }, name'
  ;;

  let empty_name = [], Value_name.empty
  let add_empty t = add_value_name t empty_name

  let find_value_name { names; _ } name =
    match Map.find names (Value_name.Qualified.to_ustring name) with
    | Some name -> name
    | None ->
      compiler_bug
        [%message "Context.find_value_name: missing" (name : Value_name.Qualified.t)]
  ;;

  let find_empty t = find_value_name t empty_name

  (* TODO: clean up *)
  (*let add_cnstr t ~names name entry =
    let rec loop t name args scheme =
      match (scheme : Type.Scheme.t) with
      | Function (arg, result) ->
        loop t name (Value_kind.of_type_scheme ~names arg :: args) result
      | Type_app _ -> { t with cnstrs = Map.add_exn t.cnstrs ~key:name ~data:args }
      | Var _ | Tuple _ ->
        compiler_bug
          [%message "Invalid cnstr" (name : Unique_name.t) (scheme : Type.Scheme.t)]
    in
    match Name_bindings.Name_entry.scheme entry with
    | Some scheme -> loop t name [] scheme
    | None -> compiler_bug [%message "Missing cnstr" (name : Unique_name.t)]
  ;;*)

  let of_name_bindings name_bindings =
    Name_bindings.fold_local_names
      name_bindings
      ~init:{ names = Ustring.Map.empty; name_bindings }
      ~f:(fun t name _entry -> fst (add_value_name t name))
  ;;

  let rec cnstr_index ({ name_bindings; _ } as t) scheme cnstr_name =
    let result =
      match (scheme : Type.Scheme.t) with
      | Type_app (type_name, _args) ->
        (match snd (Name_bindings.find_type_decl name_bindings type_name) with
        | Alias scheme -> Some (cnstr_index t scheme cnstr_name)
        | Variants variants ->
          List.findi variants ~f:(fun _ (cnstr_name', _) ->
            Cnstr_name.(cnstr_name = cnstr_name'))
          |> Option.map ~f:fst
        | Abstract | Record _ -> None)
      | Var _ | Function _ | Tuple _ -> None
    in
    option_or_default result ~f:(fun () ->
      compiler_bug
        [%message
          "Constructor index lookup failed"
            (scheme : Type.Scheme.t)
            (cnstr_name : Cnstr_name.t)])
  ;;
end

module Expr = struct
  (* TODO: probably need more bytecode-like-level instructions e.g. allocate this thing *)
  (* TODO: can put tags in the pointer to the block e.g. constructor tag,
     see https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/pointer-tagging
     GHC uses 3 bits (on 64-bit architectures) to store up to 7 constructors (0-6). 
     The highest tag value (7) indicates that the constructor tag must be looked up from
     the info table. Not sure if we want to have info tables, so in that case maybe it
     should be another field on the object. We could also do what OCaml does and take up
     some bits in the block header. *)
  type t =
    | Literal of Untyped.Literal.t
    | Name of Unique_name.t
    (* TODO: closure env *)
    | Closure of func
    | Fun_call of func * t Non_empty.t
    | Get_block_field of int * t
    (* TODO: enforce that all switch cases have the same type + support switch on blocks *)
    | Switch of t * (Untyped.Literal.t * t) Non_empty.t

  and func =
    { (*args : (Unique_name.t * Value_kind.t) Non_empty.t
    ; returns : Value_kind.t*)
      (* TODO: monomorphize functions/types per [Value_kind.t]. For now we can just box
       everything. *)
      arg_num : int
    ; body : t
    }
  [@@deriving sexp_of]

  let rec of_typed_expr ~ctx ((expr, typ) : Typed.Expr.generalized) =
    match expr, typ with
    | Literal lit, _ -> Literal lit
    | Name name, _ -> Name (Context.find_value_name ctx name)
    (* FIXME: we've discarded all but the result type - is that bad? How can we easily
       get the argument types? - can add them to the typed expr in the AST if needed *)
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
      loop (f, Function (arg_type, result_type)) (arg, arg_type) [] 0
    (* TODO: coalesce multi-argument functions *)
    | Lambda (_, body), Function (_, body_type) ->
      Closure { arg_num = 1; body = of_typed_expr ~ctx (body, body_type) }
    | Match (expr, expr_type, branches), _ ->
      (* TODO: switch statement optimization *)
      let branches =
        Non_empty.map branches ~f:(fun (pat, expr) ->
          let switch_case : Untyped.Literal.t =
            match pat with
            | Cnstr_appl ((_, cnstr_name), []) ->
              let cnstr_index = Context.cnstr_index ctx expr_type cnstr_name in
              Int cnstr_index
            | pat ->
              raise_s
                [%message
                  "TODO: unimplemented match to switch pattern" (pat : Typed.Pattern.t)]
          in
          switch_case, of_typed_expr ~ctx (expr, typ))
      in
      Switch (of_typed_expr ~ctx (expr, typ), branches)
    | Let _, _ | Tuple _, _ -> failwith "TODO: MIR let/tuple expr"
    | Record_literal _, _ | Record_update (_, _), _ | Record_field_access (_, _), _ ->
      failwith "TODO: records in MIR exprs"
    | Lambda (_, _), (Var _ | Type_app (_, _) | Tuple _) ->
      compiler_bug
        [%message "Incompatible expr and type" (expr, typ : Typed.Expr.generalized)]
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
  let underscore = Value_name.of_string_unchecked "_" in
  (* TODO: warn/error if bindings are not exhaustive *)
  let rec bind_pattern ~ctx path pattern mir_expr stmts : Context.t * Stmt.t list =
    match (pattern : Typed.Pattern.t) with
    | Catch_all name ->
      let name = Option.value name ~default:underscore in
      let ctx, name = Context.add_value_name ctx (path, name) in
      ctx, Value_def (name, mir_expr) :: stmts
    | Record _ ->
      (* Assert the result can be destructed, then destruct it, binding to the names *)
      failwith "TODO: record pattern bindings"
    | As (pattern, name) ->
      (* TODO: this isn't good as it duplicates the expression *)
      let ctx, name = Context.add_value_name ctx (path, name) in
      bind_pattern ~ctx path pattern (Name name) (Value_def (name, mir_expr) :: stmts)
    | Cnstr_appl (_, args) | Tuple args ->
      (* TODO: Need to assert that the constructor actually matched and throw an
         exception otherwise *)
      let ctx, name = Context.add_empty ctx in
      let stmts = Stmt.Value_def (name, mir_expr) :: stmts in
      List.foldi args ~init:(ctx, stmts) ~f:(fun i (ctx, stmts) arg ->
        bind_pattern ~ctx path arg (Get_block_field (i, Name name)) stmts)
    | Constant _ ->
      (* Assert equality between the result and the constant *)
      failwith "TODO: constant pattern bindings"
    | Union (_, _) ->
      (* Assert both cases can be bound to *)
      failwith "TODO: union pattern bindings"
  in
  let loop ~ctx path (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:(ctx, []) ~f:(fun (ctx, stmts) def ->
      match def.Node.node with
      | Let bindings ->
        List.fold
          bindings
          ~init:(ctx, stmts)
          ~f:(fun (ctx, stmts) { node = pattern, expr; _ } ->
          let mir_expr = Expr.of_typed_expr ~ctx expr in
          bind_pattern ~ctx path pattern mir_expr stmts)
      | Module (_, _, _) | Trait (_, _, _, _) | Impl (_, _, _, _) ->
        failwith "TODO: Ir.of_typed_module leftover cases"
      | Common_def _ -> ctx, stmts)
  in
  fun ~names ((module_name, _sigs, defs) : Typed.Module.t) ->
    let path = Name_bindings.(current_path names |> Path.to_module_path) in
    let _ctx, stmts =
      loop ~ctx:(Context.of_name_bindings names) (path @ [ module_name ]) defs
    in
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
