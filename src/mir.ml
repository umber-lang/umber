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
          loop (Nonempty.cons (of_type_scheme ~names arg) args) result
        | _ -> Closure (Nonempty.rev args, of_type_scheme ~names result)
      in
      loop [ of_type_scheme ~names arg ] result
  ;;*)
end

module Context : sig
  type t [@@deriving sexp_of]

  val of_name_bindings : Name_bindings.t -> t
  val add_value_name : t -> Value_name.t -> t * Unique_name.t
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
    let t = { names = Ustring.Map.empty; name_bindings } in
    Name_bindings.fold_local_names name_bindings ~init:t ~f:(fun t name _entry ->
      fst (add t name))
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
  type t =
    | Literal of Untyped.Literal.t
    | Name of Unique_name.t
    (* TODO: recursive lets? Mutual recursion? *)
    | Let of Unique_name.t * t * t
    (* TODO: closure env - pass a pointer to a struct with all the free variables *)
    | Closure of func
    | Fun_call of func * t Nonempty.t
    | Make_block of t list
    | Get_block_field of int * t
    | If of t * t * t
    (* TODO: enforce that all switch cases have the same type + support switch on blocks
       - actually I'm not sure if this makes sense - variants will have different
         Value_kinds e.g. Int64 vs Block *)
    | Switch of
        { expr : t
        ; cases : (Untyped.Literal.t Nonempty.t * t) list
        ; default : t option
        }

  and func =
    { (*args : (Unique_name.t * Value_kind.t) Nonempty.t
    ; returns : Value_kind.t*)
      (* TODO: monomorphize functions/types per [Value_kind.t]. For now we can just box
       everything. *)
      arg_num : int
    ; body : t
    }
  [@@deriving sexp_of]

  let rec fold_let_pattern ~ctx ~init:acc ~add_let pattern (mir_expr : t) =
    let underscore = Value_name.of_string_unchecked "_" in
    (* TODO: warn/error if bindings are not exhaustive *)
    match (pattern : Typed.Pattern.t) with
    | Catch_all name ->
      let name = Option.value name ~default:underscore in
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
      failwith "TODO: constant pattern bindings"
    | Union (_, _) ->
      (* Assert both cases can be bound to *)
      failwith "TODO: union pattern bindings"
    | Record _ ->
      (* Assert the result can be destructed, then destruct it, binding to the names *)
      failwith "TODO: record pattern bindings"
  ;;

  let rec of_typed_expr ~ctx : Typed.Expr.generalized -> t = function
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
          let func = { arg_num = arg_num + 1; body = of_typed_expr ~ctx (f, f_type) } in
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
    | Match (expr, expr_type, arms), match_type ->
      (* TODO: switch statement optimization *)
      let rec switch_case : Typed.Pattern.t -> Untyped.Literal.t list = function
        | Cnstr_appl ((_, cnstr_name), []) ->
          let cnstr_index = Context.cnstr_index ctx expr_type cnstr_name in
          [ Int cnstr_index ]
        | Catch_all _ -> []
        | As (pattern, _name) -> switch_case pattern
        | (Cnstr_appl _ | Constant _ | Tuple _ | Record _ | Union (_, _)) as pat ->
          raise_s
            [%message
              "TODO: unimplemented match to switch pattern" (pat : Typed.Pattern.t)]
      in
      (* TODO: Warn about unused cases *)
      let cases, default =
        Nonempty.fold_right arms ~init:([], None) ~f:(fun (pat, expr) (cases, default) ->
          if Option.is_some default
          then cases, default
          else (
            (* FIXME: need to bind variable names from the switch case when making this *)
            let expr = of_typed_expr ~ctx (expr, match_type) in
            match switch_case pat with
            | [] -> cases, Some expr
            | case :: rest -> (Nonempty.(case :: rest), expr) :: cases, default))
      in
      Switch { expr = of_typed_expr ~ctx (expr, match_type); cases; default }
    | Let { rec_; bindings; body }, body_type ->
      (* TODO: let statements in expressions should be able to be made into global statements
         (e.g. to define static functions/values) - not all lets should be global though e.g.
         for simple expressions like `let y = x + x; (y, y)` *)
      if rec_
      then failwith "TODO: let rec in MIR expr"
      else (
        let _ctx, acc =
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
