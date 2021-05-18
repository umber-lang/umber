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

  let dash = Ustring.of_string_exn "-"

  let of_ustring ustr =
    Ustring.concat [ ustr; dash; Ustring.of_string_exn (Id.to_string (Id.create ())) ]
  ;;
end

module Prim_op = struct
  type t =
    | Int_add (* TODO: arguments? *)
    | Io_print_string
end

module Value_kind = struct
  type t =
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
    | Block of t list

  (* TODO: String Should be representable some other way - unless this means an inline string?
     Should just be like Array Char (but packed) unless optimizations based on immutability are done
     See Rust's &str and String *)

  (* TODO: something like `Block` to represent constructor application/records/tuples/arrays *)

  let rec of_type_scheme : Type.Scheme.t -> t = function
    | Var x -> Var x
    | Type_app _ ->
      (* FIXME: need to lookup the type name to figure out what this is *)
      failwith "TODO: type_app"
    | Tuple fields -> Block (List.map ~f:of_type_scheme fields)
    | Function (arg, result) ->
      let rec loop args result =
        match (result : Type.Scheme.t) with
        | Function (arg, result) -> loop (Non_empty.cons (of_type_scheme arg) args) result
        | _ -> Closure (Non_empty.rev args, of_type_scheme result)
      in
      loop [ of_type_scheme arg ] result
  ;;
end

module Cnstr = struct
  type t = Value_kind.t list
end

module Context : sig
  type t

  val of_name_bindings : Name_bindings.t -> t
  val find_value_name : t -> Value_name.Qualified.t -> Unique_name.t
  val add_value_name : t -> Value_name.Qualified.t -> t
end = struct
  type t =
    { names : Unique_name.t Ustring.Map.t
    ; cnstrs : Cnstr.t Unique_name.Map.t
    }

  let empty = { names = Ustring.Map.empty; cnstrs = Unique_name.Map.empty }

  let find_value_name { names; _ } name =
    Map.find_exn names (Value_name.Qualified.to_ustring name)
  ;;

  let add_value_name t name =
    let name = Value_name.Qualified.to_ustring name in
    let name' = Unique_name.of_ustring name in
    { t with names = Map.add_exn t.names ~key:name ~data:name' }, name'
  ;;

  let add_cnstr t name entry =
    match Name_bindings.Name_entry.scheme entry with
    | Some (Type_app (_, args)) ->
      let args = List.map args ~f:Value_kind.of_type_scheme in
      { t with cnstrs = Map.add_exn t.cnstrs ~key:name ~data:args }
    | None | Some _ -> compiler_bug [%message "Invalid cnstr" (name : Unique_name.t)]
  ;;

  let of_name_bindings =
    Name_bindings.fold_local_names ~init:empty ~f:(fun t (path, name) entry ->
      let t, name' = add_value_name t (path, name) in
      if Value_name.is_cnstr_name name then add_cnstr t name' entry else t)
  ;;

  let add_value_name t name = fst (add_value_name t name)
end

module Expr = struct
  type t =
    | Literal of Untyped.Literal.t
    | Name of Unique_name.t
    | Fun_call of Unique_name.t * t Non_empty.t
    (* TODO: args for prim_ops - they're just functions, right?
       We do at least need a variant for external functions, which is probably what
       these should be, although having some operations be completely built into the
       compiler seems reasonable *)
    | Prim_op of Prim_op.t

  (* TODO: switch (to implement match) *)

  let rec of_typed_expr ~ctx = function
    | Typed.Expr.Literal lit -> Literal lit
    | Name name -> Name (Context.find_value_name ctx name)
    | Fun_call (f, arg) ->
      let rec loop f arg args =
        match f with
        | Typed.Expr.Name fun_name ->
          let fun_name = Context.find_value_name ctx fun_name in
          Fun_call (fun_name, of_typed_expr ~ctx arg :: args)
        | Fun_call (f', arg') -> loop f' arg' (of_typed_expr ~ctx arg :: args)
        | Lambda _ | Match _ | Let _ ->
          (* TODO: dynamic function application *)
          raise_s
            [%message
              "Unimplemented function expression" (f : Type.Scheme.t Typed.Expr.t)]
        | Literal _ | Tuple _ | Record_literal _ | Record_update _ | Record_field_access _
          ->
          (* TODO: should be able to find where this is from a Node.t or something *)
          mir_error
            [%message
              "This is not a function; it cannot be applied"
                (f : Type.Scheme.t Typed.Expr.t)]
      in
      loop f arg []
    (* TODO: lambdas need to generate function definitions and update the context *)
    | Lambda _ | Match (_, _) | Let _ | Tuple _ -> failwith "TODO: MIR expr cases"
    | Record_literal _ | Record_update (_, _) | Record_field_access (_, _) ->
      failwith "TODO: records in MIR"
  ;;
end

module Function = struct
  type t =
    { args : (Unique_name.t * Value_kind.t) Non_empty.t
    ; returns : Value_kind.t
    ; body : Expr.t
    }
end

module Stmt = struct
  (* TODO: closures require creating functions on the fly
     - I suppose we can model this by creating a struct/record with all the used
       parameters and passing that in to a global function *)
  type t =
    | Value_def of Unique_name.t * Expr.t
    | Fun_def of Unique_name.t * Function.t

  (* TODO: Functions can be computed at runtime e.g.
     `let f = if true then fun x -> x * 2 else fun x -> x * 3`
     so functions sometimes just have to be runtime-constructed values anyway *)
end

(* TODO: need to handle ordering? function definitions can have side effects *)
type t = Stmt.t list

let of_typed_module =
  (* TODO: have to deal with names within let bindings, which may be re-used while
     being different *)
  let underscore = Value_name.of_string_unchecked "_" in
  let bind_name ~ctx path name mir_expr stmts =
    let name = Context.find_value_name ctx (path, name) in
    Stmt.Value_def (name, mir_expr) :: stmts
  in
  let rec bind_pattern ~ctx path pattern mir_expr stmts =
    match (pattern : Typed.Pattern.t) with
    | Catch_all name ->
      let name = Option.value name ~default:underscore in
      bind_name ~ctx path name mir_expr stmts
    | Tuple _ | Record _ ->
      (* TODO: tuples, etc. should create the thing, then call getters to get all
      the names out *)
      (* Assert the result can be destructed, then destruct it, binding to the names *)
      failwith "TODO: tuple/record pattern bindings"
    | As (pattern, name) ->
      bind_pattern ~ctx path pattern mir_expr stmts |> bind_name ~ctx path name mir_expr
    | Constant _ ->
      (* Assert equality between the result and the constant *)
      failwith "TODO: constant pattern bindings"
    | Cnstr_appl ((_, _), _) ->
      (* Similar to constant bindings *) failwith "TODO: cnstr pattern bindings"
    | Union (_, _) ->
      (* Assert both cases can be bound to *)
      failwith "TODO: union pattern bindings"
  in
  let loop ~ctx path (defs : Typed.Module.def Node.t list) =
    List.fold defs ~init:[] ~f:(fun stmts def ->
      match def.Node.node with
      | Let bindings ->
        List.fold bindings ~init:stmts ~f:(fun stmts { node = pattern, (expr, _); _ } ->
          let mir_expr = Expr.of_typed_expr ~ctx expr in
          bind_pattern ~ctx path pattern mir_expr stmts)
      | Module (_, _, _) | Trait (_, _, _, _) | Impl (_, _, _, _) ->
        failwith "TODO: Ir.of_typed_module leftover cases"
      | Common_def _ -> stmts)
  in
  fun ~names ~path ((module_name, _sigs, defs) : Typed.Module.t) ->
    loop ~ctx:(Context.of_name_bindings names) (path @ [ module_name ]) defs |> List.rev
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
