open Import
open Names

(* TODO: interning? also, need some way of specifying that names are the same 
   - hash the Name + Name_entry ? *)
module Unique_name : sig
  include General_name

  (* FIXME: should really be of a qualified name - need to map over name_bindings and 
     give every entry a unique_name - should probably map from name_entry ID -
     use unique_id instead of phys_equal 
     
     three parts: Name_bindings, Name_entry.Id -> Unique_name, Unique_name -> LLVM stuff 
     unique IDs can also help name_bindings round-trip, which breaks phys_equal - actually
     it doesn't, because we really do want to say that looking up led us to the same place 
     
     - Actually, the absolute qualified path should be a unique identifier - convert from that *)

  val of_ustring : Ustring.t -> t
  val of_value_name : Value_name.t -> t
end = struct
  include Ustring
  module Id = Unique_id.Int ()

  let of_ustring ustr = ustr ^ Ustring.of_string_exn (Id.to_string (Id.create ()))
  let of_value_name = of_ustring << Value_name.to_ustring
end

module Prim_op = struct
  type t =
    | Int_add (* TODO: arguments? *)
    | Io_print_string
end

module Value_kind = struct
  type t =
    | Bool
    | Int64
    | Float64
    | Char (* Unicode scalar value - so unsigned 32 *)
    | String

  (* TODO: String Should be representable some other way - unless this means an inline string?
     Should just be like Array Char (but packed) unless optimizations based on immutability are done
     See Rust's &str and String *)

  (* TODO: something like `Block` to represent constructor application/records/tuples/arrays *)
end

module Expr = struct
  type t =
    | Literal of Untyped.Literal.t
    | Name of Unique_name.t
    | Fun_call of func * t
    | Prim_op of Prim_op.t

  (* TODO: switch (to implement match) *)

  (* TODO: what about functions as args/return values? *)
  and func =
    { name : Unique_name.t
    ; params : (Unique_name.t * Value_kind.t) list
    ; return : Value_kind.t
    ; body : t
    }

  let of_typed_expr ~names name_table expr =
    match (expr : Typed.Expr.generalized) with
    | Literal lit, _ -> Literal lit
    | Name (_, _), _ -> 
    | Fun_call (_, _), _
    | Lambda _, _
    | Match (_, _), _
    | Let _, _
    | Tuple _, _ -> failwith "TODO: MIR expr cases"
    | Record_literal _, _ | Record_update (_, _), _ | Record_field_access (_, _), _ ->
      failwith "TODO: records in MIR"
  ;;
end

module Stmt = struct
  (* TODO: closures require creating functions on the fly
     - I suppose we can model this by creating a struct/record with all the used
       parameters and passing that in to a global function *)
  type t =
    | Value_def of Unique_name.t * Expr.t
    | Fun_def of Expr.func

  (* NOTE: Functions can be computed at runtime e.g.
     `let f = if true then fun x -> x * 2 else fun x -> x * 3` *)
end

type t = Stmt.t list

let of_typed_module ~names ((module_name, _sigs, defs) : Typed.Module.t) =
  List.fold defs ~init:[] ~f:(fun stmts def ->
    match def.Node.node with
    | Let bindings ->
      List.fold bindings ~init:stmts ~f:(fun stmts { node = pattern, expr; _ } ->
        (* TODO: match up bound names in the pattern with the corresponding parts of the
         expression *)
        match pattern with
        | Catch_all None ->
          (* FIXME: need to store unique_names and reuse them properly across all of
             Name_bindings - has to be stored in the Name_entry*)
          let mir_expr = Expr.of_typed_expr ~names (Name_table.)
          Value_def (Unique_name.of_ustring underscore, Expr.of_typed_expr ~names expr)
          :: stmts
        | Catch_all (Some _) -> (* FIXME *) failwith "TODO: let bindings names"
        | Tuple _ | Record _ ->
          (* Assert the result can be destructed, then destruct it, binding to the names *)
          failwith "TODO: tuple/record pattern bindings"
        | As (_, _) ->
          (* Bind the result to the name as well as the other pattern *)
          failwith "TODO: as pattern bindings"
        | Constant _ ->
          (* Assert equality between the result and the constant *)
          failwith "TODO: constant pattern bindings"
        | Cnstr_appl ((_, _), _) ->
          (* Similar to constant bindings *) failwith "TODO: cnstr pattern bindings"
        | Union (_, _) ->
          (* Assert both cases can be bound to *) failwith "TODO: union pattern bindings")
    | Module (_, _, _) | Trait (_, _, _, _) | Impl (_, _, _, _) ->
      failwith "TODO: Ir.of_typed_module leftover cases"
    | Common_def _ -> stmts)
  |> List.rev
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
