open Import
module Name : Names.Unidentified_name = Ustring

module Prim_op = struct
  type t =
    | Int_add (* TODO: arguments? *)
    | Io_print_string
end

module Value_kind = struct
  type t =
    | Int
    | Float
    | Char
    | String
    | Bool

  (* TODO: something like `Block` to represent constructor application *)
end

module Expr = struct
  (* TODO: store the names within each nested scope? *)
  type t =
    | Literal of Untyped.Literal.t
    | Name of Name.t
    | Fun_call of func * t
    | Fun_def of func
    | Prim_op of Prim_op.t

  and func =
    { params : (Name.t * Value_kind.t) list
    ; return : Value_kind.t
    ; body : t
    }

  (* FIXME: can Fun_def just go anywhere? *)
  (*| If
    | Switch*)

  (* TODO: Constructor application? *)
end

module Stmt = struct
  type t = Value_def of Expr.t

  (* Functions can be computed at runtime e.g.
     `let f = if true then fun x -> x * 2 else fun x -> x * 3` *)
end

type t = Stmt.t list

let of_typed_module ~names ((module_name, _sigs, defs) : Typed.Module.t) =
  List.fold defs ~init:[] ~f:(fun stmts def ->
    match def.Node.node with
    | Let bindings -> ()
    | Module (_, _, _) | Trait (_, _, _, _) | Impl (_, _, _, _) ->
      failwith "TODO: Ir.of_typed_module leftover cases"
    | Common_def _ -> stmts)
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
