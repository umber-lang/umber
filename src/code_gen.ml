(* TODO: consider using GRIN for codegen https://github.com/grin-compiler/grin *)
(* Generating LLVM IR for the AST *)

(*
open Import
open Llvm
open Ast

let context = global_context ()
let the_module = create_module context "my module"
let builder = builder context

module Name_table : sig
  val lookup : string -> Value.t
end = struct
  let table = Hashtbl.create (module String)

  let lookup name = match Hashtbl.find table name with
    | Some v -> v
    | None -> raise_s [%message "Name error" name]
end

let process_literal : Literal.t -> _ = function
  | Bool b -> const_int (i1_type context) (if b then 1 else 0)
  | Int i -> const_int (i64_type context) i
  | Float x -> const_float (double_type context) x
  | Char c -> const_int (i8_type context) (Char.to_int c)
  | String s -> const_string context s

let rec process_expr : Expr.t -> _ = function
  | Literal lit -> process_literal lit
  | Name name ->
    let { Value.expr; _ } = Name_table.lookup name in
    process_expr expr (* TODO: memoization *)
  | If (cond, then_, else_) ->
    let cond = process_expr cond in
    (* need to assert cond is a bool -- probably want a separate step for type inference *)
    (* TODO: fix this *) 
    ()

let process_stmt : Stmt.t -> _ = function
  | Expr e -> process_expr e
  | Let (name, expr) -> Name_table 

*)
