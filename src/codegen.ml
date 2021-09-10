(* Generating LLVM IR for the AST *)

(*open Import
open Names
open Llvm

type t =
  { context : llcontext
  ; module_ : llmodule
  ; values : llvalue Mir.Unique_name.Table.t
  }

let create ~module_name =
  (* TODO: should this use `create_context`? *)
  let context = global_context () in
  { context
  ; module_ =
      create_module context (Ustring.to_string (Module_name.to_ustring module_name))
  ; values = Mir.Unique_name.Table.create ()
  }
;;

let codegen_literal { context; _ } lit =
  match (lit : Literal.t) with
  | Int i -> const_int (i64_type context) i
  | Float x -> const_float (double_type context) x
  | Char c -> const_int (i32_type context) (Uchar.to_int c)
  | String s -> const_string context (Ustring.to_string s)
;;

let rec codegen_expr t expr =
  match (expr : Mir.Expr.t) with
  | Primitive lit -> codegen_literal t lit
  | Name name -> Hashtbl.find_exn t.values name
  | Let (name, expr, body) ->
    Hashtbl.add_exn t.values ~key:name ~data:(codegen_expr t expr);
    codegen_expr t body
  | Fun_call (fun_, args) -> lookup_function
  | Constant_cnstr _
  | Make_block (_, _ :: _)
  | Get_block_field (_, _)
  | If _ | Catch _ | Break _ -> failwith "TODO: codegen_expr"
;;*)

(* TODO: cleanup: probably remove all of this *)

(*module Name_table : sig
  type t

  val create : unit -> t

  (*val of_name_bindings : Name_bindings.t -> t*)
end = struct
  include Value_name.Qualified.Table

  type t = llvalue Value_name.Qualified.Table.t

  let create () = create ()
end

type state =
  { context : llcontext
  ; names : Name_table.t
  }

let codegen_literal { context; _ } lit =
  match (lit : Literal.t) with
  | Int i -> const_int (i64_type context) i
  | Float x -> const_float (double_type context) x
  | Char c -> const_int (i32_type context) (Uchar.to_int c)
  | String s -> const_string context (Ustring.to_string s)
;;

let codegen_expr state ((expr, _) : Typed.Expr.generalized) =
  match expr with
  | Literal lit -> codegen_literal state lit
  | Name _ ->
    failwith "TODO: name" (*name -> Name_table.find_or_add state.names name typ*)
  | Fun_call _
  | Lambda _
  | Match _
  | Let _
  | Tuple _
  | Record_literal _
  | Record_update _
  | Record_field_access _ -> failwith "TODO: codegen_expr leftovers"
;;*)

(*let context = global_context ()
let the_module = create_module context "my module"
let builder = builder context

let codegen_literal : Typed.Literal.t -> _ = function
  | Bool b -> const_int (i1_type context) (if b then 1 else 0)
  | Int i -> const_int (i64_type context) i
  | Float x -> const_float (double_type context) x
  | Char c -> const_int (i8_type context) (Char.to_int c)
  | String s -> const_string context s
;;

let rec codegen_expr : Typed.Expr.t -> _ = function
  | Literal lit -> codegen_literal lit
  | Name name ->
    let { Value.expr; _ } = Name_table.lookup name in
    codegen_expr expr (* TODO: memoization *)
  | If (cond, then_, else_) ->
    let cond = codegen_expr cond in
    (* need to assert cond is a bool -- probably want a separate step for type inference *)
    (* TODO: fix this *) 
    ()

let codegen_stmt : Stmt.t -> _ = function
  | Expr e -> codegen_expr e
  | Let (name, expr) -> Name_table 

*)
