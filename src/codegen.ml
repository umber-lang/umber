(* Generating LLVM IR for the AST *)

open Import
open Names
open Llvm

let fun_call_name = "fun_call"

type t =
  { context : llcontext
  ; builder : llbuilder
  ; module_ : llmodule
  ; values : llvalue Unique_name.Table.t
  }

let create ~source_filename =
  let context = create_context () in
  { context
  ; builder = builder context
  ; module_ = create_module context source_filename
  ; values = Unique_name.Table.create ()
  }
;;

let block_index_type context = i16_type context

let block_header_type context =
  let typ = named_struct_type context "header" in
  struct_set_body
    typ
    [| i16_type context (* tag *)
     ; block_index_type context (* # of pointers *)
     ; block_index_type context (* # of immediates *)
    |]
    false;
  typ
;;

let block_type context =
  let block_type = named_struct_type context "block" in
  struct_set_body
    block_type
    [| block_header_type context
     ; array_type (pointer_type block_type) 0
     ; array_type (i64_type context) 0
    |]
    false;
  block_type
;;

let block_pointer_type context = pointer_type (block_type context)

let codegen_literal t lit =
  let with_type make_type const x =
    let typ = make_type t.context in
    const typ x
  in
  match (lit : Literal.t) with
  | Int i -> with_type i64_type const_int i
  | Float x -> with_type double_type const_float x
  | Char c -> with_type i32_type const_int (Uchar.to_int c)
  | String s ->
    let s = Ustring.to_string s in
    const_string t.context s
;;

let codegen_constant_tag t tag =
  const_int (integer_type t.context 63) (Mir.Cnstr.Tag.to_int tag)
;;

let codegen_non_constant_tag t tag =
  const_int (i16_type t.context) (Mir.Cnstr.Tag.to_int tag)
;;

let get_block_tag t value =
  build_gep value [| const_int (block_index_type t.context) 0 |] "tag" t.builder
;;

(* TODO: monomorphize polymorphic functions by Value_kind - should be done in mir, I think *)
let rec codegen_expr t expr =
  match (expr : Mir.Expr.t) with
  | Primitive lit -> codegen_literal t lit
  | Name name -> Hashtbl.find_exn t.values name
  | Let (name, expr, body) ->
    Hashtbl.add_exn t.values ~key:name ~data:(codegen_expr t expr);
    codegen_expr t body
  | Fun_call (fun_name, args) ->
    let fun_ =
      Option.value_exn (lookup_function (Unique_name.to_string fun_name) t.module_)
    in
    let args = Array.of_list_map ~f:(codegen_expr t) (Nonempty.to_list args) in
    build_call fun_ args fun_call_name t.builder
  | Make_block { tag; fields } ->
    if List.is_empty fields
    then codegen_constant_tag t tag
    else (
      let fields = List.map fields ~f:(codegen_expr t) in
      box t ~tag ~fields)
  | Get_block_field (i, expr) ->
    const_gep
      (codegen_expr t expr)
      [| const_int (i16_type t.context) (Mir.Block_index.to_int i + 1) |]
  | If { cond; then_; else_ } ->
    let cond = codegen_cond t cond in
    let start_block = insertion_block t.builder in
    let current_fun = block_parent start_block in
    let make_child_block ~label expr =
      let child_block = append_block t.context label current_fun in
      position_at_end child_block t.builder;
      let child_value = codegen_expr t expr in
      (* Codegen in the child can create new basic blocks, so we have to update *)
      let child_block = insertion_block t.builder in
      child_value, child_block
    in
    let ((_, then_block) as then_incoming) = make_child_block ~label:"then" then_ in
    let ((_, else_block) as else_incoming) = make_child_block ~label:"else" else_ in
    let merge_block = append_block t.context "if_merge" current_fun in
    position_at_end merge_block t.builder;
    let phi = build_phi [ then_incoming; else_incoming ] "if_phi" t.builder in
    (* Return to the start block to add the condition *)
    position_at_end start_block t.builder;
    ignore (build_cond_br cond then_block else_block t.builder : llvalue);
    (* Add unconditional jumps from the child blocks to the merge block *)
    position_at_end then_block t.builder;
    ignore (build_br merge_block t.builder : llvalue);
    position_at_end else_block t.builder;
    ignore (build_br merge_block t.builder : llvalue);
    (* Finish *)
    position_at_end merge_block t.builder;
    phi
  | Catch _ | Break _ -> failwith "TODO: codgen catch/break"

and codegen_cond t cond =
  let make_equals value value' =
    (* FIXME: This isn't going to work through pointers, yeah? Need to dereference/gep *)
    build_icmp Eq value value' "equals" t.builder
  in
  match cond with
  | Equals (expr, lit) ->
    (* FIXME: need to handle strings, chars, ints, floats, separately *)
    make_equals (codegen_expr t expr) (codegen_literal t lit)
  | Constant_tag_equals (expr, tag) ->
    make_equals (codegen_expr t expr) (codegen_constant_tag t tag)
  | Non_constant_tag_equals (expr, tag) ->
    make_equals (get_block_tag t (codegen_expr t expr)) (codegen_non_constant_tag t tag)
  | And _ -> failwith "TODO: And conditions"

and box ?(tag = Mir.Cnstr.Tag.default) t ~fields =
  (* TODO: Heap allocation. Also, GC. (Actually, for now, let's just try to plug in a
     conservative GC e.g. Boehm) *)
  let block_header =
    const_named_struct
      (block_header_type t.context)
      [| codegen_non_constant_tag t tag
       ; const_int (block_index_type t.context) (List.length fields)
      |]
  in
  let fields = const_struct t.context (Array.of_list fields) in
  const_struct t.context [| block_header; fields |]
;;

let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    let value =
      define_global (Unique_name.to_string name) (codegen_expr t expr) t.module_
    in
    set_global_constant true value;
    Hashtbl.add_exn t.values ~key:name ~data:value;
    value
  | Fun_def { fun_name; closed_over; args; body } ->
    if not (Set.is_empty closed_over)
    then raise_s [%message "TODO: closures" (closed_over : Unique_name.Set.t)];
    let type_ = block_pointer_type t.context in
    let fun_type = function_type type_ (Array.create type_ ~len:(Nonempty.length args)) in
    let fun_ = define_function (Unique_name.to_string fun_name) fun_type t.module_ in
    let fun_params = params fun_ in
    Nonempty.iteri args ~f:(fun i arg_name ->
      let arg_value = fun_params.(i) in
      set_value_name (Unique_name.to_string arg_name) arg_value;
      Hashtbl.add_exn t.values ~key:arg_name ~data:arg_value);
    let entry_block = append_block t.context "entry" fun_ in
    position_at_end entry_block t.builder;
    let return_value = codegen_expr t body in
    ignore (build_ret return_value t.builder : llvalue);
    (* FIXME: probably re-enable *)
    (*Llvm_analysis.assert_valid_function fun_;*)
    Hashtbl.add_exn t.values ~key:fun_name ~data:fun_;
    fun_
;;

let of_mir ~source_filename mir =
  let t = create ~source_filename in
  List.iter mir ~f:(ignore << codegen_stmt t);
  Llvm_analysis.assert_valid_module t.module_;
  t
;;

let to_string t = string_of_llmodule t.module_
let print t ~to_:file = print_module file t.module_
