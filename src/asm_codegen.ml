open Import
open Names

(* FIXME: stop silencing unused warnings *)
[@@@warning "-32-69-37"]

(* TODO: Do codegen directly to x86 assembly rather than going through LLVM.
   
   I feel that implementing a runtime for algebraic effects might be too
   difficult/annoying with LLVM. The coroutine support might be able to do it, but
   unclear. If I control everything then I can work at the bare metal. The difficulty will
   involve more low-level concerns rather than trying to twist LLVM's APIs to do something
   they were really not designed for.

   Some problems:
   - I have to rewrite the whole codegen backend
   - I don't know asm much at all
   - I need to think about register allocation
   - I need to think about calling conventions and implement the C calling convention in
     addition to my own one
   - I can't get optimizations for free

   Maybe I could try doing the coroutine thing and see if it works?
*)

let pp_args fmt args ~f =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") f fmt args
;;

let pp_line fmt opcode args ~f =
  Format.fprintf fmt !"%-10s %-10s" "" opcode;
  pp_args fmt args ~f;
  Format.pp_print_newline fmt ()
;;

let pp_line_one_arg fmt opcode arg = pp_line fmt opcode [ arg ] ~f:Format.pp_print_string
let pp_label fmt label = Format.fprintf fmt "%s:\n" label

module Literal = struct
  type t =
    | String of string
    | Int of int

  let pp fmt t =
    match t with
    | String s -> String.pp fmt s
    | Int i -> Int.pp fmt i
  ;;
end

module Register = struct
  type t =
    | Rax
    | Rcx
    | Rbx
    | Rsp
    | Rbp
    | Rsi
    | Rdi
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
  [@@deriving variants]

  let pp fmt t = Format.pp_print_string fmt (String.lowercase (Variants.to_name t))
end

module Arg = struct
  type t =
    | Register of Register.t
    | Memory of memory
    | Global of string

  and memory = Rel_data of string

  let rec pp fmt t =
    match t with
    | Register reg -> Register.pp fmt reg
    | Memory mem ->
      Format.pp_print_string fmt "[";
      pp_memory fmt mem;
      Format.pp_print_string fmt "]"
    | Global name -> Format.fprintf fmt "%s wrt ..plt" name

  and pp_memory fmt mem =
    match mem with
    | Rel_data name -> Format.fprintf fmt "rel %s" name
  ;;
end

module Instr = struct
  type t =
    | Lea of
        { dst : Arg.t
        ; src : Arg.t
        }
    | Call of Arg.t
    | Ret
  [@@deriving variants]

  let args t =
    match t with
    | Lea { dst; src } -> [ dst; src ]
    | Call fun_name -> [ fun_name ]
    | Ret -> []
  ;;

  let pp fmt t = pp_line fmt (String.lowercase (Variants.to_name t)) (args t) ~f:Arg.pp
end

module Instr_group = struct
  type t =
    { label : string
    ; instrs : Instr.t list
    }

  let pp fmt { label; instrs } =
    pp_label fmt label;
    List.iter instrs ~f:(Instr.pp fmt)
  ;;
end

module Data_decl = struct
  type t =
    { label : string
    ; kind : [ `Db ]
    ; payload : Literal.t list
    }

  let pp fmt { label; kind; payload } =
    pp_label fmt label;
    let kind =
      match kind with
      | `Db -> "db"
    in
    pp_line fmt kind payload ~f:Literal.pp
  ;;
end

module Program = struct
  type t =
    { globals : string list
    ; externs : string list
    ; text_section : Instr_group.t list
    ; rodata_section : Data_decl.t list
    }

  let pp_section fmt name contents ~f =
    pp_line fmt "section" [ name ] ~f:(fun fmt -> Format.fprintf fmt ".%s");
    List.iter contents ~f:(f fmt)
  ;;

  let pp fmt { globals; externs; text_section; rodata_section } =
    List.iter globals ~f:(fun name -> pp_line_one_arg fmt "global" name);
    List.iter externs ~f:(fun name -> pp_line_one_arg fmt "extern" name);
    Format.pp_print_newline fmt ();
    pp_section fmt "text" text_section ~f:Instr_group.pp;
    Format.pp_print_newline fmt ();
    pp_section fmt "rodata" rodata_section ~f:Data_decl.pp
  ;;
end

type t =
  { program : Program.t
  ; values : unit Mir_name.Map.t
  }

let pp fmt t = Program.pp fmt t.program

let empty =
  { program = { globals = []; externs = []; text_section = []; rodata_section = [] }
  ; values = Mir_name.Map.empty
  }
;;

(* FIXME: cleanup *)
(* let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    (* let global_value = Value_table.find t.values name in
    Llvm.position_at_end (Llvm.insertion_block t.main_function_builder) t.builder;
    let expr_value = codegen_expr t expr in
    Llvm.position_at_end (Llvm.insertion_block t.builder) t.main_function_builder;
    if Llvm.is_constant expr_value
    then (
      Llvm.set_global_constant true global_value;
      Llvm.set_initializer expr_value global_value)
    else ignore_value (Llvm.build_store expr_value global_value t.main_function_builder) *)
    ()
  | Fun_def { fun_name; args; body } ->
    (* let fun_ = Value_table.find t.values fun_name in
    let fun_params = Llvm.params fun_ in
    Nonempty.iteri args ~f:(fun i arg_name ->
      let arg_value = fun_params.(i) in
      Llvm.set_value_name (Mir_name.to_string arg_name) arg_value;
      Value_table.add t.values arg_name arg_value);
    let entry_block = Llvm.entry_block fun_ in
    Llvm.position_at_end entry_block t.builder;
    let return_value = codegen_expr t body in
    ignore_value (Llvm.build_ret return_value t.builder);
    Llvm_analysis.assert_valid_function fun_ *)
    ()
  | Fun_decl _ | Extern_decl _ -> (* Already handled in the preprocessing step *) ()
;; *)

(* let of_mir ~module_path:_ ~source_filename:_ mir =
  List.fold mir ~init:empty ~f:codegen_stmt
;; *)

let%expect_test "hello world" =
  let hello_world : Program.t =
    { globals = [ "main" ]
    ; externs = [ "puts" ]
    ; text_section =
        [ { label = "main"
          ; instrs =
              [ Lea { dst = Register Rdi; src = Memory (Rel_data "message") }
              ; Call (Global "puts")
              ; Ret
              ]
          }
        ]
    ; rodata_section =
        [ { label = "message"; kind = `Db; payload = [ String "Hello, world!"; Int 10 ] }
        ]
    }
  in
  Program.pp Format.std_formatter hello_world;
  [%expect
    {|
               global    main
               extern    puts

               section   .text
    main:
               lea       rdi, [rel message]
               call      puts wrt ..plt
               ret

               section   .rodata
    message:
               db        "Hello, world!", 10 |}]
;;
