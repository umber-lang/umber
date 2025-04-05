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

(* TODO: Nasm surely doesn't support exactly the same literals as OCaml, but this assumes
   they're the same. *)
module Literal = struct
  include Literal

  let pp fmt t =
    match t with
    | String s -> String.pp fmt (Ustring.to_string s)
    | Char c -> String.pp fmt (Uchar.to_string c)
    | Int i -> Int.pp fmt i
    | Float x -> Float.pp fmt x
  ;;
end

module Register = struct
  module T = struct
    type t =
      | Rax
      | Rcx
      | Rdx
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
    [@@deriving equal, compare, hash, sexp, variants, enumerate]
  end

  include T
  include Hashable.Make (T)

  let pp fmt t = Format.pp_print_string fmt (String.lowercase (Variants.to_name t))
end

module Arg = struct
  type t =
    | Register of Register.t
    | Memory of memory
    | Global of string

  and memory = Rel_label of string

  let rec pp fmt t =
    match t with
    | Register reg -> Register.pp fmt reg
    | Memory mem ->
      Format.pp_print_string fmt "[";
      pp_memory fmt mem;
      Format.pp_print_string fmt "]"
    | Global name ->
      (* "wrt ..plt" is needed for PIE (Position Independent Executables). *)
      Format.fprintf fmt "%s wrt ..plt" name

  and pp_memory fmt mem =
    match mem with
    | Rel_label name -> Format.fprintf fmt "rel %s" name
  ;;
end

module Instr = struct
  type t =
    | Mov of
        { dst : Arg.t
        ; src : Arg.t
        }
    | Lea of
        { dst : Arg.t
        ; src : Arg.t
        }
    | Call of Arg.t
    | Ret
  [@@deriving variants]

  let args t =
    match t with
    | Mov { dst; src } | Lea { dst; src } -> [ dst; src ]
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
    ; kind : [ `Bytes ]
    ; payload : Literal.t list
    }

  let pp fmt { label; kind; payload } =
    pp_label fmt label;
    let kind =
      match kind with
      | `Bytes -> "db"
    in
    pp_line fmt kind payload ~f:Literal.pp
  ;;
end

module Bss_decl = struct
  type t =
    { label : string
    ; kind : [ `Words ]
    ; size : int
    }

  let pp fmt { label; kind; size } =
    pp_label fmt label;
    let kind =
      match kind with
      | `Words -> "resw"
    in
    pp_line fmt kind [ size ] ~f:Int.pp
  ;;
end

module Program = struct
  type t =
    { globals : string list
    ; externs : string list
    ; text_section : Instr_group.t list
    ; rodata_section : Data_decl.t list
    ; bss_section : Bss_decl.t list
    }

  let pp_section fmt name contents ~f =
    pp_line fmt "section" [ name ] ~f:(fun fmt -> Format.fprintf fmt ".%s");
    List.iter contents ~f:(f fmt)
  ;;

  let pp fmt { globals; externs; text_section; rodata_section; bss_section } =
    List.iter globals ~f:(fun name -> pp_line_one_arg fmt "global" name);
    List.iter externs ~f:(fun name -> pp_line_one_arg fmt "extern" name);
    Format.pp_print_newline fmt ();
    pp_section fmt "text" text_section ~f:Instr_group.pp;
    Format.pp_print_newline fmt ();
    pp_section fmt "rodata" rodata_section ~f:Data_decl.pp;
    Format.pp_print_newline fmt ();
    pp_section fmt "bss" bss_section ~f:Bss_decl.pp
  ;;
end

(* TODO: use or delete *)
(* module Register_kind = struct
  type t =
    | Int_arg of int
    | Return_value
    | Other_caller_save
    | Other_callee_save
    | Reserved
end *)

module Call_conv = struct
  type t =
    | C
    | Umber

  (* TODO: Handle further arguments with the stack *)
  let arg_registers t : Register.t Nonempty.t =
    match t with
    | C -> [ Rdi; Rsi; Rdx; Rcx; R8; R9 ]
    | Umber -> failwith "umber cc"
  ;;

  let return_value_reg : t -> Register.t = function
    | C -> Rax
    | Umber -> failwith "umber cc"
  ;;
end

module Extern = struct
  type t =
    | C_function of Extern_name.t
    | Umber_function
end

module Register_state = struct
  type t =
    | Used
    | Unused
end

module Function_builder = struct
  (* TODO: Represents a partial codegened function. We need to consider the state of each
     register. *)
  type t =
    { register_states : Register_state.t Register.Table.t
    ; code : Instr.t Queue.t
    }

  let create () = { register_states = Register.Table.create (); code = Queue.create () }

  let move_values_for_call t ~call_conv ~(args : Arg.t Nonempty.t) =
    (* FIXME: Sort out the arguments - move them to where they need to be. Do a diff with
       the current and target register states. Be careful about ordering - we need to be
       careful not to clobber any of the args, so we can't just be like "for each arg,
       move it to where it should go". We might need to use extra registers to do swaps.
       
       e.g. how to handle this:

       Current: 1: a, 2: b, 3: c
       Target:  1: c, 2: a, 3: b

       We need:
       1 -> 4 (1: a, 2: b, 3: c, 4: a)
       3 -> 1 (1: c, 2: b, 3: c, 4: a)
       2 -> 3 (1: c, 2: b, 3: b, 4: a)
       4 -> 2 (1: c, 2: a, 3: b, 4: a)

       Also need to handle spilling onto the stack (and having values on the stack)
       if we run out of registers
       - we might need a bit more extra info than Arg.t has e.g. what stack slock a
       variable is at 
       
       A different approach would be to *force* each of the subsequent generation functions
       to put their result in target register. Then other calls could either avoid using
       that register or spill it, then restore it (it'd be most efficient to restore only
       at the end, I think, to reduce traffic, but then we'd have to keep track of the
       fact that it's been spilled...). Maybe keeping a suggestion but being able to deal
       with whatever ended up happening is easier after all...

       Let's try something naive first.
    *)
    let (), fold2_result =
      Nonempty.fold2
        args
        (Call_conv.arg_registers call_conv)
        ~init:()
        ~f:(fun () current_loc target_reg ->
        match current_loc with
        | Register current_reg ->
          if not (Register.equal current_reg target_reg)
          then failwith "TODO: different regs"
        | _ -> failwith "TODO: non-reg arg location")
    in
    (match fold2_result with
     | Same_length | Right_trailing _ -> ()
     | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack");
    t
  ;;

  (* FIXME: Remember to reverse when outputting code *)
end

type t =
  { name_table : Mir_name.Name_table.t
  ; globals : Mir_name.t Queue.t
  ; externs : Extern.t Mir_name.Table.t
  ; literals : Mir_name.t Literal.Table.t
  ; main_function : Function_builder.t
  }

let to_program { globals; externs; literals; main_function = _; name_table = _ }
  : Program.t
  =
  let globals = List.map (Queue.to_list globals) ~f:Mir_name.to_string in
  { globals
  ; externs =
      Hashtbl.to_alist externs
      |> List.map ~f:(fun (mir_name, extern) ->
           match extern with
           | C_function extern_name -> Extern_name.to_string extern_name
           | Umber_function -> Mir_name.to_string mir_name)
  ; text_section = []
  ; rodata_section =
      Hashtbl.to_alist literals
      |> List.map ~f:(fun (literal, mir_name) : Data_decl.t ->
           { label = Mir_name.to_string mir_name; kind = `Bytes; payload = [ literal ] })
  ; bss_section =
      List.map globals ~f:(fun name : Bss_decl.t ->
        { label = name; kind = `Words; size = 1 })
  }
;;

let pp fmt t = Program.pp fmt (to_program t)

let create () =
  { name_table = Mir_name.Name_table.create ()
  ; globals = Queue.create ()
  ; externs = Mir_name.Table.create ()
  ; literals = Literal.Table.create ()
  ; main_function = Function_builder.create ()
  }
;;

let codegen_literal t (literal : Literal.t) : Arg.t =
  let name =
    Hashtbl.find_or_add t.literals literal ~default:(fun () ->
      let name =
        match literal with
        | Int i -> sprintf "int.%d" i
        | Float x -> sprintf "float.%f" x
        | String s -> sprintf !"string.%{Ustring}" s
        | Char c -> sprintf !"char.%{Uchar}" c
      in
      Mir_name.create_value_name
        t.name_table
        (Value_name.Absolute.of_relative_unchecked
           (Value_name.Relative.of_ustrings_unchecked ([], Ustring.of_string_exn name))))
  in
  Memory (Rel_label (Mir_name.to_string name))
;;

let rec codegen_expr t (expr : Mir.Expr.t) ~(fun_builder : Function_builder.t) =
  match expr with
  | Primitive literal -> codegen_literal t literal
  | Fun_call (name, args) ->
    (* TODO: Maybe we could do something cleverer like suggest a good place to put the
       expression value (to reduce the need for moves) *)
    let args = Nonempty.map args ~f:(codegen_expr t ~fun_builder) in
    let name, (call_conv : Call_conv.t) =
      match Hashtbl.find_exn t.externs name with
      | C_function extern_name -> Extern_name.to_string extern_name, C
      | Umber_function -> Mir_name.to_string name, Umber
    in
    let fun_builder =
      Function_builder.move_values_for_call fun_builder ~call_conv ~args
    in
    Queue.enqueue fun_builder.code (Call (Global name));
    let output_register = Call_conv.return_value_reg call_conv in
    Hashtbl.set fun_builder.register_states ~key:output_register ~data:Used;
    Register output_register
  | Name _ | Let (_, _, _) | Make_block _ | Get_block_field (_, _) | Cond_assign _ ->
    failwith "TODO: missing mir expr kinds"
;;

let set_global t global expr ~fun_builder =
  let expr_location = codegen_expr t expr ~fun_builder in
  Queue.enqueue
    fun_builder.code
    (Mov { dst = Memory (Rel_label (Mir_name.to_string global)); src = expr_location })
;;

(* FIXME: cleanup *)
let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    (* TODO: values *)
    Queue.enqueue t.globals name;
    set_global t name expr ~fun_builder:t.main_function
  | Fun_def { fun_name = _; args = _; body = _ } ->
    (* TODO: functions definitions *)
    ()
  | Fun_decl _ ->
    (* TODO: functions declarations *)
    ()
  | Extern_decl { name; extern_name; arity = _ } ->
    (* TODO: Do we even need arity anymore? *)
    Hashtbl.add_exn t.externs ~key:name ~data:(C_function extern_name)
;;

let of_mir mir =
  let t = create () in
  List.iter mir ~f:(codegen_stmt t);
  t
;;

let%expect_test "hello world" =
  let hello_world : Program.t =
    { globals = [ "main" ]
    ; externs = [ "puts" ]
    ; text_section =
        [ { label = "main"
          ; instrs =
              [ Lea { dst = Register Rdi; src = Memory (Rel_label "message") }
              ; Call (Global "puts")
              ; Ret
              ]
          }
        ]
    ; rodata_section =
        [ { label = "message"
          ; kind = `Bytes
          ; payload = [ String (Ustring.of_string_exn "Hello, world!"); Int 10 ]
          }
        ]
    ; bss_section = []
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

let%expect_test "hello world, from MIR" =
  let name_table = Mir_name.Name_table.create () in
  let print_name =
    Mir_name.create_exportable_name
      (Value_name.Absolute.of_relative_unchecked
         (Value_name.Relative.of_string "Std.Prelude.print"))
  in
  let hello_world : Mir.t =
    [ Extern_decl
        { name = print_name
        ; extern_name = Extern_name.of_string_exn "umber_print_endline"
        ; arity = 1
        }
    ; Value_def
        ( Mir_name.create_value_name
            name_table
            (Value_name.Absolute.of_string "HelloWorld.*binding")
        , Fun_call
            (print_name, [ Primitive (String (Ustring.of_string_exn "Hello, world!")) ])
        )
    ]
  in
  Program.pp Format.std_formatter (to_program (of_mir hello_world))
;;
