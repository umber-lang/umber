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

(* TODO: Validation/sanitization of label names
   - Symbols
   - Unicode
   - $ for reserved words
   - Starting with $
   - Colons
   - periods

   This is a sad mess :(. Might be best to append a hash and replace any fishy character
   with an underscore
*)
module Label_name : sig
  type t

  include Stringable.S with type t := t

  val of_mir_name : Mir_name.t -> t
  val of_extern_name : Extern_name.t -> t
end = struct
  type t = string

  let of_string = Fn.id
  let to_string = Fn.id
  let of_mir_name = Mir_name.to_string >> of_string
  let of_extern_name = Extern_name.to_string >> of_string
end

let pp_args fmt args ~f =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") f fmt args
;;

let pp_line fmt opcode args ~f =
  Format.fprintf fmt !"%-10s %-10s" "" opcode;
  pp_args fmt args ~f;
  Format.pp_print_newline fmt ()
;;

let pp_label fmt label = Format.fprintf fmt !"%{Label_name}:\n" label

module Size = struct
  type t =
    | I8
    | I16
    | I32
    | I64
end

module Asm_literal = struct
  type t =
    | Int of int
    | Float of float
    | String of Ustring.t

  let pp_string_escaped fmt s =
    Format.pp_print_char fmt '`';
    Ustring.iter s ~f:(fun uchar ->
      match Uchar.to_char uchar with
      | Some c ->
        (match c with
         | '\\' -> Format.pp_print_string fmt "\\\\"
         | '\000' .. '\031' | '\127' ->
           (* Use a one-byte escape. *)
           Format.fprintf fmt "\\x%x" (Char.to_int c)
         | _ -> Format.pp_print_char fmt c)
      | None ->
        (* Use a UTF-8 escape. *)
        Format.fprintf fmt !"\\u%x" (Uchar.to_int uchar));
    Format.pp_print_char fmt '`'
  ;;

  let pp fmt t =
    match t with
    | String s -> pp_string_escaped fmt s
    | Int i -> Format.fprintf fmt "%d" i
    | Float x -> Format.fprintf fmt "%f" x
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

module Global_kind = struct
  type t =
    | Extern_proc
    | Other
end

module Arg = struct
  type t =
    | Register of Register.t
    | Memory of Size.t * t
    | Global of Label_name.t * Global_kind.t

  let rec pp fmt t =
    match t with
    | Register reg -> Register.pp fmt reg
    | Memory (size, t) ->
      Format.pp_print_string
        fmt
        (match size with
         | I8 -> "byte"
         | I16 -> "word"
         | I32 -> "dword"
         | I64 -> "qword");
      Format.pp_print_string fmt " [";
      pp fmt t;
      Format.pp_print_string fmt "]"
    | Global (name, kind) ->
      (match kind with
       | Extern_proc ->
         (* "wrt ..plt" is needed for PIE (Position Independent Executables). *)
         Format.fprintf fmt !"%{Label_name} wrt ..plt" name
       | Other -> Format.fprintf fmt !"%{Label_name}" name)
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
    { label : Label_name.t
    ; instrs : Instr.t list
    }

  let pp fmt { label; instrs } =
    pp_label fmt label;
    List.iter instrs ~f:(Instr.pp fmt)
  ;;
end

module Data_decl = struct
  type t =
    { label : Label_name.t
    ; payloads : (Size.t * Asm_literal.t) list
    }

  let pp fmt { label; payloads } =
    pp_label fmt label;
    List.iter payloads ~f:(fun (kind, payload) ->
      let kind =
        match kind with
        | I8 -> "db"
        | I16 -> "dw"
        | I32 -> "dd"
        | I64 -> "dq"
      in
      pp_line fmt kind [ payload ] ~f:Asm_literal.pp)
  ;;
end

module Bss_decl = struct
  type t =
    { label : Label_name.t
    ; kind : [ `Words ]
    ; size : int
    }

  let pp fmt { label; kind; size } =
    pp_label fmt label;
    let kind =
      match kind with
      | `Words -> "resq"
    in
    pp_line fmt kind [ size ] ~f:Int.pp
  ;;
end

module Global_decl = struct
  type t =
    { name : Label_name.t
    ; strength : [ `Weak | `Strong ]
        (** [strength] determines whether this global will be deduplicated with globals from
        other files. [`Strong] is the default. We use [`Weak] for literal constants. *)
    }

  let pp fmt { name; strength } =
    match strength with
    | `Strong -> Format.fprintf fmt !"%{Label_name}" name
    | `Weak -> Format.fprintf fmt !"%{Label_name}:weak" name
  ;;
end

module Program = struct
  type t =
    { globals : Global_decl.t list
    ; externs : Label_name.t list
    ; text_section : Instr_group.t list
    ; rodata_section : Data_decl.t list
    ; bss_section : Bss_decl.t list
    }

  let pp_section fmt name contents ~align ~f =
    if not (List.is_empty contents)
    then (
      pp_line fmt "section" [ name ] ~f:(fun fmt -> Format.fprintf fmt ".%s");
      Option.iter align ~f:(fun align -> pp_line fmt "sectalign" [ align ] ~f:Int.pp);
      List.iter contents ~f:(f fmt);
      Format.pp_print_newline fmt ())
  ;;

  let pp fmt { globals; externs; text_section; rodata_section; bss_section } =
    pp_line fmt "default" [ "rel" ] ~f:Format.pp_print_string;
    List.iter globals ~f:(fun global -> pp_line fmt "global" [ global ] ~f:Global_decl.pp);
    List.iter externs ~f:(fun name ->
      pp_line fmt "extern" [ Label_name.to_string name ] ~f:Format.pp_print_string);
    Format.pp_print_newline fmt ();
    pp_section fmt "text" text_section ~align:None ~f:Instr_group.pp;
    pp_section fmt "rodata" rodata_section ~align:(Some 8) ~f:Data_decl.pp;
    pp_section fmt "bss" bss_section ~align:(Some 8) ~f:Bss_decl.pp
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
    | C_function of Label_name.t
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
    ; code : (Label_name.t * Instr.t Queue.t) Queue.t
    ; mutable current_group : int
    }

  let create fun_name =
    { register_states = Register.Table.create ()
    ; code = Queue.singleton (fun_name, Queue.create ())
    ; current_group = 0
    }
  ;;

  let add_code t instr = Queue.enqueue (snd (Queue.get t.code t.current_group)) instr

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
    let result =
      Nonempty.iter2
        args
        (Call_conv.arg_registers call_conv)
        ~f:(fun current_loc target_reg ->
        match current_loc with
        | Register current_reg ->
          if not (Register.equal current_reg target_reg)
          then failwith "TODO: different regs"
          else add_code t (Mov { src = Register current_reg; dst = Register target_reg })
        | Global _ as src -> add_code t (Mov { src; dst = Register target_reg })
        | Memory _ -> failwith "TODO: memory arg location")
    in
    (match result with
     | Same_length | Right_trailing _ -> ()
     | Left_trailing _ -> failwith "TODO: ran out of registers for args, use stack");
    t
  ;;

  let instr_groups t =
    Queue.to_list t.code
    |> List.map ~f:(fun (label, instrs) : Instr_group.t ->
         { label; instrs = Queue.to_list instrs })
  ;;
end

type t =
  { (* FIXME: name_table is unused *)
    name_table : Mir_name.Name_table.t
  ; bss_globals : Label_name.t Queue.t
  ; externs : Extern.t Mir_name.Table.t
  ; literals : Label_name.t Literal.Table.t
  ; main_function : Function_builder.t
  }

let constant_block ~tag ~len ~data_kind data : (Size.t * Asm_literal.t) list =
  [ I16, Int (Cnstr_tag.to_int tag)
  ; I16, Int len
  ; I32, Int 0 (* padding *)
  ; data_kind, data
  ]
;;

let constant_block_for_literal (literal : Literal.t) =
  match literal with
  | Int i -> constant_block ~tag:Cnstr_tag.int ~len:1 ~data_kind:I64 (Int i)
  | Float x -> constant_block ~tag:Cnstr_tag.float ~len:1 ~data_kind:I64 (Float x)
  | Char c ->
    (* TODO: We could just store Chars as immediate values, they are guaranteed to fit *)
    constant_block ~tag:Cnstr_tag.char ~len:1 ~data_kind:I8 (String (Ustring.of_uchar c))
  | String s ->
    let s = Ustring.to_string s in
    let n_chars = String.length s in
    let n_words = (n_chars / 8) + 1 in
    let padded_s =
      String.init (n_words * 8) ~f:(fun char_index ->
        if char_index < n_chars
        then s.[char_index]
        else if char_index = (n_words * 8) - 1
        then (* Last byte *) Char.of_int_exn (7 - (n_chars % 8))
        else (* Padding null prefix of last word *) Char.of_int_exn 0)
    in
    constant_block
      ~tag:Cnstr_tag.string
      ~len:n_words
      ~data_kind:I8
      (String (Ustring.of_string_exn padded_s))
;;

let to_program { bss_globals; externs; literals; main_function; name_table = _ }
  : Program.t
  =
  let uninitialized_globals = Queue.to_list bss_globals in
  let literals = Hashtbl.to_alist literals in
  { globals =
      { name = Label_name.of_string "main"; strength = `Strong }
      :: (List.map uninitialized_globals ~f:(fun name : Global_decl.t ->
            { name; strength = `Strong })
          @ List.map literals ~f:(fun ((_ : Literal.t), name) : Global_decl.t ->
              { name; strength = `Weak }))
  ; externs =
      Hashtbl.to_alist externs
      |> List.map ~f:(fun (mir_name, extern) ->
           match extern with
           | C_function name -> name
           | Umber_function -> Label_name.of_mir_name mir_name)
  ; text_section = Function_builder.instr_groups main_function
  ; rodata_section =
      List.map literals ~f:(fun (literal, label) : Data_decl.t ->
        { label; payloads = constant_block_for_literal literal })
  ; bss_section =
      List.map uninitialized_globals ~f:(fun name : Bss_decl.t ->
        { label = name; kind = `Words; size = 1 })
  }
;;

let pp fmt t = Program.pp fmt (to_program t)

let create () =
  { name_table = Mir_name.Name_table.create ()
  ; bss_globals = Queue.create ()
  ; externs = Mir_name.Table.create ()
  ; literals = Literal.Table.create ()
  ; main_function =
      (* FIXME: Use umber_main and make up a unique name *)
      Function_builder.create (Label_name.of_string "main")
  }
;;

let codegen_literal t (literal : Literal.t) : Arg.t =
  let name =
    Hashtbl.find_or_add t.literals literal ~default:(fun () ->
      let name =
        match literal with
        | Int i -> sprintf "int.%d" i
        | Float x -> sprintf "float.%f" x
        | String s -> sprintf "string.%d" (Ustring.hash s)
        | Char c -> sprintf "char.%d" (Uchar.to_int c)
      in
      Label_name.of_string name)
  in
  Global (name, Other)
;;

let rec codegen_expr t (expr : Mir.Expr.t) ~(fun_builder : Function_builder.t) =
  match expr with
  | Primitive literal -> codegen_literal t literal
  | Fun_call (name, args) ->
    (* TODO: Maybe we could do something cleverer like suggest a good place to put the
       expression value (to reduce the need for moves) *)
    let args = Nonempty.map args ~f:(codegen_expr t ~fun_builder) in
    let name, (call_conv : Call_conv.t), (global_kind : Global_kind.t) =
      match Hashtbl.find t.externs name with
      | Some (C_function name) -> name, C, Extern_proc
      | Some Umber_function -> Label_name.of_mir_name name, Umber, Extern_proc
      | None -> Label_name.of_mir_name name, Umber, Other
    in
    let fun_builder =
      Function_builder.move_values_for_call fun_builder ~call_conv ~args
    in
    Function_builder.add_code fun_builder (Call (Global (name, global_kind)));
    let output_register = Call_conv.return_value_reg call_conv in
    Hashtbl.set fun_builder.register_states ~key:output_register ~data:Used;
    Register output_register
  | Name _ | Let (_, _, _) | Make_block _ | Get_block_field (_, _) | Cond_assign _ ->
    failwith "TODO: missing mir expr kinds"
;;

let set_global t global expr ~fun_builder =
  Queue.enqueue t.bss_globals global;
  let expr_location = codegen_expr t expr ~fun_builder in
  Function_builder.add_code
    fun_builder
    (Mov { dst = Memory (I64, Global (global, Other)); src = expr_location })
;;

(* FIXME: cleanup *)
let codegen_stmt t stmt =
  match (stmt : Mir.Stmt.t) with
  | Value_def (name, expr) ->
    (* TODO: values *)
    set_global t (Label_name.of_mir_name name) expr ~fun_builder:t.main_function
  | Fun_def { fun_name = _; args = _; body = _ } ->
    (* TODO: functions definitions *)
    ()
  | Fun_decl { name; arity = _ } ->
    Hashtbl.add_exn t.externs ~key:name ~data:Umber_function
  | Extern_decl { name; extern_name; arity = _ } ->
    (* TODO: Do we even need arity anymore? Also applies to [Fun_decl] above. *)
    Hashtbl.add_exn
      t.externs
      ~key:name
      ~data:(C_function (Label_name.of_extern_name extern_name))
;;

(* FIXME: Decide what to do with this *)
let codegen_runtime_required_functions t =
  (* Ensure that functions the runtime needs are codegened. See closure.rs in the runtime. *)
  (* codegen_umber_apply_fun t ~n_args:2 *)
  set_global
    t
    (Label_name.of_string "umber_apply2")
    (Primitive (Int 0))
    ~fun_builder:t.main_function
;;

let of_mir mir =
  let t = create () in
  List.iter mir ~f:(codegen_stmt t);
  codegen_runtime_required_functions t;
  Function_builder.add_code t.main_function Ret;
  t
;;

let%expect_test "hello world" =
  let hello_world : Program.t =
    { globals = [ { name = Label_name.of_string "main"; strength = `Strong } ]
    ; externs = [ Label_name.of_string "puts" ]
    ; text_section =
        [ { label = Label_name.of_string "main"
          ; instrs =
              [ Lea
                  { dst = Register Rdi
                  ; src = Memory (I64, Global (Label_name.of_string "message", Other))
                  }
              ; Call (Global (Label_name.of_string "puts", Extern_proc))
              ; Ret
              ]
          }
        ]
    ; rodata_section =
        [ { label = Label_name.of_string "message"
          ; payloads = [ I8, String (Ustring.of_string_exn "Hello, world!"); I8, Int 10 ]
          }
        ]
    ; bss_section = []
    }
  in
  Program.pp Format.std_formatter hello_world;
  [%expect
    {|
               default   rel
               global    main
               extern    puts

               section   .text
    main:
               lea       rdi, qword [message]
               call      puts wrt ..plt
               ret

               section   .rodata
               sectalign 8
    message:
               db        `Hello, world!`
               db        10 |}]
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
            (Value_name.Absolute.of_string "HelloWorld.#binding")
        , Fun_call
            (print_name, [ Primitive (String (Ustring.of_string_exn "Hello, world!")) ])
        )
    ]
  in
  Program.pp Format.std_formatter (to_program (of_mir hello_world));
  [%expect
    {|
               default   rel
               global    main
               global    HelloWorld.#binding.1
               global    umber_apply2
               global    string.210886959:weak
               global    int.0:weak
               extern    umber_print_endline

               section   .text
    main:
               mov       rdi, string.210886959
               call      umber_print_endline wrt ..plt
               mov       qword [HelloWorld.#binding.1], rax
               mov       qword [umber_apply2], int.0
               ret

               section   .rodata
               sectalign 8
    string.210886959:
               dw        32772
               dw        2
               dd        0
               db        `Hello, world!\x0\x0\x2`
    int.0:
               dw        32769
               dw        1
               dd        0
               dq        0

               section   .bss
               sectalign 8
    HelloWorld.#binding.1:
               resq      1
    umber_apply2:
               resq      1 |}]
;;
