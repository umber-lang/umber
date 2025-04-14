open Import
open Names

module Label_name = struct
  module T = struct
    type t = string [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Hashable.Make (T)

  let of_string s =
    (* See the NASM for docs for what constitutes a valid identifier:
       https://www.nasm.us/xdoc/2.16.03/html/nasmdoc3.html#section-3.1 *)
    let buffer = Buffer.create (String.length s) in
    let escape_used = ref false in
    let escaping_now = ref false in
    String.iteri s ~f:(fun i c ->
      let needs_escape =
        if i = 0
        then (
          match c with
          | 'a' .. 'z' | 'A' .. 'Z' | '.' | '_' | '?' -> false
          | _ -> true)
        else (
          match c with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' | '@' | '~' | '.' | '?'
            -> false
          | _ -> true)
      in
      match !escaping_now, needs_escape with
      | false, false -> (* No escaping needed. *) Buffer.add_char buffer c
      | false, true ->
        (* Start escaping. *)
        escape_used := true;
        escaping_now := true;
        Buffer.add_string buffer "__"
      | true, false ->
        (* Stop escaping.*)
        escaping_now := false;
        Buffer.add_char buffer c
      | true, true -> (* Keep escaping. *) ());
    if !escape_used
    then (
      Buffer.add_char buffer '#';
      Buffer.add_string buffer (Int.to_string (String.hash s)));
    Buffer.contents buffer
  ;;

  let to_string = Fn.id
  let of_mir_name = Mir_name.to_string >> of_string
  let of_extern_name = Extern_name.to_string >> of_string

  let to_mir_name t =
    Mir_name.create_exportable_name
      (Value_name.Absolute.of_relative_unchecked (Value_name.Relative.of_string t))
  ;;
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
  [@@deriving equal, sexp_of]

  let n_bytes = function
    | I8 -> 1
    | I16 -> 2
    | I32 -> 4
    | I64 -> 8
  ;;
end

module Asm_literal = struct
  (* TODO: I think 64-bit assembly does weird things with int literals, and they'll get
     silently truncated except in Mov. Look into that and write a test. *)
  type t =
    | Int of int
    | Float of float
    | String of Ustring.t
  [@@deriving sexp_of]

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
    (* TODO: Floating point registers xmm0 to xmm7 *)
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
  include Comparable.Make (T)
  include Hashable.Make (T)

  let pp fmt t = Format.pp_print_string fmt (String.lowercase (Variants.to_name t))
end

module Call_conv = struct
  type t =
    | C
    | Umber
  [@@deriving sexp_of]

  (* TODO: Decide on an umber calling convention. For now, let's just copy C's calling
     convention since we have to implement that anyway. We can probably switch to
     basically copying OCaml's after that. *)

  (* TODO: Handle further arguments with the stack *)
  let arg_registers t : Register.t Nonempty.t =
    match t with
    | C | Umber -> [ Rdi; Rsi; Rdx; Rcx; R8; R9 ]
  ;;

  let return_value_register : t -> Register.t = function
    | C | Umber -> Rax
  ;;

  let register_is_reserved t (reg : Register.t) =
    match t, reg with
    | (C | Umber), (Rsp (* Stack pointer *) | Rbp (* Frame pointer *)) -> true
    | _ -> false
  ;;

  let all_available_registers t =
    (* FIXME: Handle caller vs callee saved registers. *)
    List.filter Register.all ~f:(not << register_is_reserved t)
  ;;
end

module Global_kind = struct
  type t =
    | Extern_proc
    | Other
  [@@deriving sexp_of]
end

module Value = struct
  type 'reg t =
    | Register of 'reg
    | Global of Label_name.t * Global_kind.t
    | Constant of Asm_literal.t
    | Memory of Size.t * 'reg memory_expr

  and 'reg memory_expr =
    | Register of 'reg
    | Global of Label_name.t * Global_kind.t
    | Offset of int
    | Add of 'reg memory_expr * 'reg memory_expr
  [@@deriving sexp_of]

  let pp_global fmt name (kind : Global_kind.t) =
    match kind with
    | Extern_proc ->
      (* "wrt ..plt" is needed for PIE (Position Independent Executables). *)
      Format.fprintf fmt !"%{Label_name} wrt ..plt" name
    | Other -> Format.fprintf fmt !"%{Label_name}" name
  ;;

  let rec pp_memory_expr fmt memory_expr =
    match memory_expr with
    | Offset i -> Format.fprintf fmt "%d" i
    | Register reg -> Register.pp fmt reg
    | Global (name, kind) -> pp_global fmt name kind
    | Add (lhs, rhs) ->
      pp_memory_expr fmt lhs;
      Format.pp_print_string fmt " + ";
      pp_memory_expr fmt rhs
  ;;

  let pp fmt t =
    match t with
    | Register reg -> Register.pp fmt reg
    | Memory (size, memory_expr) ->
      Format.pp_print_string
        fmt
        (match size with
         | I8 -> "byte"
         | I16 -> "word"
         | I32 -> "dword"
         | I64 -> "qword");
      Format.pp_print_string fmt " [";
      pp_memory_expr fmt memory_expr;
      Format.pp_print_string fmt "]"
    | Global (name, kind) -> pp_global fmt name kind
    | Constant literal -> Asm_literal.pp fmt literal
  ;;

  let mem_offset mem size offset =
    if offset = 0
    then Memory (size, mem)
    else Memory (size, Add (mem, Offset (Size.n_bytes size * offset)))
  ;;

  let mem_of_value (value : _ t) : _ memory_expr option =
    match value with
    | Register reg -> Some (Register reg)
    | Global (name, kind) -> Some (Global (name, kind))
    | Memory _ -> None
    | Constant (Int i) -> Some (Offset i)
    | Constant (Float _ | String _) ->
      compiler_bug [%message "Invalid memory expression" (value : _ t)]
  ;;

  let rec map_registers_memory_expr (mem : _ memory_expr) ~f : _ memory_expr =
    match mem with
    | Register reg -> Register (f reg)
    | (Global _ | Offset _) as mem -> mem
    | Add (lhs, rhs) ->
      let lhs = map_registers_memory_expr lhs ~f in
      let rhs = map_registers_memory_expr rhs ~f in
      Add (lhs, rhs)
  ;;

  let map_registers t ~f =
    match t with
    | Register reg -> Register (f reg)
    | (Global _ | Constant _) as t -> t
    | Memory (size, mem) -> Memory (size, map_registers_memory_expr mem ~f)
  ;;

  let rec fold_registers_memory_expr (mem : _ memory_expr) ~init ~f =
    match mem with
    | Register reg -> f init reg
    | Global _ | Offset _ -> init
    | Add (lhs, rhs) ->
      let init = fold_registers_memory_expr lhs ~init ~f in
      fold_registers_memory_expr rhs ~init ~f
  ;;

  let fold_registers t ~init ~f =
    match t with
    | Register reg -> f init reg
    | Global _ | Constant _ -> init
    | Memory ((_ : Size.t), mem) -> fold_registers_memory_expr mem ~init ~f
  ;;

  let rec fold_map_registers_memory_expr (mem : _ memory_expr) ~init ~f
    : _ * _ memory_expr
    =
    match mem with
    | Register reg ->
      let init, reg = f init reg in
      init, Register reg
    | (Global _ | Offset _) as mem -> init, mem
    | Add (lhs, rhs) ->
      let init, lhs = fold_map_registers_memory_expr lhs ~init ~f in
      let init, rhs = fold_map_registers_memory_expr rhs ~init ~f in
      init, Add (lhs, rhs)
  ;;

  let fold_map_registers t ~init ~f =
    match t with
    | Register reg ->
      let init, reg = f init reg in
      init, Register reg
    | (Global _ | Constant _) as t -> init, t
    | Memory (size, mem) ->
      let init, mem = fold_map_registers_memory_expr mem ~init ~f in
      init, Memory (size, mem)
  ;;
end

(* TODO: 64-bit values only work with mov - they'll get silently truncated
   otherwise. They need to be loaded into a register first. Ideally the types shouldn't
   allow any Instr except Mov to have a literal value. *)
(* TODO: Might make sense to abstract this a little more e.g. group the jumps into one
   variant. Might make some of the code easier to handle. *)
module Instr = struct
  module Nonterminal = struct
    type 'reg t =
      | And of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Call of
          { fun_ : 'reg Value.t
          ; call_conv : Call_conv.t
          ; arity : int
          }
      | Cmp of 'reg Value.t * 'reg Value.t
      | Lea of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      (* FIXME: Prevent Mov between two memory locations, it's not allowed. Likely
         similar constraints for other instructions. *)
      | Mov of
          { dst : 'reg Value.t
          ; src : 'reg Value.t
          }
      | Test of 'reg Value.t * 'reg Value.t
    [@@deriving sexp_of, variants]

    let args = function
      | And { dst; src } | Mov { dst; src } | Lea { dst; src } -> [ dst; src ]
      | Cmp (a, b) | Test (a, b) -> [ a; b ]
      | Call { fun_ = x; call_conv = _; arity = _ } -> [ x ]
    ;;

    let map_args t ~f =
      match t with
      | And { dst; src } ->
        let dst = f dst in
        let src = f src in
        And { dst; src }
      | Mov { dst; src } ->
        let dst = f dst in
        let src = f src in
        Mov { dst; src }
      | Lea { dst; src } ->
        let dst = f dst in
        let src = f src in
        Lea { dst; src }
      | Cmp (a, b) ->
        let a = f a in
        let b = f b in
        Cmp (a, b)
      | Test (a, b) ->
        let a = f a in
        let b = f b in
        Test (a, b)
      | Call { fun_; call_conv; arity } -> Call { fun_ = f fun_; call_conv; arity }
    ;;

    let fold_map_args t ~init ~f =
      match t with
      | And { dst; src } ->
        let init, dst = f init dst in
        let init, src = f init src in
        init, And { dst; src }
      | Mov { dst; src } ->
        let init, dst = f init dst in
        let init, src = f init src in
        init, Mov { dst; src }
      | Lea { dst; src } ->
        let init, dst = f init dst in
        let init, src = f init src in
        init, Lea { dst; src }
      | Cmp (a, b) ->
        let init, a = f init a in
        let init, b = f init b in
        init, Cmp (a, b)
      | Test (a, b) ->
        let init, a = f init a in
        let init, b = f init b in
        init, Test (a, b)
      | Call { fun_; call_conv; arity } ->
        let init, fun_ = f init fun_ in
        init, Call { fun_; call_conv; arity }
    ;;
  end

  module Terminal = struct
    type t =
      | Ret
      | Jump of Label_name.t
      | Jump_if of
          { cond : [ `Zero | `Nonzero ]
          ; then_ : Label_name.t
          ; else_ : Label_name.t
          }
    [@@deriving sexp_of, variants]

    let name = function
      | Ret -> "ret"
      | Jump _ -> "jmp"
      | Jump_if { cond; _ } ->
        (match cond with
         | `Zero -> "jz"
         | `Nonzero -> "jnz")
    ;;

    let args : t -> _ Value.t list = function
      | Jump label | Jump_if { cond = _; then_ = label; else_ = _ } ->
        [ Global (label, Other) ]
      | Ret -> []
    ;;
  end

  type 'reg t =
    | Terminal of Terminal.t
    | Nonterminal of 'reg Nonterminal.t
  [@@deriving sexp_of, variants]

  let pp fmt t =
    let name, args =
      match t with
      | Terminal t -> Terminal.name t, Terminal.args t
      | Nonterminal t -> Nonterminal.Variants.to_name t, Nonterminal.args t
    in
    pp_line fmt (String.lowercase name) args ~f:Value.pp
  ;;

  (* FIXME: cleanup *)
  (* let fold_map_args t ~init ~f =
    let handle_jump init label ~(f : _ -> _ Value.t -> _ * _ Value.t) ~mk =
      let init, value = f init (Global (label, Other)) in
      let label =
        match value with
        | Global (label, _) -> label
        | _ ->
          compiler_bug
            [%message "Expected label name for jump instruction" (value : _ Value.t)]
      in
      init, mk label
    in
    match t with
    | Nonterminal t ->
      let init, t = Nonterminal.fold_map_args t ~init ~f in
      init, Nonterminal t
    | Terminal t ->
      let init, (t : Terminal.t) =
        match t with
        | Jmp label -> handle_jump init label ~f ~mk:Terminal.jmp
        | Jnz label -> handle_jump init label ~f ~mk:Terminal.jnz
        | Jz label -> handle_jump init label ~f ~mk:Terminal.jz
        | Ret -> init, Ret
      in
      init, Terminal t *)
end

module Basic_block = struct
  type 'reg t =
    { label : Label_name.t
    ; code : 'reg Instr.Nonterminal.t list
    ; terminal : Instr.Terminal.t
    }
  [@@deriving sexp_of]

  let pp fmt { label; code; terminal } =
    pp_label fmt label;
    List.iter code ~f:(Instr.pp fmt << Instr.nonterminal);
    Instr.pp fmt (Terminal terminal)
  ;;
end

module Data_decl = struct
  module Payload = struct
    type t =
      | Literal of Asm_literal.t
      | Label of Label_name.t

    let pp fmt t =
      match t with
      | Literal literal -> Asm_literal.pp fmt literal
      | Label label_name -> Format.fprintf fmt !"%{Label_name}" label_name
    ;;
  end

  type t =
    { label : Label_name.t
    ; payloads : (Size.t * Payload.t) list
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
      pp_line fmt kind [ payload ] ~f:Payload.pp)
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

type t =
  { globals : Global_decl.t list
  ; externs : Label_name.t list
  ; text_section : Register.t Basic_block.t list
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
  pp_section fmt "text" text_section ~align:None ~f:(fun fmt bb ->
    (* Labels starting with "." are local to a function. Other labels are function names.
       Add an extra newline in between functions to help readability. *)
    if not (String.is_prefix ~prefix:"." (Label_name.to_string bb.label))
    then Format.pp_print_newline fmt ();
    Basic_block.pp fmt bb);
  pp_section fmt "rodata" rodata_section ~align:(Some 8) ~f:Data_decl.pp;
  pp_section fmt "bss" bss_section ~align:(Some 8) ~f:Bss_decl.pp
;;
