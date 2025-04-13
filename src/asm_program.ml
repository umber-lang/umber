open Import
open Names

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
  type t [@@deriving sexp_of]

  include Stringable.S with type t := t
  include Hashable.S with type t := t

  val of_mir_name : Mir_name.t -> t
  val of_extern_name : Extern_name.t -> t
  val to_mir_name : t -> Mir_name.t
end = struct
  module T = struct
    type t = string [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)

  let of_string = Fn.id
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
  [@@deriving sexp_of]
end

module Asm_literal = struct
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
  include Hashable.Make (T)

  let pp fmt t = Format.pp_print_string fmt (String.lowercase (Variants.to_name t))
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
    | Memory of Size.t * 'reg memory_expr
    | Global of Label_name.t * Global_kind.t
    | Constant of Asm_literal.t

  and 'reg memory_expr =
    | Value of 'reg t
    | Add of 'reg memory_expr * 'reg memory_expr
  [@@deriving sexp_of]

  let rec pp fmt t =
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
    | Global (name, kind) ->
      (match kind with
       | Extern_proc ->
         (* "wrt ..plt" is needed for PIE (Position Independent Executables). *)
         Format.fprintf fmt !"%{Label_name} wrt ..plt" name
       | Other -> Format.fprintf fmt !"%{Label_name}" name)
    | Constant literal -> Asm_literal.pp fmt literal

  and pp_memory_expr fmt memory_expr =
    match memory_expr with
    | Value t -> pp fmt t
    | Add (lhs, rhs) ->
      pp_memory_expr fmt lhs;
      Format.pp_print_string fmt " + ";
      pp_memory_expr fmt rhs
  ;;

  let mem_offset value size i = Memory (size, Add (Value value, Value (Constant (Int i))))

  let rec fold_map_registers t ~init ~f =
    match t with
    | Register reg ->
      let init, reg = f init reg in
      init, Register reg
    | (Global _ | Constant _) as t -> init, t
    | Memory (size, mem) ->
      let init, mem = fold_map_registers_memory_expr mem ~init ~f in
      init, Memory (size, mem)

  and fold_map_registers_memory_expr mem ~init ~f =
    match mem with
    | Value t ->
      let init, t = fold_map_registers t ~init ~f in
      init, Value t
    | Add (lhs, rhs) ->
      let init, lhs = fold_map_registers_memory_expr lhs ~init ~f in
      let init, rhs = fold_map_registers_memory_expr rhs ~init ~f in
      init, Add (lhs, rhs)
  ;;
end

(* TODO: 64-bit values only work with mov - they'll get silently truncated
   otherwise. They need to be loaded into a register first. Ideally the types shouldn't
   allow any Instr except Mov to have a literal value. *)
(* TODO: Might make sense to abstract this a little more e.g. group the jumps into one
   variant. Might make some of the code easier to handle. *)
module Instr = struct
  type 'reg t =
    | And of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Call of 'reg Value.t
    | Cmp of 'reg Value.t * 'reg Value.t
    | Jmp of Label_name.t
    | Jnz of Label_name.t
    | Jz of Label_name.t
    | Lea of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Mov of
        { dst : 'reg Value.t
        ; src : 'reg Value.t
        }
    | Ret
    | Setz of 'reg Value.t
    | Test of 'reg Value.t * 'reg Value.t
  [@@deriving variants, sexp_of]

  let pp fmt t =
    let args =
      match t with
      | And { dst; src } | Mov { dst; src } | Lea { dst; src } -> [ dst; src ]
      | Cmp (a, b) | Test (a, b) -> [ a; b ]
      | Call x | Setz x -> [ x ]
      | Jmp label | Jnz label | Jz label -> [ Global (label, Other) ]
      | Ret -> []
    in
    pp_line fmt (String.lowercase (Variants.to_name t)) args ~f:Value.pp
  ;;

  (* TODO: Consider properly separating terminators and non-terminators - each block can
     only have 1 terminator, and terminators can't be present in the middle of the block.
     Maybe have Instr be a variant of Terminator or Non_terminator. *)
  let is_terminator = function
    | Jmp _ | Jnz _ | Jz _ | Ret -> true
    | And _ | Mov _ | Lea _ | Cmp _ | Test _ | Call _ | Setz _ -> false
  ;;

  let fold_map_args t ~init ~f =
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
    | Call x ->
      let init, x = f init x in
      init, Call x
    | Setz x ->
      let init, x = f init x in
      init, Setz x
    | Jmp label -> handle_jump init label ~f ~mk:jmp
    | Jnz label -> handle_jump init label ~f ~mk:jnz
    | Jz label -> handle_jump init label ~f ~mk:jz
    | Ret -> init, Ret
  ;;
end

module Instr_group = struct
  type t =
    { label : Label_name.t
    ; instrs : Register.t Instr.t list
    }

  let pp fmt { label; instrs } =
    pp_label fmt label;
    List.iter instrs ~f:(Instr.pp fmt)
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
