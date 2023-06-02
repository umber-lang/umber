open Import

module Kind = struct
  type t =
    | Syntax_error
    | Name_error
    | Type_error
    | Mir_error
    | Codegen_error
    | Other
  [@@deriving sexp]
end

type t =
  { kind : Kind.t
  ; msg : Sexp.t
  ; filename : Filename.t option [@sexp.option]
  ; span : Span.t option [@sexp.option]
  ; exn : Exn.t option [@sexp.option]
  ; backtrace : Backtrace.t option [@sexp.option]
  }
[@@deriving sexp_of]

exception Compilation_error of t [@@deriving sexp_of]

let create ?filename ?span ?exn ~msg kind =
  { kind; msg; filename; span; exn; backtrace = None }
;;

let try_with ?filename ?span kind ~msg f =
  try Ok (f ()) with
  | exn -> Error { kind; msg; filename; span; exn = Some exn; backtrace = None }
;;

let try_with' f =
  try Ok (f ()) with
  | Compilation_error error ->
    Error { error with backtrace = Some (Backtrace.Exn.most_recent ()) }
;;

let raise ?filename ?span ?exn ~msg kind =
  raise (Compilation_error { kind; msg; filename; span; exn; backtrace = None })
;;
