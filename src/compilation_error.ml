open Import

module Kind = struct
  type t =
    | Syntax_error
    | Name_error
    | Type_error
    | Mir_error
    | Other
  [@@deriving sexp]
end

type t =
  { kind : Kind.t
  ; msg : Sexp.t
  ; filename : Filename.t option [@sexp.option]
  ; span : Span.t option [@sexp.option]
  ; exn : Exn.t option [@sexp.option]
  }
[@@deriving sexp_of]

exception Compilation_error of t [@@deriving sexp_of]

let create ?filename ?span ?exn ~msg kind = { kind; msg; filename; span; exn }

let raise ?filename ?span ?exn ~msg kind =
  raise (Compilation_error { kind; msg; filename; span; exn })
;;
