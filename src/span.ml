open Import

module Pos = struct
  module T = struct
    type t =
      { line : int
      ; col : int
      }
    [@@deriving equal, compare, hash]

    let dummy = { line = 0; col = 0 }
    let to_string_terse { line; col } = sprintf "%d:%d" line col
    let sexp_of_t t = Sexp.Atom (to_string_terse t)
    let of_string_terse str = Scanf.sscanf str "%d:%d" (fun line col -> { line; col })

    let t_of_sexp = function
      | Sexp.Atom str -> of_string_terse str
      | List _ -> raise_s [%message "Span.Pos.t_of_sexp: parse failed"]
    ;;
  end

  include T
  include Comparable.Make (T)

  let of_lexing_position { Lexing.pos_lnum; pos_cnum; pos_bol; _ } =
    { line = pos_lnum; col = pos_cnum - pos_bol + 1 }
  ;;
end

type t =
  { start : Pos.t
  ; end_ : Pos.t
  }
[@@deriving equal, compare, hash]

let dummy = { start = Pos.dummy; end_ = Pos.dummy }

let of_loc ((pos1, pos2) : Lexing.position * Lexing.position) =
  { start = Pos.of_lexing_position pos1; end_ = Pos.of_lexing_position pos2 }
;;

let combine span1 span2 =
  { start = Pos.min span1.start span2.start; end_ = Pos.max span1.end_ span2.end_ }
;;

let first_to_last spans = combine (List.hd_exn spans) (List.last_exn spans)

let to_string_terse { start; end_; _ } =
  sprintf "%d:%d-%d:%d" start.line start.col end_.line end_.col
;;

let sexp_of_t t = Sexp.Atom (to_string_terse t)

let of_string_terse str =
  Scanf.sscanf str "%d:%d-%d:%d" (fun line col line' col' ->
    { start = { line; col }; end_ = { line = line'; col = col' } })
;;

let t_of_sexp = function
  | Sexp.Atom str -> of_string_terse str
  | List _ -> raise_s [%message "Span.t_of_sexp: parse failed"]
;;
