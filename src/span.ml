open Import

module Pos = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving equal, compare, hash, sexp]

  let of_lexing_position { Lexing.pos_lnum; pos_cnum; pos_bol; _ } =
    { line = pos_lnum; col = pos_cnum - pos_bol + 1 }
  ;;
end

type t =
  { file : Filename.t
  ; start : Pos.t
  ; end_ : Pos.t
  }
[@@deriving equal, compare, hash, sexp]

let of_lexing_positions ((pos1, pos2) : Lexing.position * Lexing.position) =
  let file =
    if String.(pos1.pos_fname = pos2.pos_fname)
    then pos1.pos_fname
    else
      compiler_bug
        [%message "Lexing positions from different files" pos1.pos_fname pos2.pos_fname]
  in
  { file; start = Pos.of_lexing_position pos1; end_ = Pos.of_lexing_position pos2 }
;;
