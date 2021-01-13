open Import

module Pos = struct
  module T = struct
    type t =
      { line : int
      ; col : int
      }
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

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

exception Different_files of Filename.t * Filename.t

let check_filenames file1 file2 =
  if String.(file1 <> file2) then raise (Different_files (file1, file2))
;;

let of_lexing_positions ((pos1, pos2) : Lexing.position * Lexing.position) =
  check_filenames pos1.pos_fname pos2.pos_fname;
  { file = pos1.pos_fname
  ; start = Pos.of_lexing_position pos1
  ; end_ = Pos.of_lexing_position pos2
  }
;;

let combine span1 span2 =
  check_filenames span1.file span2.file;
  { span1 with
    start = Pos.min span1.start span2.start
  ; end_ = Pos.max span1.end_ span2.end_
  }
;;

let first_to_last first rest = combine first (List.last_exn rest)
