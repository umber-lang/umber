open Core_kernel

module Pos = struct
  type t = private int * int

  (* Numbers, not the number/letter combo. Can convert easily. Origin is bottom left. *)
end

module Piece = struct
  module Color = struct
    type t =
      | Black
      | White
    [@@deriving equal, compare, hash]
  end

  module Type = struct
    type t =
      | Pawn
      | Rook
      | Knight
      | Bishop
      | Queen
      | King
    [@@deriving equal, compare, hash]
  end

  type t = Color.t * Type.t [@@deriving equal, compare, hash]
end

(*
module Board : sig
  type t

  val starting : t
  val piece_at : t -> Pos.t -> Piece.t option
  val move : t -> from:Pos.t -> to_:Pos.t -> t option
end = struct
  type t = (Pos.t, Piece.t) Map.t

  let starting = Map.of_alist_exn (module Piece) [ (0, 0), (White, Rook) ]
  let piece_at = Map.find

  let move board ~from ~to_ =
    (* Check that a piece exists, that the move can be done in general,
       and that there is not a piece of the same colour in the way *)
    let open Option.Let_syntax in
    let%bind (piece_color, piece_type) = piece_at board from in
    match piece_type with
    | Pawn -> ... etc.
  ;;
end*)
