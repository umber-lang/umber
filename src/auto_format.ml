open! Import

(* See https://www.researchgate.net/publication/2629249_Strictly_Pretty *)

module Document = struct
  type t =
    | Empty
    | Text of string
    | Break of string
    | Concat of t * t
    | Indent of int * t
    | Group of t
  [@@deriving variants]

  let space = Break " "

  (* FIXME: This won't work due to indentation. Maybe add [Force_break]? *)
  let line_break = Break "\n"

  let rec concat_all = function
    | [] -> Empty
    | [ t ] -> t
    | t :: ts -> Concat (t, concat_all ts)
  ;;

  let ( ^^ ) t t' = Concat (t, t')

  let ( ^| ) t t' =
    match t, t' with
    | Empty, t' -> t'
    | t, Empty -> t
    | _ -> t ^^ space ^^ t'
  ;;

  let separated ?(sep = space) ts = concat_all (List.intersperse ts ~sep)
end

type fragment =
  { indent : int
  ; mode : [ `Flat | `Break ]
  ; doc : Document.t
  }

let rec fits fragments ~length =
  if length < 0
  then false
  else (
    match fragments with
    | [] -> true
    | { indent; mode; doc } :: fragments ->
      (match doc with
       | Empty -> fits ~length fragments
       | Text str | Break str -> fits ~length:(length - String.length str) fragments
       | Concat (left, right) ->
         fits
           ~length
           ({ indent; mode; doc = left } :: { indent; mode; doc = right } :: fragments)
       | Indent (indent_to_add, doc) ->
         fits ~length ({ indent = indent + indent_to_add; mode; doc } :: fragments)
       | Group doc -> fits ~length ({ indent; mode; doc } :: fragments)))
;;

let format doc ~max_line_length =
  let rec format fragments ~used_length =
    match fragments with
    | [] -> Sequence.empty
    | { indent; mode; doc } :: fragments ->
      (match doc with
       | Empty -> format fragments ~used_length
       | Text text ->
         Sequence.append
           (Sequence.singleton text)
           (format fragments ~used_length:(used_length + String.length text))
       | Break sep ->
         (match mode with
          | `Flat ->
            Sequence.append
              (Sequence.singleton sep)
              (format fragments ~used_length:(used_length + String.length sep))
          | `Break ->
            Sequence.append
              (Sequence.singleton ("\n" ^ String.make indent ' '))
              (format fragments ~used_length:indent))
       | Concat (left, right) ->
         format
           ~used_length
           ({ indent; mode; doc = left } :: { indent; mode; doc = right } :: fragments)
       | Indent (indent_to_add, doc) ->
         format ~used_length ({ indent = indent + indent_to_add; mode; doc } :: fragments)
       | Group doc ->
         let mode =
           if fits
                ({ indent; mode = `Flat; doc } :: fragments)
                ~length:(max_line_length - used_length)
           then `Flat
           else `Break
         in
         format ~used_length ({ indent; mode; doc } :: fragments))
  in
  format [ { indent = 0; mode = `Flat; doc } ] ~used_length:0
;;

let print doc ~max_line_length =
  format doc ~max_line_length |> Sequence.iter ~f:print_string
;;

let%expect_test _ =
  let open Document in
  let example =
    Group
      (Text "begin"
       ^^ Indent (2, space ^^ Group (separated (List.init 3 ~f:(const (Text "stmt;")))))
       ^| Text "end")
  in
  print example ~max_line_length:50;
  [%expect {| begin stmt; stmt; stmt; end |}];
  print example ~max_line_length:25;
  [%expect {|
    begin
      stmt; stmt; stmt;
    end |}];
  print example ~max_line_length:10;
  [%expect {|
    begin
      stmt;
      stmt;
      stmt;
    end |}]
;;
