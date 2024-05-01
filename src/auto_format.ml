open! Import

(* See https://www.researchgate.net/publication/2629249_Strictly_Pretty *)

module Document = struct
  type t =
    | Empty
    | Text of string
    | Break
    | Concat of t * t
    | Indent of int * t
    | Group of t
  [@@deriving variants]

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
    | _ -> t ^^ Break ^^ t'
  ;;

  let separated ts = concat_all (List.intersperse ts ~sep:Break)
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
       | Text str -> fits ~length:(length - String.length str) fragments
       | Break -> fits ~length:(length - 1) fragments
       | Concat (left, right) ->
         fits
           ~length
           ({ indent; mode; doc = left } :: { indent; mode; doc = right } :: fragments)
       | Indent (indent_to_add, doc) ->
         fits ~length ({ indent = indent + indent_to_add; mode; doc } :: fragments)
       | Group doc -> fits ~length ({ indent; mode; doc } :: fragments)))
;;

let format =
  let rec format fragments ~max_line_length ~used_length =
    match fragments with
    | [] -> Sequence.empty
    | { indent; mode; doc } :: fragments ->
      (match doc with
       | Empty -> format fragments ~max_line_length ~used_length
       | Text text ->
         Sequence.append
           (Sequence.singleton text)
           (format
              fragments
              ~max_line_length
              ~used_length:(used_length + String.length text))
       | Break ->
         (match mode with
          | `Flat ->
            Sequence.append
              (Sequence.singleton " ")
              (format fragments ~max_line_length ~used_length:(used_length + 1))
          | `Break ->
            Sequence.append
              (Sequence.singleton ("\n" ^ String.make indent ' '))
              (format fragments ~max_line_length ~used_length:indent))
       | Concat (left, right) ->
         format
           ~max_line_length
           ~used_length
           ({ indent; mode; doc = left } :: { indent; mode; doc = right } :: fragments)
       | Indent (indent_to_add, doc) ->
         format
           ~max_line_length
           ~used_length
           ({ indent = indent + indent_to_add; mode; doc } :: fragments)
       | Group doc ->
         let mode =
           if fits
                ({ indent; mode = `Flat; doc } :: fragments)
                ~length:(max_line_length - used_length)
           then `Flat
           else `Break
         in
         format ~max_line_length ~used_length ({ indent; mode; doc } :: fragments))
  in
  fun doc ~max_line_length ->
    format [ { indent = 0; mode = `Flat; doc } ] ~max_line_length ~used_length:0
;;

let print doc ~max_line_length =
  format doc ~max_line_length |> Sequence.iter ~f:print_string
;;

let%expect_test _ =
  let open Document in
  let example =
    Group
      (Text "begin"
       ^^ Indent
            ( 2
            , Break
              ^^ Group
                   (Document.separated (List.init 3 ~f:(const (Document.Text "stmt;"))))
            )
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
