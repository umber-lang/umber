open! Import

(* See https://www.researchgate.net/publication/2629249_Strictly_Pretty *)

module Document = struct
  type t =
    | Empty
    | Text of string
    | Break
    | Force_break
    | Concat of t * t
    | Indent of int * t
    | Group of t
  [@@deriving sexp, variants]

  let rec concat_all = function
    | [] -> Empty
    | [ t ] -> t
    | t :: ts -> Concat (t, concat_all ts)
  ;;

  let ( ^^ ) t t' =
    match t, t' with
    | Empty, t' -> t'
    | t, Empty -> t
    | _ -> Concat (t, t')
  ;;

  let ( ^| ) t t' =
    match t, t' with
    | Empty, t' -> t'
    | t, Empty -> t
    | _ -> t ^^ Break ^^ t'
  ;;

  let separated ?(sep = Break) ts = concat_all (List.intersperse ts ~sep)
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
       | Force_break -> false
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
       | Break ->
         (match mode with
          | `Flat ->
            Sequence.append
              (Sequence.singleton " ")
              (format fragments ~used_length:(used_length + 1))
          | `Break -> format_break indent fragments)
       | Force_break -> format_break indent fragments
       | Concat (left, right) ->
         format
           ~used_length
           ({ indent; mode; doc = left } :: { indent; mode; doc = right } :: fragments)
       | Indent (indent_to_add, doc) ->
         format ~used_length ({ indent = indent + indent_to_add; mode; doc } :: fragments)
       | Group doc ->
         let mode =
           if fits
                [ { indent; mode = `Flat; doc } ]
                ~length:(max_line_length - used_length)
           then `Flat
           else `Break
         in
         format ~used_length ({ indent; mode; doc } :: fragments))
  and format_break indent fragments =
    (* TODO: Extra spaces on a blank line should be removed. Maybe we could "buffer" the
       spaces and then output them later (once it's confirmed that non-space characters
       will appear before a newline)? *)
    Sequence.append
      (Sequence.singleton ("\n" ^ String.make indent ' '))
      (format fragments ~used_length:indent)
  in
  format [ { indent = 0; mode = `Flat; doc = Group doc } ] ~used_length:0
;;

let print doc ~max_line_length =
  format doc ~max_line_length |> Sequence.iter ~f:print_string;
  Out_channel.newline stdout
;;

let%test_module _ =
  (module struct
    open Document

    let%expect_test "basic example with different line lengths" =
      let example =
        Group
          (Text "begin"
           ^^ Indent
                (2, Break ^^ Group (separated (List.init 3 ~f:(const (Text "stmt;")))))
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
      [%expect
        {|
        begin
          stmt;
          stmt;
          stmt;
        end |}]
    ;;

    let%expect_test "let binding with if statement" =
      let example =
        (t_of_sexp << Sexp.of_string)
          {|
          (Concat (Text let)
           (Concat Break
            (Concat (Text _)
             (Concat Break
              (Concat (Text =)
               (Indent 2
                (Concat Break
                 (Group
                  (Group
                   (Concat (Text if)
                    (Concat (Indent 2 (Concat Break (Group (Text false))))
                     (Concat Break
                      (Concat (Text then)
                       (Concat (Indent 2 (Concat Break (Group (Text 1.))))
                        (Concat Break
                         (Concat (Text else)
                          (Indent 2 (Concat Break (Group (Text 2.5))))))))))))))))))))
      |}
      in
      print example ~max_line_length:33;
      [%expect {| let _ = if false then 1. else 2.5 |}];
      print example ~max_line_length:32;
      [%expect {|
        let
        _
        =
          if false then 1. else 2.5 |}]
    ;;
  end)
;;
