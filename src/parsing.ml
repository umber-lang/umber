open Import
open Sedlexing

type token = [%import: Parser.token] [@@deriving sexp]

let fprint_s sexp ~out =
  Sexp.to_string_hum sexp |> Out_channel.output_string out;
  Out_channel.newline out
;;

(* Code from:
   https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)

module I = Parser.MenhirInterpreter

module Terminal = struct
  type 'a t = [%import: 'a Parser.MenhirInterpreter.terminal] [@@deriving sexp_of]
end

module Nonterminal = struct
  type 'a t = [%import: 'a Parser.MenhirInterpreter.nonterminal] [@@deriving sexp_of]
end

let handle_syntax_error f =
  try Ok (f ()) with
  | Lexer.Syntax_error (pos, msg) ->
    Error
      (Compilation_error.create
         Syntax_error
         ~span:(Span.of_pos pos)
         ~msg:(Atom (Ustring.to_string msg)))
;;

let rec lex ~print_tokens_to lexbuf lexer =
  let token = Lexer.read lexer lexbuf in
  sexp_of_token token |> fprint_s ~out:print_tokens_to;
  match token with
  | EOF -> ()
  | _ -> lex ~print_tokens_to lexbuf lexer
;;

let try_lex ~print_tokens_to lexbuf lexer =
  handle_syntax_error (fun () -> lex ~print_tokens_to lexbuf lexer)
;;

let parse ?print_tokens_to ?(full_lex = false) lexbuf =
  let lex_remaining ~print_tokens_to lexbuf lexer =
    match print_tokens_to with
    | Some print_tokens_to when full_lex -> lex ~print_tokens_to lexbuf lexer
    | _ -> ()
  in
  let last_prod = ref None in
  let rec parse ?print_tokens_to last_token lexbuf checkpoint lexer =
    match checkpoint with
    | I.InputNeeded _env ->
      let token = Lexer.read lexer lexbuf in
      Option.iter print_tokens_to ~f:(fun out -> sexp_of_token token |> fprint_s ~out);
      let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
      let checkpoint = I.offer checkpoint (token, start_pos, end_pos) in
      parse ?print_tokens_to token lexbuf checkpoint lexer
    | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint lexer
    | I.AboutToReduce (_, prod) ->
      last_prod := Some prod;
      let checkpoint = I.resume checkpoint in
      parse ?print_tokens_to last_token lexbuf checkpoint lexer
    | I.HandlingError env ->
      (* TODO: you can actually resume the parser here, should look into that *)
      let error () =
        lex_remaining ~print_tokens_to lexbuf lexer;
        let state = I.current_state_number env in
        let prod_msg =
          match !last_prod with
          | Some prod ->
            let (X symbol) = I.lhs prod in
            let empty_sexp _ = Sexp.List [] in
            let str =
              match symbol with
              | T terminal -> Terminal.sexp_of_t empty_sexp terminal
              | N nonterminal -> Nonterminal.sexp_of_t empty_sexp nonterminal
            in
            sprintf ". (Last production: %s)" (Sexp.to_string str)
          | None -> ""
        in
        Lexer.syntax_error
          ~msg:(sprintf "Parser error in state %d%s" state prod_msg)
          lexbuf
      in
      (*(match last_token with
      | INDENT | DEDENT | LINE_SEP ->
        (match I.pop env with
        | Some env ->
          let checkpoint = I.input_needed env in
          print_s [%message "Yeeting an error away" (last_token : token)];
          (* TODO: probably remove this - this is such a hack *)
          parse ?print_tokens_to last_token lexbuf checkpoint lexer
        | None -> error ())
      | _ -> error ())*)
      error ()
    | I.Accepted v -> v
    | I.Rejected ->
      lex_remaining ~print_tokens_to lexbuf lexer;
      Lexer.syntax_error ~msg:"Invalid syntax (parser rejected the input)" lexbuf
  in
  let start, _ = lexing_positions lexbuf in
  Module.with_filename
    (parse ?print_tokens_to EOF lexbuf (Parser.Incremental.prog start) (Lexer.create ()))
    start.pos_fname
;;

let try_parse ?print_tokens_to ~full_lex lexbuf =
  handle_syntax_error (fun () -> parse ?print_tokens_to ~full_lex lexbuf)
;;

let with_file filename ~f =
  In_channel.with_file filename ~f:(fun inx ->
    let lexbuf = Utf8.from_channel inx in
    Sedlexing.set_filename lexbuf filename;
    f lexbuf)
;;

let lex_file ~print_tokens_to =
  with_file ~f:(fun lexbuf -> try_lex ~print_tokens_to lexbuf (Lexer.create ()))
;;

let parse_file ?print_tokens_to ?(full_lex = false) =
  with_file ~f:(try_parse ?print_tokens_to ~full_lex)
;;
