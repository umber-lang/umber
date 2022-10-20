open! Core

(*$
  open! Core

  let compile_and_print ~name ~is_sexp target =
    print_endline [%string "let %{name} = {|";
    Umberboot.compile_and_print ~filename:"Prelude.um" [ target ]
    print_endline "|}";
    if is_sexp then print_endline "|> Sexp.of_string"
$*)

(*$ let () = compile_and_print ~name:"names" ~is_sexp:true  Names  *)
(*$*)