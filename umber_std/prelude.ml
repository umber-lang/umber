open! Core

(*$
  open! Core

  let compile_and_print ~name ~is_sexp target =
    print_endline [%string "\nlet %{name} = {|"];
    Umberboot.compile_and_print
      ~filename:"Std/Prelude.um"
      ~no_std:true
      ~parent:(Umber.Ast.Module_name.of_string_exn "Std")
      [ target ];
    print_endline "|}";
    if is_sexp then print_endline "|> Sexp.of_string"
  ;;
$*)

(*$ let () = compile_and_print ~name:"names" ~is_sexp:true Names *)
let names = {|
((current_path "Std(d)")
 (toplevel
  ((names
    ((True (Local ((typ (Scheme (Type_app Bool ()))))))
     (False (Local ((typ (Scheme (Type_app Bool ()))))))))
   (types
    ((Int ((Local (() Abstract))))
     (Bool ((Local (() (Variants ((False ()) (True ())))))))
     (Char ((Local (() Abstract)))) (Float ((Local (() Abstract))))
     (String ((Local (() Abstract))))))
   (modules
    ((Std
      (Local
       (()
        ((names ()) (types ())
         (modules
          ((Prelude
            (Local
             ((((names
                 ((* (Imported ((Std Prelude Operators) *)))
                  (+ (Imported ((Std Prelude Operators) +)))
                  (- (Imported ((Std Prelude Operators) -)))
                  (. (Imported ((Std Prelude Operators) .)))
                  (";" (Imported ((Std Prelude Operators) ";")))
                  (< (Imported ((Std Prelude Operators) <)))
                  (> (Imported ((Std Prelude Operators) >)))
                  (^ (Imported ((Std Prelude Operators) ^)))
                  (!= (Imported ((Std Prelude Operators) !=)))
                  (&& (Imported ((Std Prelude Operators) &&)))
                  (:: (Imported ((Std Prelude Operators) ::)))
                  (<= (Imported ((Std Prelude Operators) <=)))
                  (== (Imported ((Std Prelude Operators) ==)))
                  (>= (Imported ((Std Prelude Operators) >=)))
                  (|> (Imported ((Std Prelude Operators) |>)))
                  (|| (Imported ((Std Prelude Operators) ||)))
                  (Nil (Imported ((Std Prelude List) Nil)))
                  (not (Imported ((Std Prelude Operators) not)))
                  (Cons (Imported ((Std Prelude List) Cons)))
                  (sqrt
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app Int ())) (Type_app Float ())))))))
                  (print
                   (Local
                    ((typ
                      (Scheme (Function ((Type_app String ())) (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name %print_endline))))))
                (types ())
                (modules
                 ((List
                   (Local
                    (()
                     ((names
                       ((Nil
                         (Local
                          ((typ
                            (Scheme
                             (Type_app Std.Prelude.List.List ((Var a))))))))
                        (Cons
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a))))
                              (Type_app Std.Prelude.List.List ((Var a)))))))))))
                      (types
                       ((List
                         ((Local
                           ((a)
                            (Variants
                             ((Nil ())
                              (Cons
                               ((Var a)
                                (Type_app Std.Prelude.List.List ((Var a)))))))))))))
                      (modules ())))))
                  (Operators
                   (Local
                    (()
                     ((names
                       ((*
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Int ()) (Type_app Int ()))
                              (Type_app Int ()))))
                           (fixity (Left 7)))))
                        (+
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Int ()) (Type_app Int ()))
                              (Type_app Int ()))))
                           (fixity (Left 6)))))
                        (-
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Int ()) (Type_app Int ()))
                              (Type_app Int ()))))
                           (fixity (Left 6)))))
                        (.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Function ((Var b)) (Var c))
                               (Function ((Var a)) (Var b)) (Var a))
                              (Var c))))
                           (fixity (Right 9)))))
                        (";"
                         (Local
                          ((typ
                            (Scheme (Function ((Tuple ()) (Var a)) (Var a))))
                           (fixity (Left 0)))))
                        (<
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (>
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (^
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Int ()) (Type_app Int ()))
                              (Type_app Int ()))))
                           (fixity (Right 8)))))
                        (!=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (&&
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Bool ()) (Type_app Bool ()))
                              (Type_app Bool ()))))
                           (fixity (Left 3)))))
                        (::
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Var a) (Type_app Std.Prelude.List ((Var a))))
                              (Type_app Std.Prelude.List ((Var a))))))
                           (fixity (Right 5)))))
                        (<=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (==
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (>=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) (Type_app Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (|>
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Function ((Var a)) (Var b)))
                              (Var b))))
                           (fixity (Left 0)))))
                        (||
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Bool ()) (Type_app Bool ()))
                              (Type_app Bool ()))))
                           (fixity (Left 2)))))
                        (not
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Bool ()))
                              (Type_app Bool ())))))))))
                      (types ()) (modules ())))))))))
              ((names
                ((* (Imported ((Std Prelude Operators) *)))
                 (+ (Imported ((Std Prelude Operators) +)))
                 (- (Imported ((Std Prelude Operators) -)))
                 (. (Imported ((Std Prelude Operators) .)))
                 (";" (Imported ((Std Prelude Operators) ";")))
                 (< (Imported ((Std Prelude Operators) <)))
                 (> (Imported ((Std Prelude Operators) >)))
                 (^ (Imported ((Std Prelude Operators) ^)))
                 (!= (Imported ((Std Prelude Operators) !=)))
                 (&& (Imported ((Std Prelude Operators) &&)))
                 (:: (Imported ((Std Prelude Operators) ::)))
                 (<= (Imported ((Std Prelude Operators) <=)))
                 (== (Imported ((Std Prelude Operators) ==)))
                 (>= (Imported ((Std Prelude Operators) >=)))
                 (|> (Imported ((Std Prelude Operators) |>)))
                 (|| (Imported ((Std Prelude Operators) ||)))
                 (not (Imported ((Std Prelude Operators) not)))
                 (sqrt
                  (Local
                   ((typ
                     (Scheme
                      (Function ((Type_app Int ())) (Type_app Float ()))))
                    (type_source Extern_declared) (extern_name %int_sqrt))))
                 (print
                  (Local
                   ((typ
                     (Scheme (Function ((Type_app String ())) (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name %print_endline))))))
               (types ())
               (modules
                ((List
                  (Local
                   (()
                    ((names
                      ((Nil
                        (Local
                         ((typ
                           (Scheme
                            (Type_app Std.Prelude.List.List ((Var a))))))))
                       (Cons
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Var a)
                              (Type_app Std.Prelude.List.List ((Var a))))
                             (Type_app Std.Prelude.List.List ((Var a)))))))))))
                     (types
                      ((List
                        ((Local
                          ((a)
                           (Variants
                            ((Nil ())
                             (Cons
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a)))))))))))))
                     (modules ())))))
                 (Operators
                  (Local
                   (()
                    ((names
                      ((*
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Int ()) (Type_app Int ()))
                             (Type_app Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name %int_mul))))
                       (+
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Int ()) (Type_app Int ()))
                             (Type_app Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name %int_add))))
                       (-
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Int ()) (Type_app Int ()))
                             (Type_app Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name %int_sub))))
                       (.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Function ((Var b)) (Var c))
                              (Function ((Var a)) (Var b)) (Var a))
                             (Var c))))
                          (fixity (Right 9)))))
                       (";"
                        (Local
                         ((typ
                           (Scheme (Function ((Tuple ()) (Var a)) (Var a))))
                          (fixity (Left 0)))))
                       (<
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %lt))))
                       (>
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %gt))))
                       (^
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Int ()) (Type_app Int ()))
                             (Type_app Int ()))))
                          (type_source Extern_declared) (fixity (Right 8))
                          (extern_name %int_pow))))
                       (!=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %neq))))
                       (&&
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ()) (Type_app Bool ()))
                             (Type_app Bool ()))))
                          (fixity (Left 3)))))
                       (::
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Var a) (Type_app Std.Prelude.List ((Var a))))
                             (Type_app Std.Prelude.List ((Var a))))))
                          (fixity (Right 5)))))
                       (<=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %lte))))
                       (==
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %eq))))
                       (>=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name %gte))))
                       (|>
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Function ((Var a)) (Var b)))
                             (Var b))))
                          (fixity (Left 0)))))
                       (||
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ()) (Type_app Bool ()))
                             (Type_app Bool ()))))
                          (fixity (Left 2)))))
                       (not
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ()))
                             (Type_app Bool ()))))
                          (type_source Let_inferred))))))
                     (types ()) (modules ()))))))))))))))))))))))
|}
|> Sexp.of_string
(*$*)

(* let () = compile_and_print ~name:"llvm" ~is_sexp:false Llvm *)
let llvm = "placeholder"
(**)
