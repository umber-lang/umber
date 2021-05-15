let names_sexp = {|
((current_path "Std(d)")
 (toplevel
  ((names
    ((True (Local ((typ (Scheme (Type_app (() Bool) ()))))))
     (False (Local ((typ (Scheme (Type_app (() Bool) ()))))))))
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
                       (Function (Type_app (() Int) ())
                        (Type_app (() Float) ())))))))
                  (print
                   (Local
                    ((typ
                      (Scheme
                       (Function (Type_app (() String) ()) (Tuple ()))))
                     (extern_name @Io_print_string))))))
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
                             (Type_app ((Std Prelude List) List) ((Var a))))))))
                        (Cons
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function
                               (Type_app ((Std Prelude List) List) ((Var a)))
                               (Type_app ((Std Prelude List) List) ((Var a))))))))))))
                      (types
                       ((List
                         ((Local
                           ((a)
                            (Variants
                             ((Nil ())
                              (Cons
                               ((Var a)
                                (Type_app ((Std Prelude List) List)
                                 ((Var a)))))))))))))
                      (modules ())))))
                  (Operators
                   (Local
                    (()
                     ((names
                       ((*
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Int) ())
                              (Function (Type_app (() Int) ())
                               (Type_app (() Int) ())))))
                           (fixity (Left 7)))))
                        (+
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Int) ())
                              (Function (Type_app (() Int) ())
                               (Type_app (() Int) ())))))
                           (fixity (Left 6)))))
                        (-
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Int) ())
                              (Function (Type_app (() Int) ())
                               (Type_app (() Int) ())))))
                           (fixity (Left 6)))))
                        (.
                         (Local
                          ((typ
                            (Scheme
                             (Function (Function (Var b) (Var c))
                              (Function (Function (Var a) (Var b))
                               (Function (Var a) (Var c))))))
                           (fixity (Right 9)))))
                        (";"
                         (Local
                          ((typ
                            (Scheme
                             (Function (Tuple ()) (Function (Var a) (Var a)))))
                           (fixity (Left 0)))))
                        (<
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (>
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (^
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Int) ())
                              (Function (Type_app (() Int) ())
                               (Type_app (() Int) ())))))
                           (fixity (Right 8)))))
                        (!=
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (&&
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Bool) ())
                              (Function (Type_app (() Bool) ())
                               (Type_app (() Bool) ())))))
                           (fixity (Left 3)))))
                        (::
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function
                               (Type_app ((Std Prelude) List) ((Var a)))
                               (Type_app ((Std Prelude) List) ((Var a)))))))
                           (fixity (Right 5)))))
                        (<=
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (==
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (>=
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Var a) (Type_app (() Bool) ())))))
                           (fixity (Non_assoc 4)))))
                        (|>
                         (Local
                          ((typ
                            (Scheme
                             (Function (Var a)
                              (Function (Function (Var a) (Var b)) (Var b)))))
                           (fixity (Left 0)))))
                        (||
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Bool) ())
                              (Function (Type_app (() Bool) ())
                               (Type_app (() Bool) ())))))
                           (fixity (Left 2)))))
                        (not
                         (Local
                          ((typ
                            (Scheme
                             (Function (Type_app (() Bool) ())
                              (Type_app (() Bool) ())))))))))
                      (types ()) (modules ())))))))))
              ((names ()) (types ())
               (modules
                ((List
                  (Local
                   (()
                    ((names
                      ((Nil
                        (Local
                         ((typ
                           (Scheme
                            (Type_app ((Std Prelude List) List) ((Var a))))))))
                       (Cons
                        (Local
                         ((typ
                           (Scheme
                            (Function (Var a)
                             (Function
                              (Type_app ((Std Prelude List) List) ((Var a)))
                              (Type_app ((Std Prelude List) List) ((Var a))))))))))))
                     (types
                      ((List
                        ((Local
                          ((a)
                           (Variants
                            ((Nil ())
                             (Cons
                              ((Var a)
                               (Type_app ((Std Prelude List) List) ((Var a)))))))))))))
                     (modules ())))))
                 (Operators
                  (Local (() ((names ()) (types ()) (modules ()))))))))))))))))))))))
|}
