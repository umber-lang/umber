let names_sexp = {|
((Std)
 ((names
   ((True
     (Local
      ((typ (Scheme (Type_app (() Bool) ()))) (type_source Val_declared)
       (fixity ()))))
    (False
     (Local
      ((typ (Scheme (Type_app (() Bool) ()))) (type_source Val_declared)
       (fixity ()))))))
  (types
   ((Int ((Local (() Abstract))))
    (Bool ((Local (() (Variants ((False ()) (True ())))))))
    (Char ((Local (() Abstract)))) (Float ((Local (() Abstract))))
    (String ((Local (() Abstract))))))
  (modules
   ((Std
     ((names ()) (types ())
      (modules
       ((Prelude
         ((names
           ((* (Imported ((Std Prelude Operators) *)))
            (+ (Imported ((Std Prelude Operators) +)))
            (- (Imported ((Std Prelude Operators) -)))
            (. (Imported ((Std Prelude Operators) .)))
            (^ (Imported ((Std Prelude Operators) ^)))
            (&& (Imported ((Std Prelude Operators) &&)))
            (:: (Imported ((Std Prelude Operators) ::)))
            (== (Imported ((Std Prelude Operators) ==)))
            (|> (Imported ((Std Prelude Operators) |>)))
            (|| (Imported ((Std Prelude Operators) ||)))
            (Nil (Imported ((Std Prelude List) Nil)))
            (not (Imported ((Std Prelude Operators) not)))
            (Cons (Imported ((Std Prelude List) Cons)))
            (sqrt
             (Local
              ((typ
                (Scheme
                 (Function (Type_app (() Int) ()) (Type_app (() Float) ()))))
               (type_source Val_declared) (fixity ()))))))
          (types ((List ((Imported ((Std Prelude List) List))))))
          (modules
           ((List
             ((names
               ((Nil
                 (Local
                  ((typ
                    (Scheme (Type_app ((Std Prelude List) List) ((Var a)))))
                   (type_source Val_declared) (fixity ()))))
                (Cons
                 (Local
                  ((typ
                    (Scheme
                     (Function (Var a)
                      (Function
                       (Type_app ((Std Prelude List) List) ((Var a)))
                       (Type_app ((Std Prelude List) List) ((Var a)))))))
                   (type_source Val_declared) (fixity ()))))))
              (types
               ((List
                 ((Local
                   ((a)
                    (Variants
                     ((Nil ())
                      (Cons
                       ((Var a)
                        (Type_app ((Std Prelude List) List) ((Var a)))))))))))))
              (modules ())))
            (Operators
             ((names
               ((*
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Int) ())
                      (Function (Type_app (() Int) ())
                       (Type_app (() Int) ())))))
                   (type_source Val_declared) (fixity ((Left 7))))))
                (+
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Int) ())
                      (Function (Type_app (() Int) ())
                       (Type_app (() Int) ())))))
                   (type_source Val_declared) (fixity ((Left 6))))))
                (-
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Int) ())
                      (Function (Type_app (() Int) ())
                       (Type_app (() Int) ())))))
                   (type_source Val_declared) (fixity ((Left 6))))))
                (.
                 (Local
                  ((typ
                    (Scheme
                     (Function (Function (Var b) (Var c))
                      (Function (Function (Var a) (Var b))
                       (Function (Var a) (Var c))))))
                   (type_source Val_declared) (fixity ((Right 9))))))
                (^
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Int) ())
                      (Function (Type_app (() Int) ())
                       (Type_app (() Int) ())))))
                   (type_source Val_declared) (fixity ((Right 8))))))
                (&&
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Bool) ())
                      (Function (Type_app (() Bool) ())
                       (Type_app (() Bool) ())))))
                   (type_source Val_declared) (fixity ((Left 3))))))
                (::
                 (Local
                  ((typ
                    (Scheme
                     (Function (Var a)
                      (Function (Type_app ((Std Prelude) List) ((Var a)))
                       (Type_app ((Std Prelude) List) ((Var a)))))))
                   (type_source Val_declared) (fixity ((Right 5))))))
                (==
                 (Local
                  ((typ
                    (Scheme
                     (Function (Var a)
                      (Function (Var a) (Type_app (() Bool) ())))))
                   (type_source Val_declared) (fixity ((Non_assoc 4))))))
                (|>
                 (Local
                  ((typ
                    (Scheme
                     (Function (Var a)
                      (Function (Function (Var a) (Var b)) (Var b)))))
                   (type_source Val_declared) (fixity ((Left 0))))))
                (||
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Bool) ())
                      (Function (Type_app (() Bool) ())
                       (Type_app (() Bool) ())))))
                   (type_source Val_declared) (fixity ((Left 2))))))
                (not
                 (Local
                  ((typ
                    (Scheme
                     (Function (Type_app (() Bool) ())
                      (Type_app (() Bool) ()))))
                   (type_source Val_declared) (fixity ()))))))
              (types ()) (modules ())))))))))))))))
|}
