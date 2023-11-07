((current_path "Std(d)")
 (toplevel
  ((names
    ((True (Local ((typ (Scheme (Type_app Bool ()))) (extern_name %true))))
     (False (Local ((typ (Scheme (Type_app Bool ()))) (extern_name %false))))))
   (types
    ((Int ((Local (() Abstract))))
     (Bool ((Local (() (Variants ((False ()) (True ())))))))
     (Char ((Local (() Abstract)))) (Float ((Local (() Abstract))))
     (String ((Local (() Abstract))))))
   (effects ())
   (modules
    ((Std
      (Local
       (()
        ((names ()) (types ()) (effects ())
         (modules
          ((Prelude
            (Local
             ((((names
                 ((% (Imported Std.Prelude.Operators.%))
                  (* (Imported Std.Prelude.Operators.*))
                  (+ (Imported Std.Prelude.Operators.+))
                  (- (Imported Std.Prelude.Operators.-))
                  (. (Imported Std.Prelude.Operators..))
                  (";" (Imported "Std.Prelude.Operators.;"))
                  (< (Imported Std.Prelude.Operators.<))
                  (> (Imported Std.Prelude.Operators.>))
                  (^ (Imported Std.Prelude.Operators.^))
                  (!= (Imported Std.Prelude.Operators.!=))
                  (&& (Imported Std.Prelude.Operators.&&))
                  (*. (Imported Std.Prelude.Operators.*.))
                  (++ (Imported Std.Prelude.Operators.++))
                  (+. (Imported Std.Prelude.Operators.+.))
                  (-. (Imported Std.Prelude.Operators.-.))
                  (:: (Imported Std.Prelude.Operators.::))
                  (<= (Imported Std.Prelude.Operators.<=))
                  (== (Imported Std.Prelude.Operators.==))
                  (>= (Imported Std.Prelude.Operators.>=))
                  (|> (Imported Std.Prelude.Operators.|>))
                  (|| (Imported Std.Prelude.Operators.||))
                  (Nil (Imported Std.Prelude.List.Nil))
                  (mod (Imported Std.Prelude.Operators.mod))
                  (not (Imported Std.Prelude.Operators.not))
                  (Cons (Imported Std.Prelude.List.Cons))
                  (None (Imported Std.Prelude.Option.None))
                  (Some (Imported Std.Prelude.Option.Some))
                  (sqrt
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app Std.Prelude.Float.Float ())) ()
                        (Type_app Std.Prelude.Float.Float ())))))))
                  (print
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app String ())) () (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_endline))))
                  (print_int
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app Std.Prelude.Int.Int ())) ()
                        (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_int))))
                  (print_bool
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app Std.Prelude.Bool.Bool ())) ()
                        (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_bool))))))
                (types ()) (effects ())
                (modules
                 ((Int
                   (Local
                    (()
                     ((names
                       ((to_string
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Std.Prelude.Int.Int ())) ()
                              (Type_app String ())))))))))
                      (types
                       ((Int ((Local (() (Alias (Type_app Int ()))))))
                        (PrimitiveInt ((Imported Int)))))
                      (effects ()) (modules ())))))
                  (Bool
                   (Local
                    (()
                     ((names
                       ((to_string
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Std.Prelude.Bool.Bool ()))
                              () (Type_app String ())))))))))
                      (types
                       ((Bool ((Local (() (Alias (Type_app Bool ()))))))
                        (PrimitiveBool ((Imported Bool)))))
                      (effects ()) (modules ())))))
                  (List
                   (Local
                    (()
                     ((names
                       ((Nil
                         (Local
                          ((typ
                            (Scheme
                             (Type_app Std.Prelude.List.List ((Var a))))))))
                        (map
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Function ((Var a)) () (Var b)))
                              () (Type_app Std.Prelude.List.List ((Var b)))))))))
                        (Cons
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a))))
                              () (Type_app Std.Prelude.List.List ((Var a)))))))))
                        (append
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Type_app Std.Prelude.List.List ((Var a))))
                              () (Type_app Std.Prelude.List.List ((Var a)))))))))
                        (reverse
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.List.List ((Var a)))) ()
                              (Type_app Std.Prelude.List.List ((Var a)))))))))
                        (concat_map
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Function ((Var a)) ()
                                (Type_app Std.Prelude.List.List ((Var b)))))
                              () (Type_app Std.Prelude.List.List ((Var b)))))))))))
                      (types
                       ((List
                         ((Local
                           ((a)
                            (Variants
                             ((Nil ())
                              (Cons
                               ((Var a)
                                (Type_app Std.Prelude.List.List ((Var a)))))))))))))
                      (effects ()) (modules ())))))
                  (Float
                   (Local
                    (()
                     ((names
                       ((+
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ()))))
                           (fixity (Left 6)))))
                        (pi
                         (Local
                          ((typ
                            (Scheme (Type_app Std.Prelude.Float.Float ()))))))
                        (abs
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ())))))))
                        (cos
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ())))))))
                        (sin
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ())))))))))
                      (types
                       ((Float ((Local (() (Alias (Type_app Float ()))))))
                        (PrimitiveFloat ((Imported Float)))))
                      (effects ()) (modules ())))))
                  (Option
                   (Local
                    (()
                     ((names
                       ((None
                         (Local
                          ((typ
                            (Scheme
                             (Type_app Std.Prelude.Option.Option ((Var a))))))))
                        (Some
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a)) ()
                              (Type_app Std.Prelude.Option.Option ((Var a)))))))))))
                      (types
                       ((Option
                         ((Local
                           ((a) (Variants ((None ()) (Some ((Var a)))))))))))
                      (effects ()) (modules ())))))
                  (Operators
                   (Local
                    (()
                     ((names
                       ((%
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Left 7)))))
                        (*
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Left 7)))))
                        (+
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Left 6)))))
                        (-
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Left 6)))))
                        (.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Function ((Var b)) () (Var c))
                               (Function ((Var a)) () (Var b)) (Var a))
                              () (Var c))))
                           (fixity (Right 9)))))
                        (";"
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Tuple ()) (Var a)) () (Var a))))
                           (fixity (Left 0)))))
                        (<
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (>
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (^
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Right 8)))))
                        (!=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (&&
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Bool.Bool ())
                               (Type_app Std.Prelude.Bool.Bool ()))
                              () (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Left 3)))))
                        (*.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ()))))
                           (fixity (Left 7)))))
                        (++
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app String ()) (Type_app String ())) ()
                              (Type_app String ()))))
                           (fixity (Left 5)))))
                        (+.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ()))))
                           (fixity (Left 6)))))
                        (-.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              () (Type_app Std.Prelude.Float.Float ()))))
                           (fixity (Left 6)))))
                        (::
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a))))
                              () (Type_app Std.Prelude.List.List ((Var a))))))
                           (fixity (Right 5)))))
                        (<=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (==
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (>=
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Var a) (Var a)) ()
                              (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Non_assoc 4)))))
                        (|>
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Var a) (Function ((Var a)) () (Var b))) ()
                              (Var b))))
                           (fixity (Left 0)))))
                        (||
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Bool.Bool ())
                               (Type_app Std.Prelude.Bool.Bool ()))
                              () (Type_app Std.Prelude.Bool.Bool ()))))
                           (fixity (Left 2)))))
                        (mod
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int.Int ())
                               (Type_app Std.Prelude.Int.Int ()))
                              () (Type_app Std.Prelude.Int.Int ()))))
                           (fixity (Left 7)))))
                        (not
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Std.Prelude.Bool.Bool ()))
                              () (Type_app Std.Prelude.Bool.Bool ())))))))))
                      (types ()) (effects ()) (modules ())))))))))
              ((names
                ((% (Imported Std.Prelude.Operators.%))
                 (* (Imported Std.Prelude.Operators.*))
                 (+ (Imported Std.Prelude.Operators.+))
                 (- (Imported Std.Prelude.Operators.-))
                 (. (Imported Std.Prelude.Operators..))
                 (";" (Imported "Std.Prelude.Operators.;"))
                 (< (Imported Std.Prelude.Operators.<))
                 (> (Imported Std.Prelude.Operators.>))
                 (^ (Imported Std.Prelude.Operators.^))
                 (!= (Imported Std.Prelude.Operators.!=))
                 (&& (Imported Std.Prelude.Operators.&&))
                 (*. (Imported Std.Prelude.Operators.*.))
                 (++ (Imported Std.Prelude.Operators.++))
                 (+. (Imported Std.Prelude.Operators.+.))
                 (-. (Imported Std.Prelude.Operators.-.))
                 (:: (Imported Std.Prelude.Operators.::))
                 (<= (Imported Std.Prelude.Operators.<=))
                 (== (Imported Std.Prelude.Operators.==))
                 (>= (Imported Std.Prelude.Operators.>=))
                 (|> (Imported Std.Prelude.Operators.|>))
                 (|| (Imported Std.Prelude.Operators.||))
                 (mod (Imported Std.Prelude.Operators.mod))
                 (not (Imported Std.Prelude.Operators.not))
                 (sqrt
                  (Local
                   ((typ
                     (Scheme
                      (Function ((Type_app Std.Prelude.Float.Float ())) ()
                       (Type_app Std.Prelude.Float.Float ()))))
                    (type_source Extern_declared)
                    (extern_name umber_float_sqrt))))
                 (print
                  (Local
                   ((typ
                     (Scheme (Function ((Type_app String ())) () (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_endline))))
                 (print_int
                  (Local
                   ((typ
                     (Scheme
                      (Function ((Type_app Std.Prelude.Int.Int ())) ()
                       (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_int))))
                 (print_bool
                  (Local
                   ((typ
                     (Scheme
                      (Function ((Type_app Std.Prelude.Bool.Bool ())) ()
                       (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_bool))))))
               (types ()) (effects ())
               (modules
                ((Int
                  (Local
                   (()
                    ((names
                      ((to_string
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Std.Prelude.Int.Int ())) ()
                             (Type_app String ()))))
                          (type_source Extern_declared)
                          (extern_name umber_int_to_string))))))
                     (types
                      ((Int ((Local (() (Alias (Type_app Int ()))))))
                       (PrimitiveInt ((Imported Int)))))
                     (effects ()) (modules ())))))
                 (Bool
                  (Local
                   (()
                    ((names
                      ((to_string
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ())) ()
                             (Type_app String ()))))
                          (type_source Let_inferred))))))
                     (types
                      ((Bool ((Local (() (Alias (Type_app Bool ()))))))
                       (PrimitiveBool ((Imported Bool)))))
                     (effects ()) (modules ())))))
                 (List
                  (Local
                   (()
                    ((names
                      ((Nil
                        (Local
                         ((typ
                           (Scheme
                            (Type_app Std.Prelude.List.List ((Var a))))))))
                       (map
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.List.List ((Var b)))
                              (Function ((Var d)) () (Var c)))
                             () (Var a))))
                          (type_source Let_inferred))))
                       (Cons
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Var a)
                              (Type_app Std.Prelude.List.List ((Var a))))
                             () (Type_app Std.Prelude.List.List ((Var a)))))))))
                       (append
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.List.List ((Var b)))
                              (Type_app Std.Prelude.List.List ((Var c))))
                             () (Var a))))
                          (type_source Let_inferred))))
                       (reverse
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.List.List ((Var b)))) ()
                             (Var a))))
                          (type_source Let_inferred))))
                       (concat_map
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.List.List ((Var b)))
                              (Function ((Var d)) ()
                               (Type_app Std.Prelude.List.List ((Var c)))))
                             () (Var a))))
                          (type_source Let_inferred))))
                       (rev_append
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.List.List ((Var b)))
                              (Type_app Std.Prelude.List.List ((Var c))))
                             () (Var a))))
                          (type_source Let_inferred))))))
                     (types
                      ((List
                        ((Local
                          ((a)
                           (Variants
                            ((Nil ())
                             (Cons
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a)))))))))))))
                     (effects ()) (modules ())))))
                 (Float
                  (Local
                   (()
                    ((names
                      ((+
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_add))))
                       (pi
                        (Local
                         ((typ
                           (Scheme (Type_app Std.Prelude.Float.Float ())))
                          (type_source Extern_declared)
                          (extern_name umber_float_pi))))
                       (abs
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_abs))))
                       (cos
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_cos))))
                       (sin
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_sin))))))
                     (types
                      ((Float ((Local (() (Alias (Type_app Float ()))))))
                       (PrimitiveFloat ((Imported Float)))))
                     (effects ()) (modules ())))))
                 (Option
                  (Local
                   (()
                    ((names
                      ((None
                        (Local
                         ((typ
                           (Scheme
                            (Type_app Std.Prelude.Option.Option ((Var a))))))))
                       (Some
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a)) ()
                             (Type_app Std.Prelude.Option.Option ((Var a)))))))))))
                     (types
                      ((Option
                        ((Local
                          ((a) (Variants ((None ()) (Some ((Var a)))))))))))
                     (effects ()) (modules ())))))
                 (Operators
                  (Local
                   (()
                    ((names
                      ((%
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_rem))))
                       (*
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mul))))
                       (+
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_add))))
                       (-
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_sub))))
                       (.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Function ((Var b)) () (Var c))
                              (Function ((Var a)) () (Var b)) (Var a))
                             () (Var c))))
                          (type_source Val_and_let) (fixity (Right 9)))))
                       (";"
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Tuple ()) (Var a)) () (Var a))))
                          (type_source Val_and_let) (fixity (Left 0)))))
                       (<
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_lt))))
                       (>
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gt))))
                       (^
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Right 8))
                          (extern_name umber_int_pow))))
                       (!=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_neq))))
                       (&&
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Bool.Bool ())
                              (Type_app Std.Prelude.Bool.Bool ()))
                             () (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Val_and_let) (fixity (Left 3)))))
                       (*.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_float_mul))))
                       (++
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app String ()) (Type_app String ())) ()
                             (Type_app String ()))))
                          (type_source Extern_declared) (fixity (Left 5))
                          (extern_name umber_string_append))))
                       (+.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_add))))
                       (-.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             () (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_sub))))
                       (::
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Var a)
                              (Type_app Std.Prelude.List.List ((Var a))))
                             () (Type_app Std.Prelude.List.List ((Var a))))))
                          (type_source Val_and_let) (fixity (Right 5)))))
                       (<=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_lte))))
                       (==
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_eq))))
                       (>=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) ()
                             (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gte))))
                       (|>
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Var a) (Function ((Var a)) () (Var b))) ()
                             (Var b))))
                          (type_source Val_and_let) (fixity (Left 0)))))
                       (||
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Bool.Bool ())
                              (Type_app Std.Prelude.Bool.Bool ()))
                             () (Type_app Std.Prelude.Bool.Bool ()))))
                          (type_source Val_and_let) (fixity (Left 2)))))
                       (mod
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int.Int ())
                              (Type_app Std.Prelude.Int.Int ()))
                             () (Type_app Std.Prelude.Int.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mod))))
                       (not
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ())) ()
                             (Type_app Bool ()))))
                          (type_source Let_inferred))))))
                     (types ()) (effects ()) (modules ()))))))))))))))))))))))
