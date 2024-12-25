((current_path "Std(d)")
 (toplevel
  ((names
    ((True
      (Local ((type_ (Scheme ((Type_app Bool ()) ()))) (extern_name %true))))
     (False
      (Local ((type_ (Scheme ((Type_app Bool ()) ()))) (extern_name %false))))))
   (types
    ((Any ((Local (() (Alias (Intersection ()))))))
     (Int ((Local (() Abstract))))
     (Bool ((Local (() (Variants ((False ()) (True ())))))))
     (Char ((Local (() Abstract)))) (Float ((Local (() Abstract))))
     (Never ((Local (() (Alias (Union ()))))))
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
                  (// (Imported Std.Prelude.Operators.//))
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
                    ((type_
                      (Scheme
                       ((Function ((Type_app Float ())) (Effect_union ())
                         (Type_app Float ()))
                        ()))))))
                  (print
                   (Local
                    ((type_
                      (Scheme
                       ((Function ((Type_app String ())) (Effect_union ())
                         (Tuple ()))
                        ())))
                     (type_source Extern_declared)
                     (extern_name umber_print_endline))))
                  (ignore
                   (Local
                    ((type_
                      (Scheme
                       ((Function ((Var a)) (Effect_union ()) (Tuple ())) ()))))))
                  (print_int
                   (Local
                    ((type_
                      (Scheme
                       ((Function ((Type_app Int ())) (Effect_union ())
                         (Tuple ()))
                        ())))
                     (type_source Extern_declared)
                     (extern_name umber_print_int))))
                  (print_bool
                   (Local
                    ((type_
                      (Scheme
                       ((Function ((Type_app Bool ())) (Effect_union ())
                         (Tuple ()))
                        ())))
                     (type_source Extern_declared)
                     (extern_name umber_print_bool))))
                  (print_string
                   (Local
                    ((type_
                      (Scheme
                       ((Function ((Type_app String ())) (Effect_union ())
                         (Tuple ()))
                        ())))
                     (type_source Extern_declared)
                     (extern_name umber_print_string))))))
                (types ()) (effects ())
                (modules
                 ((Int
                   (Local
                    (()
                     ((names
                       ((abs
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ())) (Effect_union ())
                               (Type_app Int ()))
                              ()))))))
                        (neg
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ())) (Effect_union ())
                               (Type_app Int ()))
                              ()))))))
                        (to_string
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ())) (Effect_union ())
                               (Type_app String ()))
                              ()))))))))
                      (types ((Int ((Imported Int))))) (effects ())
                      (modules ())))))
                  (Bool
                   (Local
                    (()
                     ((names
                       ((to_string
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Bool ()))
                               (Effect_union ()) (Type_app String ()))
                              ()))))))))
                      (types ((Bool ((Imported Bool))))) (effects ())
                      (modules ())))))
                  (List
                   (Local
                    (()
                     ((names
                       ((Nil
                         (Local
                          ((type_
                            (Scheme
                             ((Type_app Std.Prelude.List.List ((Var a))) ()))))))
                        (map
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Function ((Var a)) (Effect_union ())
                                 (Var b)))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var b))))
                              ()))))))
                        (Cons
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Var a)
                                (Type_app Std.Prelude.List.List ((Var a))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var a))))
                              ()))))))
                        (fold
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Var acc)
                                (Function ((Var acc) (Var a))
                                 (Effect_union ()) (Var acc)))
                               (Effect_union ()) (Var acc))
                              ()))))))
                        (sort
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var a))))
                              ()))))))
                        (count
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Function ((Var a)) (Effect_union ())
                                 (Type_app Bool ())))
                               (Effect_union ()) (Type_app Int ()))
                              ()))))))
                        (split
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Type_app Int ()))
                               (Effect_union ())
                               (Tuple
                                ((Type_app Std.Prelude.List.List ((Var a)))
                                 (Type_app Std.Prelude.List.List ((Var a))))))
                              ()))))))
                        (unzip
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List
                                 ((Tuple ((Var a) (Var b))))))
                               (Effect_union ())
                               (Tuple
                                ((Type_app Std.Prelude.List.List ((Var a)))
                                 (Type_app Std.Prelude.List.List ((Var b))))))
                              ()))))))
                        (append
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Type_app Std.Prelude.List.List ((Var a))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var a))))
                              ()))))))
                        (length
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List
                                 ((Type_app Any ()))))
                               (Effect_union ()) (Type_app Int ()))
                              ()))))))
                        (reverse
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var a))))
                              ()))))))
                        (concat_map
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Function ((Var a)) (Effect_union ())
                                 (Type_app Std.Prelude.List.List ((Var b)))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var b))))
                              ()))))))
                        (zip_shortest
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Type_app Std.Prelude.List.List ((Var b))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List
                                ((Tuple ((Var a) (Var b))))))
                              ()))))))))
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
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ())))
                           (fixity (Left 6)))))
                        (pi
                         (Local ((type_ (Scheme ((Type_app Float ()) ()))))))
                        (abs
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ()))))))
                        (cos
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ()))))))
                        (sin
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ()))))))))
                      (types ((Float ((Imported Float))))) (effects ())
                      (modules ())))))
                  (Option
                   (Local
                    (()
                     ((names
                       ((None
                         (Local
                          ((type_
                            (Scheme
                             ((Type_app Std.Prelude.Option.Option ((Var a)))
                              ()))))))
                        (Some
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a)) (Effect_union ())
                               (Type_app Std.Prelude.Option.Option ((Var a))))
                              ()))))))))
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
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 7)))))
                        (*
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 7)))))
                        (+
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 6)))))
                        (-
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 6)))))
                        (.
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Function ((Var b)) (Effect_union ())
                                 (Var c))
                                (Function ((Var a)) (Effect_union ())
                                 (Var b))
                                (Var a))
                               (Effect_union ()) (Var c))
                              ())))
                           (fixity (Right 9)))))
                        (";"
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Tuple ()) (Var a))
                               (Effect_union ()) (Var a))
                              ())))
                           (fixity (Left 0)))))
                        (<
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (>
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (^
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Right 8)))))
                        (!=
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (&&
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Bool ()) (Type_app Bool ()))
                               (Effect_union ()) (Type_app Bool ()))
                              ())))
                           (fixity (Left 3)))))
                        (*.
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ())))
                           (fixity (Left 7)))))
                        (++
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app String ()) (Type_app String ()))
                               (Effect_union ()) (Type_app String ()))
                              ())))
                           (fixity (Left 5)))))
                        (+.
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ())))
                           (fixity (Left 6)))))
                        (-.
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Float ()) (Type_app Float ()))
                               (Effect_union ()) (Type_app Float ()))
                              ())))
                           (fixity (Left 6)))))
                        (//
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 7)))))
                        (::
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Var a)
                                (Type_app Std.Prelude.List.List ((Var a))))
                               (Effect_union ())
                               (Type_app Std.Prelude.List.List ((Var a))))
                              ())))
                           (fixity (Right 5)))))
                        (<=
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (==
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (>=
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Var a) (Var a)) (Effect_union ())
                               (Type_app Bool ()))
                              ())))
                           (fixity (Non_assoc 4)))))
                        (|>
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Var a)
                                (Function ((Var a)) (Effect_union ())
                                 (Var b)))
                               (Effect_union ()) (Var b))
                              ())))
                           (fixity (Left 0)))))
                        (||
                         (Local
                          ((type_
                            (Scheme
                             ((Function
                               ((Type_app Bool ()) (Type_app Bool ()))
                               (Effect_union ()) (Type_app Bool ()))
                              ())))
                           (fixity (Left 2)))))
                        (mod
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Int ()) (Type_app Int ()))
                               (Effect_union ()) (Type_app Int ()))
                              ())))
                           (fixity (Left 7)))))
                        (not
                         (Local
                          ((type_
                            (Scheme
                             ((Function ((Type_app Bool ()))
                               (Effect_union ()) (Type_app Bool ()))
                              ()))))))))
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
                 (// (Imported Std.Prelude.Operators.//))
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
                   ((type_
                     (Scheme
                      ((Function ((Type_app Float ())) (Effect_union ())
                        (Type_app Float ()))
                       ())))
                    (type_source Extern_declared)
                    (extern_name umber_float_sqrt))))
                 (print
                  (Local
                   ((type_
                     (Scheme
                      ((Function ((Type_app String ())) (Effect_union ())
                        (Tuple ()))
                       ())))
                    (type_source Extern_declared)
                    (extern_name umber_print_endline))))
                 (ignore
                  (Local
                   ((type_
                     (Scheme
                      ((Function ((Intersection ())) (Effect_union ())
                        (Tuple ()))
                       ())))
                    (type_source Let_inferred))))
                 (print_int
                  (Local
                   ((type_
                     (Scheme
                      ((Function ((Type_app Int ())) (Effect_union ())
                        (Tuple ()))
                       ())))
                    (type_source Extern_declared)
                    (extern_name umber_print_int))))
                 (print_bool
                  (Local
                   ((type_
                     (Scheme
                      ((Function ((Type_app Bool ())) (Effect_union ())
                        (Tuple ()))
                       ())))
                    (type_source Extern_declared)
                    (extern_name umber_print_bool))))
                 (print_string
                  (Local
                   ((type_
                     (Scheme
                      ((Function ((Type_app String ())) (Effect_union ())
                        (Tuple ()))
                       ())))
                    (type_source Extern_declared)
                    (extern_name umber_print_string))))))
               (types ()) (effects ())
               (modules
                ((Int
                  (Local
                   (()
                    ((names
                      ((abs
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ())) (Effect_union ())
                              (Type_app Int ()))
                             ())))
                          (type_source Let_inferred))))
                       (neg
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ())) (Effect_union ())
                              (Type_app Int ()))
                             ())))
                          (type_source Let_inferred))))
                       (to_string
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ())) (Effect_union ())
                              (Type_app String ()))
                             ())))
                          (type_source Extern_declared)
                          (extern_name umber_int_to_string))))))
                     (types ((Int ((Imported Int))))) (effects ())
                     (modules ())))))
                 (Bool
                  (Local
                   (()
                    ((names
                      ((to_string
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Bool ())) (Effect_union ())
                              (Type_app String ()))
                             ())))
                          (type_source Let_inferred))))))
                     (types ((Bool ((Imported Bool))))) (effects ())
                     (modules ())))))
                 (List
                  (Local
                   (()
                    ((names
                      ((Nil
                        (Local
                         ((type_
                           (Scheme
                            ((Type_app Std.Prelude.List.List ((Var a))) ()))))))
                       (map
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Function ((Var a)) (Effect_var c) (Var d)))
                              (Effect_var c)
                              (Type_app Std.Prelude.List.List ((Var d))))
                             ())))
                          (type_source Let_inferred))))
                       (Cons
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ()))))))
                       (fold
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Var b)
                               (Function ((Var b) (Var a)) (Effect_var e)
                                (Var b)))
                              (Effect_var e) (Var b))
                             ())))
                          (type_source Let_inferred))))
                       (sort
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ())))
                          (type_source Let_inferred))))
                       (count
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Function ((Var a)) (Effect_var c)
                                (Type_app Bool ())))
                              (Effect_var c) (Type_app Int ()))
                             ())))
                          (type_source Let_inferred))))
                       (split
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var f)))
                               (Type_app Int ()))
                              (Effect_union ())
                              (Tuple
                               ((Type_app Std.Prelude.List.List ((Var f)))
                                (Type_app Std.Prelude.List.List ((Var f))))))
                             ())))
                          (type_source Let_inferred))))
                       (unzip
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List
                                ((Tuple ((Var a) (Var b))))))
                              (Effect_union ())
                              (Tuple
                               ((Type_app Std.Prelude.List.List ((Var a)))
                                (Type_app Std.Prelude.List.List ((Var b))))))
                             ())))
                          (type_source Let_inferred))))
                       (append
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ())))
                          (type_source Let_inferred))))
                       (length
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List
                                ((Intersection ()))))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Let_inferred))))
                       (reverse
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ())))
                          (type_source Let_inferred))))
                       (concat_map
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Function ((Var a)) (Effect_var c)
                                (Type_app Std.Prelude.List.List ((Var d)))))
                              (Effect_var c)
                              (Type_app Std.Prelude.List.List ((Var d))))
                             ())))
                          (type_source Let_inferred))))
                       (rev_append
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ())))
                          (type_source Let_inferred))))
                       (zip_shortest
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Std.Prelude.List.List ((Var a)))
                               (Type_app Std.Prelude.List.List ((Var b))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List
                               ((Tuple ((Var a) (Var b))))))
                             ())))
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
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_add))))
                       (pi
                        (Local
                         ((type_ (Scheme ((Type_app Float ()) ())))
                          (type_source Extern_declared)
                          (extern_name umber_float_pi))))
                       (abs
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared)
                          (extern_name umber_float_abs))))
                       (cos
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared)
                          (extern_name umber_float_cos))))
                       (sin
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared)
                          (extern_name umber_float_sin))))))
                     (types ((Float ((Imported Float))))) (effects ())
                     (modules ())))))
                 (Option
                  (Local
                   (()
                    ((names
                      ((None
                        (Local
                         ((type_
                           (Scheme
                            ((Type_app Std.Prelude.Option.Option ((Var a)))
                             ()))))))
                       (Some
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a)) (Effect_union ())
                              (Type_app Std.Prelude.Option.Option ((Var a))))
                             ()))))))))
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
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_rem))))
                       (*
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mul))))
                       (+
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_add))))
                       (-
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_sub))))
                       (.
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Function ((Var a)) (Effect_var b) (Var c))
                               (Function ((Var d)) (Effect_var b) (Var a))
                               (Var d))
                              (Effect_var b) (Var c))
                             ())))
                          (type_source Let_inferred) (fixity (Right 9)))))
                       (";"
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Tuple ()) (Var a)) (Effect_union ())
                              (Var a))
                             ())))
                          (type_source Let_inferred) (fixity (Left 0)))))
                       (<
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_lt))))
                       (>
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gt))))
                       (^
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Right 8))
                          (extern_name umber_int_pow))))
                       (!=
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_neq))))
                       (&&
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Bool ()) (Type_app Bool ()))
                              (Effect_union ()) (Type_app Bool ()))
                             ())))
                          (type_source Let_inferred) (fixity (Left 3)))))
                       (*.
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_float_mul))))
                       (++
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app String ()) (Type_app String ()))
                              (Effect_union ()) (Type_app String ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 5))
                          (extern_name umber_string_append))))
                       (+.
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_add))))
                       (-.
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Float ()) (Type_app Float ()))
                              (Effect_union ()) (Type_app Float ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_sub))))
                       (//
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_div))))
                       (::
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Var a)
                               (Type_app Std.Prelude.List.List ((Var a))))
                              (Effect_union ())
                              (Type_app Std.Prelude.List.List ((Var a))))
                             ())))
                          (type_source Let_inferred) (fixity (Right 5)))))
                       (<=
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_lte))))
                       (==
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_eq))))
                       (>=
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Var a) (Var a)) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gte))))
                       (|>
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Var a)
                               (Function ((Var a)) (Effect_var c) (Var d)))
                              (Effect_var c) (Var d))
                             ())))
                          (type_source Let_inferred) (fixity (Left 0)))))
                       (||
                        (Local
                         ((type_
                           (Scheme
                            ((Function
                              ((Type_app Bool ()) (Type_app Bool ()))
                              (Effect_union ()) (Type_app Bool ()))
                             ())))
                          (type_source Let_inferred) (fixity (Left 2)))))
                       (mod
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Int ()) (Type_app Int ()))
                              (Effect_union ()) (Type_app Int ()))
                             ())))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mod))))
                       (not
                        (Local
                         ((type_
                           (Scheme
                            ((Function ((Type_app Bool ())) (Effect_union ())
                              (Type_app Bool ()))
                             ())))
                          (type_source Let_inferred))))))
                     (types ()) (effects ()) (modules ()))))))))))))))))))))))
