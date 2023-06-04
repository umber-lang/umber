open! Core

(*$
  open! Core

  let compile_and_print ~name ~is_sexp target =
    print_endline [%string "\nlet %{name} = {|"];
    Umberboot.compile
      ~filename:"Std/Prelude.um"
      ~no_std:true
      ~parent:(Umber.Ast.Module_name.of_string_exn "Std")
      [ target, Stdout ];
    print_endline "|}";
    if is_sexp then print_endline "|> Sexp.of_string"
  ;;
$*)

(*$ let () = compile_and_print ~name:"names" ~is_sexp:true Names *)
let names = {|
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
   (modules
    ((Std
      (Local
       (()
        ((names ()) (types ())
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
                       (Function ((Type_app Std.Prelude.Float ()))
                        (Type_app Std.Prelude.Float ())))))))
                  (print
                   (Local
                    ((typ
                      (Scheme (Function ((Type_app String ())) (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_endline))))
                  (print_int
                   (Local
                    ((typ
                      (Scheme
                       (Function ((Type_app Std.Prelude.Int ())) (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_int))))
                  (print_bool
                   (Local
                    ((typ
                      (Scheme (Function ((Type_app Bool ())) (Tuple ()))))
                     (type_source Extern_declared)
                     (extern_name umber_print_bool))))))
                (types ())
                (modules
                 ((Int
                   (Local
                    (()
                     ((names ())
                      (types
                       ((Int ((Local (() (Alias (Type_app Int ()))))))
                        (PrimitiveInt ((Imported Int)))))
                      (modules ())))))
                  (List
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
                              (Type_app Std.Prelude.Float.Float ()))))
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
                              (Type_app Std.Prelude.Float.Float ())))))))
                        (cos
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              (Type_app Std.Prelude.Float.Float ())))))))
                        (sin
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float.Float ())
                               (Type_app Std.Prelude.Float.Float ()))
                              (Type_app Std.Prelude.Float.Float ())))))))))
                      (types
                       ((Float ((Local (() (Alias (Type_app Float ()))))))
                        (PrimitiveFloat ((Imported Float)))))
                      (modules ())))))
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
                             (Function ((Var a))
                              (Type_app Std.Prelude.Option.Option ((Var a)))))))))))
                      (types
                       ((Option
                         ((Local
                           ((a) (Variants ((None ()) (Some ((Var a)))))))))))
                      (modules ())))))
                  (Operators
                   (Local
                    (()
                     ((names
                       ((%
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
                           (fixity (Left 7)))))
                        (*
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
                           (fixity (Left 7)))))
                        (+
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
                           (fixity (Left 6)))))
                        (-
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
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
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
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
                        (*.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float ())
                               (Type_app Std.Prelude.Float ()))
                              (Type_app Std.Prelude.Float ()))))
                           (fixity (Left 7)))))
                        (++
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app String ()) (Type_app String ()))
                              (Type_app String ()))))
                           (fixity (Left 5)))))
                        (+.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float ())
                               (Type_app Std.Prelude.Float ()))
                              (Type_app Std.Prelude.Float ()))))
                           (fixity (Left 6)))))
                        (-.
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Float ())
                               (Type_app Std.Prelude.Float ()))
                              (Type_app Std.Prelude.Float ()))))
                           (fixity (Left 6)))))
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
                        (mod
                         (Local
                          ((typ
                            (Scheme
                             (Function
                              ((Type_app Std.Prelude.Int ())
                               (Type_app Std.Prelude.Int ()))
                              (Type_app Std.Prelude.Int ()))))
                           (fixity (Left 7)))))
                        (not
                         (Local
                          ((typ
                            (Scheme
                             (Function ((Type_app Bool ()))
                              (Type_app Bool ())))))))))
                      (types ()) (modules ())))))))))
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
                      (Function ((Type_app Std.Prelude.Float ()))
                       (Type_app Std.Prelude.Float ()))))
                    (type_source Extern_declared)
                    (extern_name umber_float_sqrt))))
                 (print
                  (Local
                   ((typ
                     (Scheme (Function ((Type_app String ())) (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_endline))))
                 (print_int
                  (Local
                   ((typ
                     (Scheme
                      (Function ((Type_app Std.Prelude.Int ())) (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_int))))
                 (print_bool
                  (Local
                   ((typ (Scheme (Function ((Type_app Bool ())) (Tuple ()))))
                    (type_source Extern_declared)
                    (extern_name umber_print_bool))))))
               (types ())
               (modules
                ((Int
                  (Local
                   (()
                    ((names ())
                     (types
                      ((Int ((Local (() (Alias (Type_app Int ()))))))
                       (PrimitiveInt ((Imported Int)))))
                     (modules ())))))
                 (List
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
                             (Type_app Std.Prelude.Float.Float ()))))
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
                             (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_abs))))
                       (cos
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_cos))))
                       (sin
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float.Float ())
                              (Type_app Std.Prelude.Float.Float ()))
                             (Type_app Std.Prelude.Float.Float ()))))
                          (type_source Extern_declared)
                          (extern_name umber_float_sin))))))
                     (types
                      ((Float ((Local (() (Alias (Type_app Float ()))))))
                       (PrimitiveFloat ((Imported Float)))))
                     (modules ())))))
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
                            (Function ((Var a))
                             (Type_app Std.Prelude.Option.Option ((Var a)))))))))))
                     (types
                      ((Option
                        ((Local
                          ((a) (Variants ((None ()) (Some ((Var a)))))))))))
                     (modules ())))))
                 (Operators
                  (Local
                   (()
                    ((names
                      ((%
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_rem))))
                       (*
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mul))))
                       (+
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_add))))
                       (-
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_int_sub))))
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
                          (fixity (Non_assoc 4)) (extern_name umber_lt))))
                       (>
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gt))))
                       (^
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Right 8))
                          (extern_name umber_int_pow))))
                       (!=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_neq))))
                       (&&
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Type_app Bool ()) (Type_app Bool ()))
                             (Type_app Bool ()))))
                          (fixity (Left 3)))))
                       (*.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float ())
                              (Type_app Std.Prelude.Float ()))
                             (Type_app Std.Prelude.Float ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_float_mul))))
                       (++
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app String ()) (Type_app String ()))
                             (Type_app String ()))))
                          (type_source Extern_declared) (fixity (Left 5))
                          (extern_name umber_string_append))))
                       (+.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float ())
                              (Type_app Std.Prelude.Float ()))
                             (Type_app Std.Prelude.Float ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_add))))
                       (-.
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Float ())
                              (Type_app Std.Prelude.Float ()))
                             (Type_app Std.Prelude.Float ()))))
                          (type_source Extern_declared) (fixity (Left 6))
                          (extern_name umber_float_sub))))
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
                          (fixity (Non_assoc 4)) (extern_name umber_lte))))
                       (==
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_eq))))
                       (>=
                        (Local
                         ((typ
                           (Scheme
                            (Function ((Var a) (Var a)) (Type_app Bool ()))))
                          (type_source Extern_declared)
                          (fixity (Non_assoc 4)) (extern_name umber_gte))))
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
                       (mod
                        (Local
                         ((typ
                           (Scheme
                            (Function
                             ((Type_app Std.Prelude.Int ())
                              (Type_app Std.Prelude.Int ()))
                             (Type_app Std.Prelude.Int ()))))
                          (type_source Extern_declared) (fixity (Left 7))
                          (extern_name umber_int_mod))))
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

(*$ let () = compile_and_print ~name:"llvm" ~is_sexp:false Llvm *)
let llvm = {|
; ModuleID = 'Std/Prelude.um'
source_filename = "Std/Prelude.um"
target datalayout = "i32:64-i64:64-p:64:64-f64:64"

%umber_block = type { %umber_header, [0 x i64] }
%umber_header = type { i16, i16, i32 }

@Std.Prelude.Option.None = constant %umber_block* inttoptr (i64 1 to %umber_block*)
@Std.Prelude.List.Nil = constant %umber_block* inttoptr (i64 1 to %umber_block*)
@Std.Prelude.Float.pi = external global %umber_block*
@"Std.Prelude.Operators.::.1" = constant %umber_block* bitcast (%umber_block* (%umber_block*, %umber_block*)* @Std.Prelude.List.Cons to %umber_block*)

define i32 @"umber_main:Std/Prelude.um"() {
entry:
  ret i32 0
}

define tailcc %umber_block* @Std.Prelude.Option.Some(%umber_block* %Std.Prelude.Option.arg0.1) {
entry:
  %box = call i64* @umber_gc_alloc(i64 16)
  %box1 = bitcast i64* %box to i16*
  store i16 0, i16* %box1, align 2
  %box2 = getelementptr i16, i16* %box1, i32 1
  store i16 1, i16* %box2, align 2
  %box3 = bitcast i64* %box to %umber_block**
  %box4 = getelementptr %umber_block*, %umber_block** %box3, i32 1
  store %umber_block* %Std.Prelude.Option.arg0.1, %umber_block** %box4, align 8
  %box5 = bitcast %umber_block** %box3 to %umber_block*
  ret %umber_block* %box5
}

define tailcc %umber_block* @Std.Prelude.List.Cons(%umber_block* %Std.Prelude.List.arg0.1, %umber_block* %Std.Prelude.List.arg1.1) {
entry:
  %box = call i64* @umber_gc_alloc(i64 24)
  %box1 = bitcast i64* %box to i16*
  store i16 0, i16* %box1, align 2
  %box2 = getelementptr i16, i16* %box1, i32 1
  store i16 2, i16* %box2, align 2
  %box3 = bitcast i64* %box to %umber_block**
  %box4 = getelementptr %umber_block*, %umber_block** %box3, i32 1
  store %umber_block* %Std.Prelude.List.arg0.1, %umber_block** %box4, align 8
  %box5 = getelementptr %umber_block*, %umber_block** %box3, i32 2
  store %umber_block* %Std.Prelude.List.arg1.1, %umber_block** %box5, align 8
  %box6 = bitcast %umber_block** %box3 to %umber_block*
  ret %umber_block* %box6
}

define tailcc %umber_block* @"Std.Prelude.Float.+"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_add(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_add(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Float.abs(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_abs(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_abs(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Float.sin(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_sin(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_sin(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Float.cos(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_cos(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_cos(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Operators..(%umber_block* %Std.Prelude.Operators.f.1, %umber_block* %Std.Prelude.Operators.g.1, %umber_block* %Std.Prelude.Operators.x.1) {
entry:
  %fun_call = tail call tailcc %umber_block* @umber_apply1(%umber_block* %Std.Prelude.Operators.g.1, %umber_block* %Std.Prelude.Operators.x.1)
  %fun_call1 = tail call tailcc %umber_block* @umber_apply1(%umber_block* %Std.Prelude.Operators.f.1, %umber_block* %fun_call)
  ret %umber_block* %fun_call1
}

define tailcc %umber_block* @"Std.Prelude.Operators.+"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_add(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_add(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Operators.-(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_sub(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_sub(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.*"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_mul(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_mul(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.%"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_rem(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_rem(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.Operators.mod(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_mod(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_mod(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.^"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_int_pow(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_int_pow(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.*."(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_mul(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_mul(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.+."(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_add(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

define tailcc %umber_block* @Std.Prelude.Operators.-.(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_float_sub(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_sub(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.::"(%umber_block* %Std.Prelude.Operators.arg0.1, %umber_block* %Std.Prelude.Operators.arg1.1) {
entry:
  %"Std.Prelude.Operators.::.1" = load %umber_block*, %umber_block** @"Std.Prelude.Operators.::.1", align 8
  %fun_call = tail call tailcc %umber_block* @umber_apply2(%umber_block* %"Std.Prelude.Operators.::.1", %umber_block* %Std.Prelude.Operators.arg0.1, %umber_block* %Std.Prelude.Operators.arg1.1)
  ret %umber_block* %fun_call
}

define tailcc %umber_block* @Std.Prelude.Operators.not(%umber_block* %Std.Prelude.Operators..1) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %Std.Prelude.Operators..11 = ptrtoint %umber_block* %Std.Prelude.Operators..1 to i64
  %equals = icmp eq i64 %Std.Prelude.Operators..11, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ inttoptr (i64 1 to %umber_block*), %cond_binding_merge ], [ inttoptr (i64 3 to %umber_block*), %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define tailcc %umber_block* @"Std.Prelude.Operators.=="(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_eq(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_eq(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.!="(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_neq(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_neq(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.<"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_lt(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_lt(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.<="(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_lte(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_lte(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.>"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_gt(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_gt(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.>="(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_gte(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_gte(%umber_block*, %umber_block*)

define tailcc %umber_block* @"Std.Prelude.Operators.&&"(%umber_block* %Std.Prelude.Operators.a.1, %umber_block* %Std.Prelude.Operators.b.1) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %Std.Prelude.Operators.a.11 = ptrtoint %umber_block* %Std.Prelude.Operators.a.1 to i64
  %equals = icmp eq i64 %Std.Prelude.Operators.a.11, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ %Std.Prelude.Operators.b.1, %cond_binding_merge ], [ inttoptr (i64 1 to %umber_block*), %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define tailcc %umber_block* @"Std.Prelude.Operators.||"(%umber_block* %Std.Prelude.Operators.a.2, %umber_block* %Std.Prelude.Operators.b.2) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %Std.Prelude.Operators.a.21 = ptrtoint %umber_block* %Std.Prelude.Operators.a.2 to i64
  %equals = icmp eq i64 %Std.Prelude.Operators.a.21, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ inttoptr (i64 3 to %umber_block*), %cond_binding_merge ], [ %Std.Prelude.Operators.b.2, %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define tailcc %umber_block* @"Std.Prelude.Operators.|>"(%umber_block* %Std.Prelude.Operators.x.2, %umber_block* %Std.Prelude.Operators.f.2) {
entry:
  %fun_call = tail call tailcc %umber_block* @umber_apply1(%umber_block* %Std.Prelude.Operators.f.2, %umber_block* %Std.Prelude.Operators.x.2)
  ret %umber_block* %fun_call
}

define tailcc %umber_block* @"Std.Prelude.Operators.;"(%umber_block* %"Std.Prelude.Operators.*lambda_arg.1", %umber_block* %Std.Prelude.Operators.x.3) {
entry:
  ret %umber_block* %Std.Prelude.Operators.x.3
}

define tailcc %umber_block* @"Std.Prelude.Operators.++"(%umber_block* %0, %umber_block* %1) {
entry:
  %fun_call = tail call %umber_block* @umber_string_append(%umber_block* %0, %umber_block* %1)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_string_append(%umber_block*, %umber_block*)

define tailcc %umber_block* @Std.Prelude.sqrt(%umber_block* %0) {
entry:
  %fun_call = tail call %umber_block* @umber_float_sqrt(%umber_block* %0)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_float_sqrt(%umber_block*)

define tailcc %umber_block* @Std.Prelude.print(%umber_block* %0) {
entry:
  %fun_call = tail call %umber_block* @umber_print_endline(%umber_block* %0)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_print_endline(%umber_block*)

define tailcc %umber_block* @Std.Prelude.print_int(%umber_block* %0) {
entry:
  %fun_call = tail call %umber_block* @umber_print_int(%umber_block* %0)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_print_int(%umber_block*)

define tailcc %umber_block* @Std.Prelude.print_bool(%umber_block* %0) {
entry:
  %fun_call = tail call %umber_block* @umber_print_bool(%umber_block* %0)
  ret %umber_block* %fun_call
}

declare %umber_block* @umber_print_bool(%umber_block*)

declare i64* @umber_gc_alloc(i64)

define linkonce_odr tailcc %umber_block* @umber_apply1(%umber_block* %0, %umber_block* %1) {
entry:
  %is_on_heap = call i1 @umber_gc_is_on_heap(%umber_block* %0)
  br i1 %is_on_heap, label %closure_call, label %regular_call

closure_call:                                     ; preds = %entry
  %closure_gep = getelementptr %umber_block, %umber_block* %0, i32 0, i32 1, i32 0
  %closure_gep_raw = load i64, i64* %closure_gep, align 8
  %closure_fun = inttoptr i64 %closure_gep_raw to %umber_block* (%umber_block*, %umber_block*)*
  %closure_call1 = tail call tailcc %umber_block* %closure_fun(%umber_block* %0, %umber_block* %1)
  br label %call_phi

regular_call:                                     ; preds = %entry
  %calling_fun = bitcast %umber_block* %0 to %umber_block* (%umber_block*)*
  %regular_call2 = tail call tailcc %umber_block* %calling_fun(%umber_block* %1)
  br label %call_phi

call_phi:                                         ; preds = %regular_call, %closure_call
  %call_phi3 = phi %umber_block* [ %closure_call1, %closure_call ], [ %regular_call2, %regular_call ]
  ret %umber_block* %call_phi3
}

declare i1 @umber_gc_is_on_heap(%umber_block*)

define linkonce_odr tailcc %umber_block* @umber_apply2(%umber_block* %0, %umber_block* %1, %umber_block* %2) {
entry:
  %is_on_heap = call i1 @umber_gc_is_on_heap(%umber_block* %0)
  br i1 %is_on_heap, label %closure_call, label %regular_call

closure_call:                                     ; preds = %entry
  %closure_gep = getelementptr %umber_block, %umber_block* %0, i32 0, i32 1, i32 0
  %closure_gep_raw = load i64, i64* %closure_gep, align 8
  %closure_fun = inttoptr i64 %closure_gep_raw to %umber_block* (%umber_block*, %umber_block*, %umber_block*)*
  %closure_call1 = tail call tailcc %umber_block* %closure_fun(%umber_block* %0, %umber_block* %1, %umber_block* %2)
  br label %call_phi

regular_call:                                     ; preds = %entry
  %calling_fun = bitcast %umber_block* %0 to %umber_block* (%umber_block*, %umber_block*)*
  %regular_call2 = tail call tailcc %umber_block* %calling_fun(%umber_block* %1, %umber_block* %2)
  br label %call_phi

call_phi:                                         ; preds = %regular_call, %closure_call
  %call_phi3 = phi %umber_block* [ %closure_call1, %closure_call ], [ %regular_call2, %regular_call ]
  ret %umber_block* %call_phi3
}

|}
(*$*)
