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

(*$ let () = compile_and_print ~name:"llvm" ~is_sexp:false Llvm *)
let llvm = {|
; ModuleID = 'Std/Prelude.um'
source_filename = "Std/Prelude.um"
target datalayout = "i32:64-i64:64-p:64:64-f64:64"

%umber_block = type { %umber_header, [0 x i64] }
%umber_header = type { i16, i16, i32 }

@Std.Prelude.List.Nil.23 = constant %umber_block* inttoptr (i64 1 to %umber_block*)
@"Std.Prelude.Operators.::.31" = constant %umber_block* bitcast (%umber_block* (%umber_block*, %umber_block*)* @Std.Prelude.List.Cons.24 to %umber_block*)

define void @main() {
entry:
  ret void
}

define %umber_block* @Std.Prelude.List.Cons.24(%umber_block* %arg0.26, %umber_block* %arg1.25) {
entry:
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i32), i32 3))
  %box = bitcast i8* %malloccall to i64*
  %box1 = bitcast i64* %box to i16*
  store i16 0, i16* %box1, align 2
  %box2 = getelementptr i16, i16* %box1, i64 1
  store i16 2, i16* %box2, align 2
  %box3 = bitcast i64* %box to %umber_block**
  %box4 = getelementptr %umber_block*, %umber_block** %box3, i64 1
  store %umber_block* %arg0.26, %umber_block** %box4, align 8
  %box5 = bitcast i64* %box to %umber_block**
  %box6 = getelementptr %umber_block*, %umber_block** %box5, i64 2
  store %umber_block* %arg1.25, %umber_block** %box6, align 8
  %box7 = bitcast i64* %box to %umber_block*
  ret %umber_block* %box7
}

declare noalias i8* @malloc(i32)

define %umber_block* @Std.Prelude.Operators...27(%umber_block* %Std.Prelude.Operators.f.28, %umber_block* %Std.Prelude.Operators.g.29, %umber_block* %Std.Prelude.Operators.x.30) {
entry:
  %Std.Prelude.Operators.f.281 = bitcast %umber_block* %Std.Prelude.Operators.f.28 to %umber_block* (%umber_block*)*
  %Std.Prelude.Operators.g.292 = bitcast %umber_block* %Std.Prelude.Operators.g.29 to %umber_block* (%umber_block*)*
  %fun_call = tail call %umber_block* %Std.Prelude.Operators.g.292(%umber_block* %Std.Prelude.Operators.x.30)
  %fun_call3 = tail call %umber_block* %Std.Prelude.Operators.f.281(%umber_block* %fun_call)
  ret %umber_block* %fun_call3
}

define %umber_block* @Std.Prelude.Operators.not.32(%umber_block* %.33) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %.331 = ptrtoint %umber_block* %.33 to i64
  %equals = icmp eq i64 %.331, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ inttoptr (i64 1 to %umber_block*), %cond_binding_merge ], [ inttoptr (i64 3 to %umber_block*), %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define %umber_block* @"Std.Prelude.Operators.&&.34"(%umber_block* %Std.Prelude.Operators.a.35, %umber_block* %Std.Prelude.Operators.b.36) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %Std.Prelude.Operators.a.351 = ptrtoint %umber_block* %Std.Prelude.Operators.a.35 to i64
  %equals = icmp eq i64 %Std.Prelude.Operators.a.351, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ %Std.Prelude.Operators.b.36, %cond_binding_merge ], [ inttoptr (i64 1 to %umber_block*), %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define %umber_block* @"Std.Prelude.Operators.||.37"(%umber_block* %Std.Prelude.Operators.a.38, %umber_block* %Std.Prelude.Operators.b.39) {
entry:
  br label %cond

cond_binding_merge:                               ; preds = %cond_binding
  br label %cond_otherwise_merge

cond:                                             ; preds = %entry
  %Std.Prelude.Operators.a.381 = ptrtoint %umber_block* %Std.Prelude.Operators.a.38 to i64
  %equals = icmp eq i64 %Std.Prelude.Operators.a.381, 3
  br i1 %equals, label %cond_binding, label %cond_otherwise

cond_binding:                                     ; preds = %cond
  br label %cond_binding_merge

cond_otherwise:                                   ; preds = %cond
  br label %cond_otherwise_merge

cond_otherwise_merge:                             ; preds = %cond_otherwise, %cond_binding_merge
  %cond_otherwise_merge2 = phi %umber_block* [ inttoptr (i64 3 to %umber_block*), %cond_binding_merge ], [ %Std.Prelude.Operators.b.39, %cond_otherwise ]
  ret %umber_block* %cond_otherwise_merge2
}

define %umber_block* @"Std.Prelude.Operators.|>.40"(%umber_block* %Std.Prelude.Operators.x.41, %umber_block* %Std.Prelude.Operators.f.42) {
entry:
  %Std.Prelude.Operators.f.421 = bitcast %umber_block* %Std.Prelude.Operators.f.42 to %umber_block* (%umber_block*)*
  %fun_call = tail call %umber_block* %Std.Prelude.Operators.f.421(%umber_block* %Std.Prelude.Operators.x.41)
  ret %umber_block* %fun_call
}

define %umber_block* @"Std.Prelude.Operators.;.43"(%umber_block* %"*lambda_arg.44", %umber_block* %Std.Prelude.Operators.x.45) {
entry:
  ret %umber_block* %Std.Prelude.Operators.x.45
}

|}
(*$*)
