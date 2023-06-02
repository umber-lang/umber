open Import

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
   GADTs (local type equality/type narrowing)
   Some of these features can make local let-generalization difficult, see:
   https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
   for an argument for just abolishing local let-generalization *)

(* TODO: Consider integrating source locations into stored types to give better type
   errors. *)

exception Type_error of Ustring.t * (Type.t * Type.t) option [@@deriving sexp]

let type_error s t1 t2 = raise (Type_error (Ustring.of_string_exn s, Some (t1, t2)))
let type_error_msg s = raise (Type_error (Ustring.of_string_exn s, None))

type t = { vars : Type.t Type.Var_id.Table.t } [@@deriving sexp]

let create () = { vars = Type.Var_id.Table.create () }

let rec occurs_in id : Type.t -> bool = function
  | Var id' -> Type.Var_id.(id = id')
  | Type_app (_, fields) | Tuple fields -> List.exists fields ~f:(occurs_in id)
  | Function (args, body) -> Nonempty.exists args ~f:(occurs_in id) || occurs_in id body
  | Partial_function (args, id') ->
    Nonempty.exists args ~f:(occurs_in id) || Type.Var_id.(id = id')
;;

let fun_arg_number_mismatch = type_error "Function argument number mismatch"

let rec unify ~names ~types t1 t2 =
  let is_bound = Hashtbl.mem types.vars in
  let get_type = Hashtbl.find_exn types.vars in
  let set_type id typ =
    if occurs_in id typ then type_error "Occurs check failed" t1 t2;
    Hashtbl.set types.vars ~key:id ~data:typ
  in
  let iter2 xs ys ~f =
    match List.iter2 ~f xs ys with
    | Ok () -> ()
    | Unequal_lengths -> type_error "Type item length mismatch" t1 t2
  in
  let instantiate_alias param_list expr =
    let params = Type.Param.Env_to_vars.create () in
    List.iter param_list ~f:(fun p ->
      ignore (Type.Param.Env_to_vars.find_or_add params p : Type.Var_id.t));
    Type.Scheme.instantiate expr ~params
  in
  let lookup_type names name args =
    let type_entry = Name_bindings.find_absolute_type_entry names name in
    if List.length args = Type.Decl.arity (Name_bindings.Type_entry.decl type_entry)
    then type_entry
    else type_error "Partially applied type constructor" t1 t2
  in
  match t1, t2 with
  | Var id1, Var id2 when Type.Var_id.(id1 = id2) -> ()
  | Var id1, _ when is_bound id1 -> unify ~names ~types (get_type id1) t2
  | _, Var id2 when is_bound id2 -> unify ~names ~types t1 (get_type id2)
  | Var id1, _ -> set_type id1 t2
  | _, Var id2 -> set_type id2 t1
  | Type_app (name1, args1), Type_app (name2, args2) ->
    let type_entry1 = lookup_type names name1 args1 in
    (match Name_bindings.Type_entry.decl type_entry1 with
     | params, Alias expr -> unify ~names ~types (instantiate_alias params expr) t2
     | (_ : _ Type.Decl.t) ->
       let type_entry2 = lookup_type names name2 args2 in
       (match Name_bindings.Type_entry.decl type_entry2 with
        | params, Alias expr -> unify ~names ~types t1 (instantiate_alias params expr)
        | (_ : _ Type.Decl.t) ->
          if not (Name_bindings.Type_entry.identical type_entry1 type_entry2)
          then type_error "Type application mismatch" t1 t2;
          iter2 args1 args2 ~f:(unify ~names ~types)))
  | Type_app (name, args), ((Tuple _ | Function _ | Partial_function _) as other_type)
  | ((Tuple _ | Function _ | Partial_function _) as other_type), Type_app (name, args) ->
    (match Name_bindings.Type_entry.decl (lookup_type names name args) with
     | params, Alias expr ->
       unify ~names ~types (instantiate_alias params expr) other_type
     | _ -> type_error "Type application mismatch" t1 t2)
  | Function (args1, res1), Function (args2, res2) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Same_length -> ()
     | Left_trailing _ | Right_trailing _ -> fun_arg_number_mismatch t1 t2);
    unify ~names ~types res1 res2
  | Partial_function (args1, id1), Partial_function (args2, id2) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing args1_trailing ->
       unify ~names ~types (Partial_function (args1_trailing, id1)) (Var id2)
     | Right_trailing args2_trailing ->
       unify ~names ~types (Var id1) (Partial_function (args2_trailing, id2))
     | Same_length -> unify ~names ~types (Var id1) (Var id2))
  | Partial_function (args1, id), Function (args2, res) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing _ -> fun_arg_number_mismatch t1 t2
     | Right_trailing args2_trailing ->
       let id' = Type.Var_id.create () in
       unify ~names ~types (Var id') res;
       unify ~names ~types (Var id) (Partial_function (args2_trailing, id'))
     | Same_length -> unify ~names ~types (Var id) res)
  | Function (args1, res), Partial_function (args2, id) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing args1_trailing ->
       let id' = Type.Var_id.create () in
       unify ~names ~types res (Var id');
       unify ~names ~types (Partial_function (args1_trailing, id')) (Var id)
     | Right_trailing _ -> fun_arg_number_mismatch t1 t2
     | Same_length -> unify ~names ~types res (Var id))
  | Tuple xs, Tuple ys -> iter2 ~f:(unify ~names ~types) xs ys
  | Tuple _, (Function _ | Partial_function _)
  | Function _, Tuple _
  | Partial_function _, Tuple _ -> type_error "Types do not match" t1 t2
;;

let rec substitute types typ =
  Type.Expr.map typ ~var:Fn.id ~pf:Fn.id ~name:Fn.id ~f:(fun typ ->
    match typ with
    | Var id ->
      (match Hashtbl.find types.vars id with
       | Some type_sub -> Retry type_sub
       | None -> Halt typ)
    | Partial_function (args, id) -> combine_partial_functions types typ args id
    | _ -> Defer typ)

and combine_partial_functions types typ args id =
  match Hashtbl.find types.vars id with
  | None -> Defer typ
  | Some type_sub ->
    let args = Nonempty.map args ~f:(substitute types) in
    (match substitute types type_sub with
     | Partial_function (args', id') ->
       let args' = Nonempty.map args' ~f:(substitute types) in
       let args_combined = Nonempty.(args @ args') in
       let typ = Type.Expr.Partial_function (args_combined, id') in
       combine_partial_functions types typ args_combined id'
     | (Var _ | Type_app _ | Tuple _ | Function _) as type_sub ->
       Halt (Function (args, type_sub)))
;;

let generalize types typ =
  let env = Type.Param.Env_of_vars.create () in
  Type.Expr.map
    (substitute types typ)
    ~var:(Type.Param.Env_of_vars.find_or_add env)
    ~pf:(never_happens [%here])
    ~name:Fn.id
    ~f:
      (function
       | Partial_function (args, id) -> Defer (Function (args, Var id))
       | typ -> Defer typ)
;;

let%expect_test "unification cycles" =
  let types = create () in
  let names = Name_bindings.core in
  print_s [%sexp (types : t)];
  [%expect {| ((vars ())) |}];
  let a = Type.fresh_var () in
  let b = Type.fresh_var () in
  let c = Type.fresh_var () in
  let d = Type.fresh_var () in
  unify ~names ~types a b;
  print_s [%sexp (types : t)];
  [%expect {| ((vars ((0 (Var 1))))) |}];
  unify ~names ~types b c;
  print_s [%sexp (types : t)];
  [%expect {| ((vars ((0 (Var 1)) (1 (Var 2))))) |}];
  unify ~names ~types c d;
  print_s [%sexp (types : t)];
  [%expect {| ((vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3))))) |}];
  unify ~names ~types d a;
  print_s [%sexp (types : t)];
  [%expect {| ((vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3))))) |}]
;;
