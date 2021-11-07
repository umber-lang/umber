open Import

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
     GADTs (local type equality/type narrowing)
     Some of these features can make local let-generalization difficult, see:
     https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
     for an argument for just abolishing local let-generalization *)

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
  match t1, t2 with
  | Var id1, Var id2 when Type.Var_id.(id1 = id2) -> ()
  | Var id1, _ when is_bound id1 -> unify ~names ~types (get_type id1) t2
  | _, Var id2 when is_bound id2 -> unify ~names ~types t1 (get_type id2)
  | Var id1, _ -> set_type id1 t2
  | _, Var id2 -> set_type id2 t1
  | Type_app (name1, args1), Type_app (name2, args2) ->
    let instantiate_alias param_list expr =
      let params = Type.Param.Env_to_vars.create () in
      List.iter param_list ~f:(fun p ->
        ignore (Type.Param.Env_to_vars.find_or_add params p : Type.Var_id.t));
      Type.Scheme_plain.instantiate
        expr
        ~params
        ~map_name:(Name_bindings.absolutify_type_name names)
    in
    let get_decl names name args =
      let decl = Name_bindings.find_absolute_type_decl names name in
      if List.length args = Type.Decl.arity decl
      then decl
      else type_error "Partially applied type constructor" t1 t2
    in
    (match get_decl names name1 args1 with
    | params, Alias expr -> unify ~names ~types (instantiate_alias params expr) t2
    | decl1 ->
      (match get_decl names name2 args2 with
      | params, Alias expr -> unify ~names ~types t1 (instantiate_alias params expr)
      | decl2 ->
        if not (phys_equal decl1 decl2) then type_error "Type application mismatch" t1 t2;
        iter2 args1 args2 ~f:(unify ~names ~types)))
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
  | Function (args2, res), Partial_function (args1, id) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
    | Left_trailing args1_trailing ->
      let id' = Type.Var_id.create () in
      unify ~names ~types res (Var id');
      unify ~names ~types (Partial_function (args1_trailing, id')) (Var id)
    | Right_trailing _ -> fun_arg_number_mismatch t1 t2
    | Same_length -> unify ~names ~types res (Var id))
  | Tuple xs, Tuple ys -> iter2 ~f:(unify ~names ~types) xs ys
  | (Type_app _ | Tuple _ | Function _ | Partial_function _), _ ->
    type_error "Fell through cases" t1 t2
;;

let rec substitute types typ =
  Type.Expr.map typ ~var:Fn.id ~pf:Fn.id ~f:(fun typ ->
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
    ~var:(Type.Param.of_var ~env)
    ~pf:(never_happens [%here])
    ~f:(function
      | Partial_function (args, id) -> Defer (Function (args, Var id))
      | typ -> Defer typ)
;;
