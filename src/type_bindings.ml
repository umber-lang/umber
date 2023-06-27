open! Import
open! Names

(* TODO: Trait constraints, subtyping, (functional dependencies or associated types),
   GADTs (local type equality/type narrowing)
   Some of these features can make local let-generalization difficult, see:
   https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
   for an argument for just abolishing local let-generalization *)

(* TODO: Consider integrating source locations into stored types to give better type
   errors. *)

let type_error msg t1 t2 =
  (* Prevent unstable Var_ids from appearing in test output *)
  let env = Type.Param.Env_of_vars.create () in
  let handle_var = Type.Param.Env_of_vars.find_or_add env in
  let map_type t =
    Type.Expr.map t ~var:handle_var ~pf:handle_var ~name:Fn.id
    |> [%sexp_of:
         (Type_param_name.t, Type_param_name.t, Module_path.absolute) Type.Expr.t]
  in
  Compilation_error.raise
    Type_error
    ~msg:[%message msg ~type1:(map_type t1 : Sexp.t) ~type2:(map_type t2 : Sexp.t)]
;;

type t =
  { type_vars : Type.t Type.Var_id.Table.t
  ; effect_vars :
      (Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effect_row
      Type.Var_id.Table.t
  }
[@@deriving sexp]

let create () =
  { type_vars = Type.Var_id.Table.create (); effect_vars = Type.Var_id.Table.create () }
;;

let rec occurs_in id : Type.t -> bool = function
  | Var id' -> Type.Var_id.(id = id')
  | Type_app (_, fields) | Tuple fields -> List.exists fields ~f:(occurs_in id)
  | Function (args, effect_row, body) ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effect_row id effect_row
    || occurs_in id body
  | Partial_function (args, effect_row, id') ->
    Nonempty.exists args ~f:(occurs_in id)
    || occurs_in_effect_row id effect_row
    || Type.Var_id.(id = id')

and occurs_in_effect_row id (effect_row : _ Type.Expr.effect_row) =
  List.exists effect_row ~f:(function
    | Effect_var id' -> Type.Var_id.(id = id')
    | Effect (_, args) -> List.exists args ~f:(occurs_in id))
;;

let fun_arg_number_mismatch = type_error "Function argument number mismatch"

(* FIXME: I think we actually need to point the effect vars at total rows. Maybe this is
   where multiple variables make us pretty sad. *)
let check_effect_is_total effect =
  if not (Type.Expr.effect_is_total effect)
  then raise_s [%message "Effect in partial application is not total"]
;;

let unhandled_effects effects =
  Compilation_error.raise
    Type_error
    ~msg:
      [%message
        "Unhandled effects"
          (effects
            : (Type.Var_id.t, Type.Var_id.t, Module_path.absolute) Type.Expr.effect_row)]
;;

let make_total t (effects : _ Type.Expr.effect_row) =
  List.iter effects ~f:(function
    | Effect_var id ->
      Hashtbl.update t.effect_vars id ~f:(function
        | None -> Type.Expr.total_effect
        | Some effects ->
          if Type.Expr.effect_is_total effects then effects else unhandled_effects effects)
    | Effect _ as effect -> unhandled_effects [ effect ])
;;

let rec unify ~names ~types t1 t2 =
  let is_bound = Hashtbl.mem types.type_vars in
  let get_type = Hashtbl.find_exn types.type_vars in
  let set_type id typ =
    if occurs_in id typ then type_error "Occurs check failed" t1 t2;
    Hashtbl.set types.type_vars ~key:id ~data:typ
  in
  let iter2 xs ys ~f =
    match List.iter2 ~f xs ys with
    | Ok () -> ()
    | Unequal_lengths -> type_error "Type item length mismatch" t1 t2
  in
  let instantiate_alias (param_list : Type_param_name.t Unique_list.t) expr =
    let params = Type.Param.Env_to_vars.create () in
    List.iter
      (param_list :> Type_param_name.t list)
      ~f:(fun p -> ignore (Type.Param.Env_to_vars.find_or_add params p : Type.Var_id.t));
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
  | Function (args1, _, res1), Function (args2, _, res2) ->
    (* FIXME: Do we need to unify or do anything with effects here, or will subtyping
       handle it later? We might end up needing to represent the subtyping contraints
       explicitly here. Also ignored in partial same length cases below. *)
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Same_length -> ()
     | Left_trailing _ | Right_trailing _ -> fun_arg_number_mismatch t1 t2);
    unify ~names ~types res1 res2
  | Partial_function (args1, effect_row1, id1), Partial_function (args2, effect_row2, id2)
    ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing args1_trailing ->
       (* FIXME: Left fun has more args, right fun is under-applied and must be total. *)
       check_effect_is_total effect_row2;
       unify ~names ~types (Partial_function (args1_trailing, effect_row1, id1)) (Var id2)
     | Right_trailing args2_trailing ->
       check_effect_is_total effect_row1;
       unify ~names ~types (Var id1) (Partial_function (args2_trailing, effect_row2, id2))
     | Same_length -> unify ~names ~types (Var id1) (Var id2))
  | Partial_function (args1, effect_row1, id), Function (args2, effect_row2, res) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing _ -> fun_arg_number_mismatch t1 t2
     | Right_trailing args2_trailing ->
       check_effect_is_total effect_row1;
       let id' = Type.Var_id.create () in
       unify ~names ~types (Var id') res;
       unify ~names ~types (Var id) (Partial_function (args2_trailing, effect_row2, id'))
     | Same_length -> unify ~names ~types (Var id) res)
  | Function (args1, effect_row1, res), Partial_function (args2, effect_row2, id) ->
    (match Nonempty.iter2 args1 args2 ~f:(unify ~names ~types) with
     | Left_trailing args1_trailing ->
       check_effect_is_total effect_row2;
       let id' = Type.Var_id.create () in
       unify ~names ~types res (Var id');
       unify ~names ~types (Partial_function (args1_trailing, effect_row1, id')) (Var id)
     | Right_trailing _ -> fun_arg_number_mismatch t1 t2
     | Same_length -> unify ~names ~types res (Var id))
  | Tuple xs, Tuple ys -> iter2 ~f:(unify ~names ~types) xs ys
  | Tuple _, (Function _ | Partial_function _)
  | Function _, Tuple _
  | Partial_function _, Tuple _ -> type_error "Types do not match" t1 t2
;;

(* | Partial_effect _, _ | _, Partial_effect _ ->
    (* FIXME: check *)
    compiler_bug [%message "unfying effect rows"] *)

(* FIXME: cleanup *)
(* and unify_effect_rows
  ~names
  ~types
  effects1
  effects2
  =
  (* FIXME: How should I think about effect unification? What about multiples of the same
     effect with different type parameters? Maybe we just disallow that for now. *)
  (* Union the effects. Maybe effects should be maps we can merge? *)
  let effects =
    Map.fold_symmetric_diff
      effects1
      effects2
      ~init:(Effect_name.Map.empty, Effect_name.Map.empty)
      ~data_equal:(fun _ _ -> false)
      ~f:(fun (left_only, right_only) data ->
        (* FIXME: Somehow need to unify the unmatched parts with the variable *)
        match data with
        | `Left args -> Map.add_exn left_only ~key ~data, right_only
        | `Right args -> left_only, Map.add_exn left_only ~key ~data
        | `Unequal (args1, args2) ->
          iter2 args1 args2 ~f:(unify ~names ~types);
          left_only, right_only)
  in
  unify ~names ~types
;; *)

(* FIXME: Need to simplify effect types *)
let rec substitute types typ =
  Type.Expr.map typ ~var:Fn.id ~pf:Fn.id ~name:Fn.id ~f:(fun typ ->
    match typ with
    | Var id ->
      (match Hashtbl.find types.type_vars id with
       | Some type_sub -> Retry type_sub
       | None -> Halt typ)
    | Partial_function (args, effect_row, id) ->
      combine_partial_functions types typ args effect_row id
    | _ -> Defer typ)

and combine_partial_functions types typ args effect_row id =
  match Hashtbl.find types.type_vars id with
  | None -> Defer typ
  | Some type_sub ->
    let args = Nonempty.map args ~f:(substitute types) in
    (match substitute types type_sub with
     | Partial_function (args', effect_row', id') ->
       check_effect_is_total effect_row;
       let args' = Nonempty.map args' ~f:(substitute types) in
       let args_combined = Nonempty.(args @ args') in
       let typ : Type.t = Partial_function (args_combined, effect_row', id') in
       combine_partial_functions types typ args_combined effect_row' id'
     | (Var _ | Type_app _ | Tuple _ | Function _) as type_sub ->
       Halt (Function (args, effect_row, type_sub)))
;;

(* TODO: We should probably have a notion of type variable scope so that the type
   variables we introduce can be shared between multiple type expressions in the same
   expresion/statement. *)
let generalize types typ =
  let env = Type.Param.Env_of_vars.create () in
  Type.Expr.map
    (substitute types typ)
    ~var:(Type.Param.Env_of_vars.find_or_add env)
    ~pf:(never_happens [%here])
    ~name:Fn.id
    ~f:(function
      | Partial_function (args, effect_row, id) ->
        (* FIXME: fix effects *)
        Defer (Function (args, effect_row, Var id))
      | typ -> Defer typ)
;;

let%expect_test "unification cycles" =
  let types = create () in
  let names = Name_bindings.core in
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ()) (effect_vars ())) |}];
  let a = Type.fresh_var () in
  let b = Type.fresh_var () in
  let c = Type.fresh_var () in
  let d = Type.fresh_var () in
  unify ~names ~types a b;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)))) (effect_vars ())) |}];
  unify ~names ~types b c;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)))) (effect_vars ())) |}];
  unify ~names ~types c d;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}];
  unify ~names ~types d a;
  print_s [%sexp (types : t)];
  [%expect {| ((type_vars ((0 (Var 1)) (1 (Var 2)) (2 (Var 3)))) (effect_vars ())) |}]
;;
