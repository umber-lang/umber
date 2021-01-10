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

let rec unify =
  let rec occurs_in id = function
    | Type.Expr.Var id2 -> Type.Var_id.(id = id2)
    | Primitive _ -> false
    | Type_app (_, fields) | Tuple fields -> List.exists fields ~f:(occurs_in id)
    | Function (arg, body) -> occurs_in id arg || occurs_in id body
  in
  fun ~names ~types t1 t2 ->
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
    | Primitive p1, Primitive p2 ->
      if not (Type.Primitive.equal p1 p2) then type_error "Primitive type mismatch" t1 t2
    | Type_app (name1, args1), Type_app (name2, args2) ->
      (* FIXME: should look up information about the type in `names` e.g. its arity
         (type constructors must be fully applied) *)
      let decl1, decl2 =
        ( Name_bindings.find_absolute_decl names name1
        , Name_bindings.find_absolute_decl names name2 )
      in
      let decls_compatible =
        let (params1, decl1), (params2, decl2) = decl1, decl2 in
        List.equal Type.Param.equal params1 params2
        &&
        match decl1, decl2 with
        | Type.Decl.(Alias e1, Alias e2) ->
          let instantiate =
            Type.Scheme.instantiate ~map_name:(Name_bindings.absolutify_type_name names)
          in
          unify ~names ~types (instantiate e1) (instantiate e2);
          true
        | _ ->
          (* TODO: make this less fragile (e.g. it breaks if you read some name bindings
             from a file with multiple copies of some bindings *)
          phys_equal decl1 decl2
      in
      if not decls_compatible then type_error "Type application mismatch" t1 t2;
      if List.length args1 <> Type.Decl.arity decl1
      then type_error "Partially applied type constructor" t1 t2;
      iter2 ~f:(unify ~names ~types) args1 args2
    | Function (arg1, res1), Function (arg2, res2) ->
      unify ~names ~types arg1 arg2;
      unify ~names ~types res1 res2
    | Tuple xs, Tuple ys -> iter2 ~f:(unify ~names ~types) xs ys
    (* TODO: need nominal typing for records/type applications:
       to infer the type of records, unify the structural record type with the nominal type *)
    | _ -> type_error "Fell through cases" t1 t2
;;

let substitute types typ =
  Type.Expr.map typ ~f:(function
    | Var id as typ ->
      (match Hashtbl.find types.vars id with
      | Some type_sub -> `Retry type_sub
      | None -> `Halt typ)
    | typ -> `Defer typ)
;;

let generalize types typ =
  let env = Type.Param.Env_of_vars.create () in
  Type.Expr.map_vars (substitute types typ) ~f:(Type.Param.Env_of_vars.find_or_add env)
;;
