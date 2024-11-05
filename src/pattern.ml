open Import
open Names

(* TODO: consider functorising this so we can have some changes from untyped to
   typed patterns, e.g.
   - No more type annotations without needing a type parameter
   - Replacing record expressions like `{ name }` with `{ name = name }`
   - Putting record field names in a canonical order, probably in a map *)
type ('typ, 'name) t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of ('typ, 'name) t * Value_name.t
  | Cnstr_appl of 'name Cnstr_name.Qualified.t * ('typ, 'name) t list
  | Tuple of ('typ, 'name) t list
  | Record of (Value_name.t * ('typ, 'name) t option) Nonempty.t
  | Union of ('typ, 'name) t * ('typ, 'name) t
  | Type_annotation of ('typ, 'name) t * 'typ
[@@deriving equal, sexp, variants]

let rec fold_until pat ~init ~f =
  match (f init pat : _ Fold_action.t) with
  | Stop _ as stop -> stop
  | Continue (`Halt acc) -> Continue acc
  | Continue (`Defer init) ->
    (match pat with
     | Constant _ | Catch_all _ -> Continue init
     | As (pat, _) | Type_annotation (pat, _) -> fold_until pat ~init ~f
     | Cnstr_appl (_, fields) | Tuple fields ->
       List.fold_until fields ~init ~f:(fun init pat -> fold_until pat ~init ~f)
     | Record fields ->
       Nonempty.fold_until fields ~init ~f:(fun init (_, pat) ->
         match pat with
         | None -> Continue init
         | Some pat -> fold_until pat ~init ~f)
     | Union (pat1, pat2) ->
       let%bind.Fold_action init = fold_until pat1 ~init ~f in
       fold_until pat2 ~init ~f)
;;

let fold_types pat ~init ~f =
  fold_until pat ~init ~f:(fun acc pat ->
    match pat with
    | Type_annotation (_, type_) -> Continue (`Halt (f acc type_))
    | Constant _ | Catch_all _ | Cnstr_appl _ | As _ | Tuple _ | Record _ | Union _ ->
      Continue (`Defer acc))
  |> Fold_action.id
;;

(* TODO: consider abstracting this. It would help out with the verbosity of type errors
   for some of the complex types in typed.ml. *)
module Names = struct
  type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

  let empty = Value_name.Map.empty

  let add_name pat_names name typ ~type_source ~fixity =
    let name_entry = Name_bindings.Name_entry.create typ ~type_source ?fixity in
    match Map.add pat_names ~key:name ~data:name_entry with
    | `Ok pat_names -> pat_names
    | `Duplicate ->
      Name_bindings.name_error
        ~msg:"Duplicate name in pattern"
        (Value_name.to_ustring name)
  ;;

  let add_fresh_name pat_names name ~type_source ~fixity =
    let var = Internal_type.fresh_var () in
    add_name pat_names name var ~type_source ~fixity, var
  ;;

  let fold pat ~init ~f =
    let rec loop acc ~f = function
      | Catch_all (Some name) -> f acc name
      | As (pat, name) -> loop (f acc name) ~f pat
      | Cnstr_appl (_, items) | Tuple items -> List.fold items ~init:acc ~f:(loop ~f)
      | Record fields ->
        Nonempty.fold fields ~init:acc ~f:(fun acc -> function
          | name, None -> f acc name
          | _, Some pat -> loop acc ~f pat)
      | Union (pat, _) ->
        (* Both branches bind the same names, so only one need be considered *)
        loop acc ~f pat
      | Type_annotation (pat, _) -> loop acc ~f pat
      | Constant _ | Catch_all None -> acc
    in
    loop init ~f pat
  ;;

  let gather pat ~type_source ~fixity =
    fold pat ~init:Value_name.Map.empty ~f:(fun pat_names name ->
      fst (add_fresh_name pat_names name ~type_source ~fixity))
  ;;

  let find = Map.find
  let mem = Map.mem
  let merge = Map.merge_skewed
end
