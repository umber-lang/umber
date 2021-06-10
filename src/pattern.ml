open Import
open Names

type 'typ t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of 'typ t * Value_name.t
  | Cnstr_appl of Cnstr_name.Qualified.t * 'typ t list
  | Tuple of 'typ t list
  (* TODO: record fields should be non-empty *)
  | Record of (Value_name.t * 'typ t option) list
  | Union of 'typ t * 'typ t
  | Type_annotation of 'typ t * 'typ
[@@deriving sexp, variants]

let rec fold pat ~init ~f =
  let retry init pat = fold pat ~init ~f in
  let init, result = f ~retry init pat in
  match result with
  | None -> init
  | Some pat ->
    (match pat with
    | Constant _ | Catch_all _ -> init
    | As (pat, _) | Type_annotation (pat, _) -> fold pat ~init ~f
    | Cnstr_appl (_, fields) | Tuple fields ->
      List.fold fields ~init ~f:(fun init -> fold ~init ~f)
    | Record fields ->
      List.fold fields ~init ~f:(fun init (_, pat) ->
        Option.fold pat ~init ~f:(fun init -> fold ~init ~f))
    | Union (pat1, pat2) ->
      let init = fold pat1 ~init ~f in
      fold pat2 ~init ~f)
;;

module Names = struct
  type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

  let empty = Value_name.Map.empty

  (* TODO: consider making this a map to types directly, since let_inferred is always used *)
  let add_name pat_names name typ =
    let name_entry = Name_bindings.Name_entry.let_inferred typ in
    match Map.add pat_names ~key:name ~data:name_entry with
    | `Ok pat_names -> pat_names
    | `Duplicate ->
      Name_bindings.name_error_msg
        "Duplicate name in pattern"
        (Value_name.to_ustring name)
  ;;

  let add_fresh_name pat_names name =
    let var = Type.fresh_var () in
    add_name pat_names name var, var
  ;;

  let fold pat ~init ~f =
    let rec loop acc ~f = function
      | Catch_all (Some name) -> f acc name
      | As (pat, name) -> loop (f acc name) ~f pat
      | Cnstr_appl (_, items) | Tuple items -> List.fold items ~init:acc ~f:(loop ~f)
      | Record fields ->
        List.fold fields ~init:acc ~f:(fun acc -> function
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

  let gather =
    fold ~init:Value_name.Map.empty ~f:(fun pat_names name ->
      fst (add_fresh_name pat_names name))
  ;;
end
