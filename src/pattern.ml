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
       Nonempty.fold fields ~init ~f:(fun init (_, pat) ->
         Option.fold pat ~init ~f:(fun init -> fold ~init ~f))
     | Union (pat1, pat2) ->
       let init = fold pat1 ~init ~f in
       fold pat2 ~init ~f)
;;

(* TODO: consider abstracting this. It would help out with the verbosity of type errors
   for some of the complex types in typed.ml. *)
module Names = struct
  type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

  let empty = Value_name.Map.empty

  (* TODO: consider making this a map to types directly, since let_inferred is always used *)
  let add_name pat_names name typ =
    let name_entry = Name_bindings.Name_entry.let_inferred typ in
    match Map.add pat_names ~key:name ~data:name_entry with
    | `Ok pat_names -> pat_names
    | `Duplicate ->
      Name_bindings.name_error
        ~msg:"Duplicate name in pattern"
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

  let gather =
    fold ~init:Value_name.Map.empty ~f:(fun pat_names name ->
      fst (add_fresh_name pat_names name))
  ;;

  let find = Map.find
  let mem = Map.mem
  let merge = Map.merge_skewed
end
