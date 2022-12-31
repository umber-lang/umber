open Import
open Names

(* TODO: consider functorising this so we can have some changes from untyped to
   typed patterns, e.g.
   - No more type annotations without needing a type parameter
   - Replacing record expressions like `{ name }` with `{ name = name }`
   - Putting record field names in a canonical order, probably in a map *)
type 'typ t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of 'typ t * Value_name.t
  | Cnstr_appl of Cnstr_name.Qualified.t * 'typ t list
  | Tuple of 'typ t list
  | Record of (Value_name.t * 'typ t option) Nonempty.t
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
       Nonempty.fold fields ~init ~f:(fun init (_, pat) ->
         Option.fold pat ~init ~f:(fun init -> fold ~init ~f))
     | Union (pat1, pat2) ->
       let init = fold pat1 ~init ~f in
       fold pat2 ~init ~f)
;;

let fold_names pat ~init ~f =
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

(* TODO: consider abstracting this. It would help out with the verbosity of type errors
   for some of the complex types in typed.ml. *)
module Names : sig
  type 'a pattern := 'a t
  type t [@@deriving sexp]

  val empty : t
  val to_map : t -> Name_bindings.Name_entry.t Value_name.Map.t
  val name_set : t -> Value_name.Set.t

  val add_name
    :  t
    -> Value_name.t
    -> Type.t
    -> name_table:Name_id.Name_table.t
    -> current_path:Module_path.t
    -> in_expr:bool
    -> t

  val add_fresh_name
    :  t
    -> Value_name.t
    -> name_table:Name_id.Name_table.t
    -> current_path:Module_path.t
    -> in_expr:bool
    -> t * Type.t

  val gather
    :  _ pattern
    -> name_table:Name_id.Name_table.t
    -> current_path:Module_path.t
    -> in_expr:bool
    -> t

  val find : t -> Value_name.t -> Name_bindings.Name_entry.t option
  val mem : t -> Value_name.t -> bool

  val fold
    :  t
    -> init:'acc
    -> f:('acc -> Value_name.t -> Name_bindings.Name_entry.t -> 'acc)
    -> 'acc

  val merge
    :  t
    -> t
    -> combine:
         (key:Value_name.t
          -> Name_bindings.Name_entry.t
          -> Name_bindings.Name_entry.t
          -> Name_bindings.Name_entry.t)
    -> t

  val fold_name_diff
    :  t
    -> t
    -> init:'acc
    -> f:
         ('acc
          -> Value_name.t
          -> [ `Left
             | `Right
             | `Both of Name_bindings.Name_entry.t * Name_bindings.Name_entry.t
             ]
          -> 'acc)
    -> 'acc
end = struct
  type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

  let empty = Value_name.Map.empty
  let to_map = Fn.id
  let name_set = Map.key_set

  (* TODO: consider making this a map to types directly, since let_inferred is always used *)
  let add_name t name typ ~name_table ~current_path ~in_expr =
    let name_id = Name_id.create_value_name name_table (current_path, name) ~in_expr in
    let name_entry = Name_bindings.Name_entry.let_inferred name_id typ in
    match Map.add t ~key:name ~data:name_entry with
    | `Ok pat_names -> pat_names
    | `Duplicate ->
      Name_bindings.name_error_msg
        "Duplicate name in pattern"
        (Value_name.to_ustring name)
  ;;

  let add_fresh_name pat_names name ~name_table ~current_path ~in_expr =
    let var = Type.fresh_var () in
    add_name pat_names name var ~name_table ~current_path ~in_expr, var
  ;;

  let gather pattern ~name_table ~current_path ~in_expr =
    fold_names pattern ~init:Value_name.Map.empty ~f:(fun pat_names name ->
      fst (add_fresh_name pat_names name ~name_table ~current_path ~in_expr))
  ;;

  let fold t ~init ~f = Map.fold t ~init ~f:(fun ~key ~data acc -> f acc key data)
  let find = Map.find
  let mem = Map.mem
  let merge = Map.merge_skewed

  let fold_name_diff t t' ~init ~f =
    Map.fold_symmetric_diff
      t
      t'
      ~init
      ~data_equal:(fun _ _ -> true)
      ~f:(fun acc (name, diff) ->
        let diff =
          match diff with
          | `Left _ -> `Left
          | `Right _ -> `Right
          | `Unequal entries -> `Both entries
        in
        f acc name diff)
  ;;
end
