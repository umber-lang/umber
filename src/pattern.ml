open Import
open Names

(* TODO: consider functorising this so we can have some changes from untyped to
   typed patterns, e.g.
   - No more type annotations without needing a type parameter
   - Replacing record expressions like `{ name }` with `{ name = name }`
   - Putting record field names in a canonical order, probably in a map *)
type ('typ, 'name) t =
  | Constant of Literal.t
  | Catch_all of 'name option
  | As of ('typ, 'name) t * 'name
  | Cnstr_appl of Cnstr_name.Qualified.t * ('typ, 'name) t list
  | Tuple of ('typ, 'name) t list
  | Record of ('name * ('typ, 'name) t option) Nonempty.t
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

module Names : sig
  type ('typ, 'name) pattern := ('typ, 'name) t
  type t [@@deriving sexp]

  val empty : t
  val name_set : t -> Value_name.Set.t
  val add_name : t -> Value_name.t -> Name_id.t -> Type.t -> t

  val gather
    :  (_, Value_name.t) pattern
    -> name_table:Name_id.Name_table.t
    -> current_path:Module_path.t
    -> in_expr:bool
    -> t

  val find : t -> Value_name.t -> (Name_id.t * Type.t) option
  val mem : t -> Value_name.t -> bool

  val fold
    :  t
    -> init:'acc
    -> f:('acc -> Value_name.t -> Name_id.t * Type.t -> 'acc)
    -> 'acc

  val merge
    :  t
    -> t
    -> combine:
         (key:Value_name.t
          -> Name_id.t * Type.t
          -> Name_id.t * Type.t
          -> Name_id.t * Type.t)
    -> t

  val fold_name_diff
    :  t
    -> t
    -> init:'acc
    -> f:
         ('acc
          -> Value_name.t
          -> [ `Left | `Right | `Both of (Name_id.t * Type.t) * (Name_id.t * Type.t) ]
          -> 'acc)
    -> 'acc
end = struct
  type t = (Name_id.t * Type.t) Value_name.Map.t [@@deriving sexp]

  let empty = Value_name.Map.empty
  let name_set = Map.key_set

  let add_name t name name_id typ =
    match Map.add t ~key:name ~data:(name_id, typ) with
    | `Ok pat_names -> pat_names
    | `Duplicate ->
      Name_bindings.name_error_msg
        "Duplicate name in pattern"
        (Value_name.to_ustring name)
  ;;

  let gather pattern ~name_table ~current_path ~in_expr =
    fold_names pattern ~init:Value_name.Map.empty ~f:(fun t name ->
      let name_id = Name_id.create_value_name name_table (current_path, name) ~in_expr in
      add_name t name name_id (Type.fresh_var ()))
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
