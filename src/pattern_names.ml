open Import
open Names

(* FIXME: Cleanup - maybe move to typed pattern *)
(* let rec map pat ~f =
  match (f pat : _ Map_action.t) with
  | Halt pat -> pat
  | Retry pat -> map pat ~f
  | Defer pat ->
    (match pat with
     | Constant _ | Catch_all _ -> pat
     | As (pat, name) -> As (map pat ~f, name)
     | Cnstr_appl (cnstr, args) -> Cnstr_appl (cnstr, List.map args ~f:(map ~f))
     | Tuple fields -> Tuple (List.map fields ~f:(map ~f))
     | Record fields ->
       Record (Nonempty.map fields ~f:(Tuple2.map_snd ~f:(Option.map ~f:(map ~f))))
     | Union (pat1, pat2) ->
       let pat1 = map pat1 ~f in
       let pat2 = map pat2 ~f in
       Union (pat1, pat2)
     | Type_annotation (pat, type_) -> Type_annotation (map pat ~f, type_))
;;

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
;; *)

type t = Name_bindings.Name_entry.t Value_name.Map.t [@@deriving sexp]

let empty = Value_name.Map.empty

let add_name pat_names name typ ~type_source ~fixity =
  let name_entry = Name_bindings.Name_entry.create typ ~type_source ?fixity in
  match Map.add pat_names ~key:name ~data:name_entry with
  | `Ok pat_names -> pat_names
  | `Duplicate ->
    Name_bindings.name_error ~msg:"Duplicate name in pattern" (Value_name.to_ustring name)
;;

let add_fresh_name pat_names name ~type_source ~fixity =
  let var = Internal_type.fresh_var () in
  add_name pat_names name var ~type_source ~fixity, var
;;

let gather pat ~type_source ~fixity ~fold =
  fold pat ~init:Value_name.Map.empty ~f:(fun pat_names name ->
    fst (add_fresh_name pat_names name ~type_source ~fixity))
;;

let find = Map.find
let mem = Map.mem
let merge = Map.merge_skewed
