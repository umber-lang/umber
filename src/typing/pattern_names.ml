open! Core
open! Import
open Names

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
