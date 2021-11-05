open Import
open Names

module Binding = struct
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Qualified.Set.t
    ; info : 'a
    }
  [@@deriving sexp]
end

module Binding_id = Unique_id.Int ()
module G = Graph.Imperative.Digraph.Concrete (Binding_id)

(* NOTE: scc gives components numbered in topological order! *)
module Topologically_sorted_components = Graph.Components.Make (G)

type 'a t =
  { graph : G.t
  ; binding_table : ('a Binding.t * Name_bindings.Path.t) Binding_id.Table.t
  }

let create () = { graph = G.create (); binding_table = Binding_id.Table.create () }

let add_binding t new_binding new_path =
  let new_id = Binding_id.create () in
  Hashtbl.set t.binding_table ~key:new_id ~data:(new_binding, new_path);
  G.add_vertex t.graph new_id;
  G.iter_vertex
    (fun old_id ->
      (* TODO: test if these [Module_path] checks are fooled by aliasing/imports *)
      let old_binding, old_path = Hashtbl.find_exn t.binding_table old_id in
      let old_path = Name_bindings.Path.to_module_path old_path in
      let new_path = Name_bindings.Path.to_module_path new_path in
      (* Check if the new binding used any names bound by the old binding *)
      if Set.exists new_binding.used_names ~f:(fun (path, name) ->
           Module_path.equal path old_path && Set.mem old_binding.bound_names name)
      then G.add_edge t.graph new_id old_id;
      (* Check if the old binding used any names bound by the new binding *)
      if Set.exists old_binding.used_names ~f:(fun (path, name) ->
           Module_path.equal path new_path && Set.mem new_binding.bound_names name)
      then G.add_edge t.graph old_id new_id)
    t.graph
;;

let of_bindings bindings =
  let t = create () in
  Sequence.iter bindings ~f:(fun (binding, path) -> add_binding t binding path);
  t
;;

let to_regrouped_bindings t =
  Topologically_sorted_components.scc_list t.graph
  |> Sequence.of_list
  |> Sequence.map ~f:(function
       | [] ->
         compiler_bug [%message "Topologically_sorted_components: empty binding group"]
       | id :: ids ->
         let binding, path = Hashtbl.find_exn t.binding_table id in
         let bindings =
           List.map ids ~f:(fun id ->
             let binding, path' = Hashtbl.find_exn t.binding_table id in
             if not (Name_bindings.Path.equal path path')
             then (
               let msg =
                 "Mutually recursive functions are not allowed across module boundaries"
               in
               let module1 = Name_bindings.Path.to_module_path path in
               let module2 = Name_bindings.Path.to_module_path path' in
               Compilation_error.raise
                 Type_error
                 ~msg:[%message msg (module1 : Module_path.t) (module2 : Module_path.t)]);
             binding)
         in
         Nonempty.(binding :: bindings), path)
;;
