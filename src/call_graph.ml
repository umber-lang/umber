(* TODO: cleanup *)
(* See:
   https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Persistent/Digraph/index.html 
   https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Components/Make/index.html
   
   Control flow analysis:
   https://backtracking.github.io/ocamlgraph/ocamlgraph/Graph/Fixpoint/index.html
   https://cseweb.ucsd.edu/classes/sp00/cse231/report/node12.html
   https://en.wikipedia.org/wiki/Data-flow_analysis *)

open Import

(*open Graph
module Digraph = Imperative.Digraph.Concrete

module T = struct
  type binding = 
  type t = 
end

module Topologically_sorted_components (G : Components.G) = struct
  (* NOTE: scc gives components numbered in topological order! *)
  module C = Components.Make (G)

  module T = Topological.Make (struct
    type t = G.t

    module V = struct
      type t = G.V.t list

      let compare = List.compare G.V.compare
      let equal = List.equal G.V.equal

      let hash =
        List.hash_fold_t
          (fun state v -> G.V.hash v |> Hash.fold_int state)
          (Hash.create ())
        >> Hash.get_hash_value
      ;;
    end

    let cache = ref None

    let check_cache t =
      match !cache with
      | Some (t', cached) when phys_equal t t' -> cached
      | _ ->
        let new_cached = C.scc_array t,  in
        cache := Some (t, new_cached);
        new_cached
    ;;

    (** Iterate over all the strongly connected components *)
    let iter_vertex f t =
      let components = check_cache t in
      Array.iter components ~f
    ;;

    (** Iterate over the successors of a component (outgoing neighbours) *)
    let iter_succ f t v =
      let components = check_cache t in
      Array.iter components ~f:(fun comp -> )
    ;;
  end)
end*)

open Names

module Binding = struct
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Qualified.Set.t
    ; info : 'a
    }
end

module Binding_id = Unique_id.Int ()
module G = Graph.Imperative.Digraph.Concrete (Binding_id)

(* NOTE: scc gives components numbered in topological order! *)
module Topologically_sorted_components = Graph.Components.Make (G)

type 'a t =
  { graph : G.t
  ; binding_table : ('a Binding.t * Module_path.t) Binding_id.Table.t
  }

let create () = { graph = G.create (); binding_table = Binding_id.Table.create () }

let add_binding t new_binding new_path =
  let new_id = Binding_id.create () in
  Hashtbl.set t.binding_table ~key:new_id ~data:(new_binding, new_path);
  G.add_vertex t.graph new_id;
  G.iter_vertex
    (fun old_id ->
      let old_binding, old_path = Hashtbl.find_exn t.binding_table old_id in
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
             if not (Module_path.equal path path')
             then
               Type_bindings.type_error_msg
                 "Mutually recursive functions are not allowed across module boundaries";
             binding)
         in
         binding :: bindings, path)
;;
