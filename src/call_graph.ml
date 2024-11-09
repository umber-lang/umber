open Import
open Names

module Binding = struct
  type 'a t =
    { bound_names : Value_name.Set.t
    ; used_names : Value_name.Absolute.Set.t
    ; info : 'a
    }
  [@@deriving fields, sexp]
end

module Binding_id : sig
  type t [@@deriving compare, equal, hash]

  include Hashable.S with type t := t

  val make_creator : unit -> (unit -> t) Staged.t
end = struct
  include Int

  let make_creator () =
    let current = ref (-1) in
    stage (fun () ->
      incr current;
      !current)
  ;;
end

module G = Graph.Imperative.Digraph.Concrete (Binding_id)

(* NOTE: scc gives components numbered in topological order! *)
module Topologically_sorted_components = Graph.Components.Make (G)

type 'a t =
  { graph : G.t
  ; binding_table : ('a Binding.t * Module_path.Absolute.t) Binding_id.Table.t
  ; create_binding : unit -> Binding_id.t
  }

let create () =
  { graph = G.create ()
  ; binding_table = Binding_id.Table.create ()
  ; create_binding = unstage (Binding_id.make_creator ())
  }
;;

let add_binding t new_binding new_path =
  let new_id = t.create_binding () in
  Hashtbl.set t.binding_table ~key:new_id ~data:(new_binding, new_path);
  G.add_vertex t.graph new_id;
  G.iter_vertex
    (fun old_id ->
      let old_binding, old_path = Hashtbl.find_exn t.binding_table old_id in
      (* Check if the new binding used any names bound by the old binding *)
      if Set.exists new_binding.used_names ~f:(fun (path, name) ->
           Module_path.Absolute.equal path old_path
           && Set.mem old_binding.bound_names name)
      then G.add_edge t.graph new_id old_id;
      (* Check if the old binding used any names bound by the new binding *)
      if Set.exists old_binding.used_names ~f:(fun (path, name) ->
           Module_path.Absolute.equal path new_path
           && Set.mem new_binding.bound_names name)
      then G.add_edge t.graph old_id new_id)
    t.graph
;;

let of_bindings bindings =
  let t = create () in
  List.iter bindings ~f:(fun (binding, path) -> add_binding t binding path);
  t
;;

let to_regrouped_bindings t =
  Topologically_sorted_components.scc_list t.graph
  |> List.map ~f:(function
       | [] ->
         compiler_bug [%message "Topologically_sorted_components: empty binding group"]
       | id :: ids ->
         let binding, path = Hashtbl.find_exn t.binding_table id in
         let bindings =
           List.map ids ~f:(fun id ->
             let binding, path' = Hashtbl.find_exn t.binding_table id in
             if not (Module_path.Absolute.equal path path')
             then (
               let modules =
                 List.sort ~compare:[%compare: Module_path.Absolute.t] [ path; path' ]
               in
               Compilation_error.raise
                 Type_error
                 ~msg:
                   [%message
                     "Mutually recursive functions are not allowed across module \
                      boundaries"
                       (modules : Module_path.Absolute.t list)]);
             binding.info)
         in
         Nonempty.(binding.info :: bindings), path)
;;

let regroup_bindings bindings = of_bindings bindings |> to_regrouped_bindings
