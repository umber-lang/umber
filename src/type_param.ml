open Import
open Names
module T = Type_param_name
include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

module Map = struct
  include Map
  include Map.Provide_hash (T)
end

let dummy = Type_param_name.default

module Env_to_vars : sig
  type t

  val create : unit -> t
  val find_or_add : t -> Type_param_name.t -> Type_var.t
end = struct
  type nonrec t = (Type_param_name.t, Type_var.t) Hashtbl.t

  let create () = Type_param_name.Table.create ()
  let find_or_add = Hashtbl.find_or_add ~default:Type_var.create
end

module Env_of_vars : sig
  type t

  val create : unit -> t
  val find_or_add : t -> Type_var.t -> Type_param_name.t
end = struct
  type nonrec t =
    { table : (Type_var.t, Type_param_name.t) Hashtbl.t
    ; mutable next_param : Type_param_name.t
    }

  let create () =
    { table = Hashtbl.create (module Type_var); next_param = Type_param_name.default }
  ;;

  let find_or_add t =
    Hashtbl.find_or_add t.table ~default:(fun () ->
      let param = t.next_param in
      t.next_param <- Type_param_name.next t.next_param;
      param)
  ;;
end
