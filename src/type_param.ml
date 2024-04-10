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

module Generator = struct
  type t = { mutable next_param_index : int }

  let create () = { next_param_index = 0 }

  let next t =
    let param = Type_param_name.generate_nth t.next_param_index in
    t.next_param_index <- t.next_param_index + 1;
    param
  ;;
end

module Env_to_vars = struct
  type nonrec t = (Type_param_name.t, Type_var.t) Hashtbl.t

  let create () = Type_param_name.Table.create ()
  let find_or_add = Hashtbl.find_or_add ~default:Type_var.create
end

module Env_of_vars = struct
  type nonrec t =
    { table : (Type_var.t, Type_param_name.t) Hashtbl.t
    ; generator : Generator.t
    }

  let sexp_of_t t = [%sexp (t.table : (Type_var.t, Type_param_name.t) Hashtbl.t)]

  let create () =
    { table = Hashtbl.create (module Type_var); generator = Generator.create () }
  ;;

  let find_or_add t =
    Hashtbl.find_or_add t.table ~default:(fun () -> Generator.next t.generator)
  ;;

  let find t var = Hashtbl.find t.table var
  let mem t var = Hashtbl.mem t.table var
end
