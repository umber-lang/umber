open Import
open Names

type 'a t =
  { data : 'a
  ; modules : 'a t Module_name.Map.t
  }

let create data = { data; modules = Module_name.Map.empty }
let current t = t.data
let map t ~f = { t with data = f t.data }

let with_module t module_name ~f =
  { t with modules = Map.update t.modules module_name ~f }
;;

let remove_module t module_name = { t with modules = Map.remove t.modules module_name }
let fold_modules t = Map.fold t.modules

(* TODO: cleanup *)
(*module Make (Kind : Kind) = struct
  module Kind = Kind
  (*module Kind_id : sig 
    type t
    val of_kind : _ Kind.t -> t

    (* FIXME: the part where this can be any type is bad *)
    val to_kind : t -> _ Kind.t
  include Comparable.S with type t := t
  end = struct
  include Int
    let of_kind : _ Kind.t -> int = Obj.magic
    let to_kind : int -> _ Kind.t = Obj.magic
  end

  module Maps_by_kind : sig
    type t

    val empty : t
    val find : t -> kind:('k, 'v, 'cmp) Kind.t -> 'k -> 'v option

    val map
      :  t
      -> kind:('k, 'v, 'cmp) Kind.t
      -> f:(('k, 'v, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t)
      -> t

    val fold
      :  t
      -> init:'a
      -> f:(kind:('k, 'v, 'cmp) Kind.t -> key:'k -> data:'v -> 'a -> 'a)
      -> 'a
  end = struct
    module Packed_map = struct
    type t = T : ('k, 'v, 'cmp) Map.t -> t

      let unpack : t -> ('k, 'v, 'cmp) Kind.t -> ('k, 'v, 'cmp) Map.t =
        fun (T map) _ -> Obj.magic map
    end
    type t = Packed_map Kind_id.Map.t
    let empty = Int.Map.empty

    let find t ~kind key =
      Map.find t (Kind_id.of_kind kind)
      |> Option.bind ~f:(fun (Packed_map map) -> )
    ;;

    let map t ~kind ~f =
      Map.update t (int_of_kind kind) ~f:(function
        | Some data -> dummy_of_map (f (map_of_dummy kind data))
        | None -> dummy_of_map (f (Map.empty (Kind.comparator kind))))
    ;;

    let fold t ~init ~f =
      Map.fold t ~init ~f:(fun ~key ~data init ->
        let kind = kind_of_int key in
        Map.fold (map_of_dummy kind data) ~init ~f:(f ~kind))
    ;;
  end*)

  module Maps_by_kind : sig
    type t

    val empty : t
    val find : t -> kind:('k, 'v, 'cmp) Kind.t -> 'k -> 'v option

    val map
      :  t
      -> kind:('k, 'v, 'cmp) Kind.t
      -> f:(('k, 'v, 'cmp) Map.t -> ('k, 'v, 'cmp) Map.t)
      -> t

    val fold
      :  t
      -> init:'a
      -> f:(kind:('k, 'v, 'cmp) Kind.t -> key:'k -> data:'v -> 'a -> 'a)
      -> 'a
  end = struct 
    type t =
    | (::) : t
    | [] : t

    
  end

  type t =
    { maps_by_kind : Maps_by_kind.t
    ; modules : t Module_name.Map.t
    }

  let empty = { maps_by_kind = Maps_by_kind.empty; modules = Module_name.Map.empty }
  let find t ~kind = Maps_by_kind.find t.maps_by_kind ~kind

  let add_exn t ~kind ~key ~data =
    { t with
      maps_by_kind = Maps_by_kind.map t.maps_by_kind ~kind ~f:(Map.add_exn ~key ~data)
    }
  ;;

  let remove t ~kind key =
    { t with
      maps_by_kind = Maps_by_kind.map t.maps_by_kind ~kind ~f:(Fn.flip Map.remove key)
    }
  ;;

  let change t ~kind key ~f =
    { t with
      maps_by_kind = Maps_by_kind.map t.maps_by_kind ~kind ~f:(Fn.flip Map.change key ~f)
    }
  ;;

  let fold t = Maps_by_kind.fold t.maps_by_kind

  let with_module t module_name ~f =
    { t with
      modules = Map.update t.modules module_name ~f:(f << Option.value ~default:empty)
    }
  ;;

  let remove_module t module_name = { t with modules = Map.remove t.modules module_name }
  let fold_modules t = Map.fold t.modules
end*)

(* TODO: cleanup *)
(*type ('k, 'v, 'cmp) t =
  { data : ('k, 'v, 'cmp) Map.t
  ; modules : ('k, 'v, 'cmp) t Module_name.Map.t
  }

let empty comparator = { data = Map.empty comparator; modules = Module_name.Map.empty }
let find t = Map.find t.data
let add_exn t ~key ~data = { t with data = Map.add_exn t.data ~key ~data }
let remove t key = { t with data = Map.remove t.data key }
let change t key ~f = { t with data = Map.change t.data key ~f }
let fold t = Map.fold t.data

let with_module t module_name ~f =
  let empty = empty (Map.comparator_s t.data) in
  { t with
    modules = Map.update t.modules module_name ~f:(f << Option.value ~default:empty)
  }
;;

let remove_module t module_name = { t with modules = Map.remove t.modules module_name }
let fold_modules t = Map.fold t.modules*)
