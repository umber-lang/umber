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

let find_module t = Map.find t.modules
let remove_module t module_name = { t with modules = Map.remove t.modules module_name }
let fold_modules t = Map.fold t.modules
