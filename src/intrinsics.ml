open Import
open Names

let std_module_name = Module_name.of_string_unchecked "Std"
let prelude_module_name = Module_name.of_string_unchecked "Prelude"

let prelude_module_path =
  Module_path.Absolute.of_relative_unchecked
    (Module_path.Relative.of_module_names [ std_module_name; prelude_module_name ])
;;

module type Type = sig
  val name : Type_name.t
  val decl : Module_path.absolute Type_decl.t
  val typ : Module_path.absolute Type_scheme.t
end

module type Variants = sig
  include Type

  val cnstrs : (Cnstr_name.t * Extern_name.t) list
  val decl : Module_path.absolute Type_decl.t
end

module Make_variants (T : sig
  val name : string
  val cnstrs : string list
end) : Variants = struct
  let name = Type_name.of_string_unchecked T.name
  let typ : _ Type_scheme.t = Type_app ((Module_path.Absolute.empty, name), []), []

  let cnstrs =
    List.map T.cnstrs ~f:(fun cnstr_name ->
      ( Cnstr_name.of_string_unchecked cnstr_name
      , Extern_name.of_string_exn [%string "%%{String.uncapitalize cnstr_name}"] ))
  ;;

  let decl =
    Unique_list.empty, Type_decl.Variants (List.map cnstrs ~f:(fun (name, _) -> name, []))
  ;;
end

module Make_type (T : sig
  val name : string
  val decl : Module_path.absolute Type_decl.t
end) : Type = struct
  let name = Type_name.of_string_unchecked T.name
  let decl = T.decl
  let typ : _ Type_scheme.t = Type_app ((Module_path.Absolute.empty, name), []), []
end

module Make_abstract (T : sig
  val name : string
end) : Type = Make_type (struct
  let name = T.name
  let decl = Unique_list.empty, Type_decl.Abstract
end)

module Bool = struct
  include Make_variants (struct
    let name = "Bool"
    let cnstrs = [ "False"; "True" ]
  end)

  let false_, true_ =
    match cnstrs with
    | [ (false_, _); (true_, _) ] ->
      (Module_path.Absolute.empty, false_), (Module_path.Absolute.empty, true_)
    | _ -> assert false
  ;;
end

module Int = Make_abstract (struct
  let name = "Int"
end)

module Float = Make_abstract (struct
  let name = "Float"
end)

module Char = Make_abstract (struct
  let name = "Char"
end)

module String = Make_abstract (struct
  let name = "String"
end)

(* TODO: Provide better support for defining custom uninhabited types as variant types
   with no constructors. *)
module Never = Make_type (struct
  let name = "Never"
  let decl : _ Type_decl.t = Unique_list.empty, Alias (Union [])
end)

module Any = Make_type (struct
  let name = "Any"
  let decl : _ Type_decl.t = Unique_list.empty, Alias (Intersection [])
end)

(* TODO: Add intrinsics for Never and Any (Bottom and Top) *)

let all : (module Type) list =
  [ (module Bool)
  ; (module Int)
  ; (module Float)
  ; (module Char)
  ; (module String)
  ; (module Never)
  ; (module Any)
  ]
;;
