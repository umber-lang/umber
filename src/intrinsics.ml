open Import
open Names

(* TODO: should make this more robust - it's not sufficient for compiler internals to
   refer to types like Int as Std.Prelude.Int since Std could be shadowed - they need a
   special primitive representation *)

let std_module_name = Module_name.of_string_unchecked "Std"
let prelude_module_name = Module_name.of_string_unchecked "Prelude"
let prelude_module_path = [ std_module_name; prelude_module_name ]

module type Type = sig
  val name : Type_name.t
  val decl : Type.Decl.t
  val typ : Type.Concrete.t
end

module type Variants = sig
  include Type

  val cnstrs : (Cnstr_name.t * Extern_name.t) list
  val decl : Type.Decl.t
end

module Make_variants (T : sig
  val name : string
  val cnstrs : string list
end) : Variants = struct
  let name = Type_name.of_string_unchecked T.name
  let typ = Type.Expr.Type_app (([], name), [])

  let cnstrs =
    List.map T.cnstrs ~f:(fun cnstr_name ->
      ( Cnstr_name.of_string_unchecked cnstr_name
      , Extern_name.of_string_exn [%string "%%{String.uncapitalize cnstr_name}"] ))
  ;;

  let decl = [], Type.Decl.Variants (List.map cnstrs ~f:(fun (name, _) -> name, []))
end

module type Abstract = Type

module Make_abstract (T : sig
  val name : string
end) : Abstract = struct
  let name = Type_name.of_string_unchecked T.name
  let decl = [], Type.Decl.Abstract
  let typ = Type.Expr.Type_app (([], name), [])
end

module Bool = struct
  include Make_variants (struct
    let name = "Bool"
    let cnstrs = [ "False"; "True" ]
  end)

  let false_, true_ =
    match cnstrs with
    | [ (false_, _); (true_, _) ] -> ([], false_), ([], true_)
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
