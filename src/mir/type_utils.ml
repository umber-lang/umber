open! Core
open! Import

let rec arity_of_type ~names (type_ : Module_path.absolute Type_scheme.type_) =
  match type_ with
  | Var _ | Tuple _ -> 0
  | Type_app (type_name, _) ->
    (match
       snd (Name_bindings.find_absolute_type_decl ~defs_only:true names type_name)
     with
     | Abstract | Variants _ | Record _ -> 0
     | Alias type_ -> arity_of_type ~names type_)
  | Function (args, _, _) -> Nonempty.length args
  | Union types | Intersection types ->
    let types = Non_single_list.to_list types in
    let type_arities = List.map types ~f:(arity_of_type ~names) in
    (match List.all_equal type_arities ~equal:Int.equal with
     | Some arity -> arity
     | None ->
       compiler_bug
         [%message
           "Conflicting arities for union or intersection type"
             (type_ : _ Type_scheme.type_)
             (type_arities : int list)])
;;
