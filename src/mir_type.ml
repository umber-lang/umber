open! Import
open Names

(* TODO: I don't think the primitive type actually makes sense: they are blocks too and
   should probably be represented as such here for consistency. *)
module Primitive = struct
  type t =
    | Int
    | Float
    | Char
    | String
  [@@deriving equal, sexp]

  let of_literal : Literal.t -> t = function
    | Int _ -> Int
    | Float _ -> Float
    | Char _ -> Char
    | String _ -> String
  ;;
end

type t =
  | Primitive of Primitive.t
  | Block of (Cnstr_tag.t * t Lazy.t list) Nonempty.t
  | Function of t Nonempty.t * t
  | Abstract
[@@deriving equal, sexp]

let assert_equal t1 t2 =
  if not (equal t1 t2) then compiler_bug [%message "Unequal types" (t1 : t) (t2 : t)]
;;

let expect_block = function
  | Block variants -> variants
  | type_ -> compiler_bug [%message "Unexpected non-block type" (type_ : t)]
;;

let expect_function = function
  | Function (arg_types, return_type) -> arg_types, return_type
  | type_ -> compiler_bug [%message "Unexpected non-function type" (type_ : t)]
;;

(* FIXME: stack overflow? *)
(* TODO: This doesn't handle polymorphic types particularly smartly. Should think about
   whether that matters. *)
let of_type_scheme =
  let rec of_type_scheme ~names ~params ~types_seen type_ =
    let of_tuple fields =
      Block
        [ ( Cnstr_tag.default
          , List.map fields ~f:(of_type_scheme ~names ~params ~types_seen) )
        ]
    in
    match (type_ : Type.Scheme.t) with
    | Var param -> Map.find params param |> Option.value ~default:Abstract
    | Tuple fields -> of_tuple fields
    | Type_app (type_name, args) ->
      let types_seen' = force types_seen in
      (match Map.find types_seen' (type_name, args) with
       | Some t -> t
       | None ->
         let rec types_seen = lazy (Map.set types_seen' ~key:(type_name, args) ~data:t)
         and t =
           lazy
             (let args = List.map args ~f:(of_type_scheme ~names ~params ~types_seen) in
              let decl_params, decl =
                Name_bindings.find_type_decl ~defs_only:true names type_name
              in
              let params =
                List.fold2_exn decl_params args ~init:params ~f:(fun params param arg ->
                  Map.set params ~key:param ~data:arg)
              in
              match decl with
              | Abstract -> Abstract
              | Variants variants ->
                (match Nonempty.of_list variants with
                 | None -> Abstract
                 | Some variants ->
                   (* FIXME: re-implementing mir logic here *)
                   let (_ : int * int), variants =
                     Nonempty.fold_map
                       variants
                       ~init:(0, 0)
                       ~f:(fun
                            (constant_tag, non_constant_tag)
                            ((_ : Cnstr_name.t), fields)
                          ->
                       let fields =
                         List.map fields ~f:(of_type_scheme ~names ~params ~types_seen)
                       in
                       let acc, tag =
                         if List.is_empty fields
                         then (constant_tag + 1, non_constant_tag), constant_tag
                         else (constant_tag, non_constant_tag + 1), non_constant_tag
                       in
                       acc, (Cnstr_tag.of_int tag, fields))
                   in
                   Block variants)
              | Record fields -> of_tuple (Nonempty.to_list fields |> List.map ~f:snd)
              | Alias type_ -> of_type_scheme ~names ~params ~types_seen type_)
         in
         force t)
    | Function (arg_types, return_type) ->
      Function
        ( Nonempty.map arg_types ~f:(of_type_scheme ~names ~params ~types_seen)
        , of_type_scheme ~names ~params ~types_seen return_type )
    | Partial_function _ -> .
  in
  let module Key = struct
    module T = struct
      type t = Type_name.Qualified.t * Type.Scheme.t list [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end
  in
  fun ~names type_ ->
    of_type_scheme
      ~names
      ~params:Type_param_name.Map.empty
      ~types_seen:(lazy Key.Map.empty)
      type_
;;

let arity = function
  | Primitive _ | Block _ | Abstract -> 0
  | Function (args, _) -> Nonempty.length args
;;
