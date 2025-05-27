open! Core
open! Import
open Names

(* TODO: Should type declarations allow constraints? *)

type 'n decl =
  | Abstract
  | Alias of 'n Type_scheme.type_
  (* TODO: variant constructors should probably support fixity declarations *)
  | Variants of (Cnstr_name.t * 'n Type_scheme.type_ list) list
  (* TODO: probably just make records a type expression - you can trivially get nominal
     records with a single variant and an inline record. One problem with this is you
     can no longer define recursive record types, which is a bit annoying. *)
  | Record of (Value_name.t * 'n Type_scheme.type_) Nonempty.t
[@@deriving compare, equal, hash, sexp]

type 'n t = Type_param_name.t Unique_list.t * 'n decl
[@@deriving compare, equal, hash, sexp]

let arity ((params, _) : _ t) = List.length (params :> Type_param_name.t list)

let map_exprs (params, decl) ~f =
  ( params
  , match decl with
    | Abstract -> Abstract
    | Alias expr -> Alias (f expr)
    | Variants cnstrs -> Variants (List.map cnstrs ~f:(Tuple2.map_snd ~f:(List.map ~f)))
    | Record fields -> Record (Nonempty.map fields ~f:(Tuple2.map_snd ~f)) )
;;

let fold_exprs (_, decl) ~init:acc ~f =
  match decl with
  | Abstract -> acc
  | Alias expr -> f acc expr
  | Variants cnstrs ->
    List.fold cnstrs ~init:acc ~f:(fun acc -> snd >> List.fold ~init:acc ~f)
  | Record fields -> Nonempty.fold fields ~init:acc ~f:(fun acc -> snd >> f acc)
;;

let iter_exprs decl ~f = fold_exprs decl ~init:() ~f:(fun () -> f)

let no_free_params =
  let check_params (params : Type_param_name.t Unique_list.t) typ =
    Type_scheme.for_all_vars
      typ
      ~f:(List.mem (params :> Type_param_name.t list) ~equal:Type_param_name.equal)
  in
  fun (params, decl) ->
    match decl with
    | Alias expr -> check_params params expr
    | Abstract -> true
    | Variants cnstrs ->
      List.for_all cnstrs ~f:(fun (_, args) -> List.for_all args ~f:(check_params params))
    | Record fields ->
      Nonempty.for_all fields ~f:(fun (_, field) -> check_params params field)
;;

let params_of_list params =
  match Unique_list.of_list params ~compare:[%compare: Type_param_name.t] with
  | Ok params -> params
  | Error duplicate ->
    Compilation_error.raise
      Name_error
      ~msg:[%message "Duplicate type parameter name" (duplicate : Type_param_name.t)]
;;
