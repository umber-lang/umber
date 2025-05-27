open! Core
open! Import
open Names

(* TODO: Make this a CST, with it precisely representing the syntax written (including
   comments). This will be helpful for writing an auto-formatter. *)

module Pattern = struct
  type t =
    | Constant of Literal.t
    | Catch_all of Value_name.t option
    | As of t * Value_name.t
    | Cnstr_appl of Cnstr_name.Relative.t * t list
    | Tuple of t list
    | Record of (Value_name.t * t option) Nonempty.t
    | Union of t * t
    | Type_annotation of t * Module_path.relative Type_scheme.t
  [@@deriving equal, sexp, variants]

  let fold_names pat ~init ~f =
    let rec loop acc ~f = function
      | Catch_all (Some name) -> f acc name
      | As (pat, name) -> loop (f acc name) ~f pat
      | Cnstr_appl (_, items) | Tuple items -> List.fold items ~init:acc ~f:(loop ~f)
      | Record fields ->
        Nonempty.fold fields ~init:acc ~f:(fun acc -> function
          | name, None -> f acc name
          | _, Some pat -> loop acc ~f pat)
      | Union (pat, _) ->
        (* Both branches bind the same names, so only one need be considered *)
        loop acc ~f pat
      | Type_annotation (pat, _) -> loop acc ~f pat
      | Constant _ | Catch_all None -> acc
    in
    loop init ~f pat
  ;;
end

module Effect_pattern = struct
  type t =
    { operation : Value_name.Relative.t
    ; args : Pattern.t Node.t Nonempty.t
    }
  [@@deriving equal, sexp]
end

module Expr = struct
  type t =
    | Literal of Literal.t
    | Name of Value_name.Relative.t
    | Qualified of Module_path.Relative.t * t Node.t
    | Fun_call of t Node.t * t Node.t Nonempty.t
    | Op_tree of (Value_name.Relative.t Node.t, t Node.t) Btree.t
    | Op_section of
        { op_side : [ `Left | `Right ]
        ; op : Value_name.Relative.t Node.t
        ; expr : t Node.t
        }
    | Lambda of Pattern.t Node.t Nonempty.t * t Node.t
    | If of t Node.t * t Node.t * t Node.t
    | Match of t Node.t * (Pattern.t Node.t * t Node.t) Nonempty.t
    | Match_function of (Pattern.t Node.t * t Node.t) Nonempty.t
    | Handle of
        t Node.t
        * ([ `Effect of Effect_pattern.t | `Value of Pattern.t ] Node.t * t Node.t)
          Nonempty.t
    | Let of (Pattern.t, t) Let_binding.t
    | Tuple of t Node.t list
    | Seq_literal of t Node.t list
    | Record_literal of (Value_name.t * t Node.t option) Nonempty.t
    | Record_update of t Node.t * (Value_name.t * t Node.t option) Nonempty.t
    | Record_field_access of t Node.t * Value_name.t Node.t
    | Type_annotation of t Node.t * Module_path.relative Type_scheme.t Node.t
  [@@deriving equal, sexp, variants]

  let qualified path expr =
    match path with
    | [] -> Node.with_value expr ~f:Fn.id
    | _ :: _ -> Qualified (Module_path.Relative.of_ustrings_unchecked path, expr)
  ;;
end

let create_effect_operation sig_ : _ Effect.Operation.t =
  match (sig_ : _ Module.sig_) with
  | Val (_, Some _, _) ->
    Compilation_error.raise
      Other
      ~msg:[%message "Fixity declarations are not supported on effect operations"]
  | Val (name, None, ((scheme, constraints) as type_)) ->
    if not (List.is_empty constraints)
    then failwith "TODO: constraints on effect operations";
    (match scheme with
     | Function (args, Effect_union [], result) -> { name; args; result }
     | Function (_, _, _) ->
       Compilation_error.raise
         Other
         ~msg:
           [%message "Effect operations can't perform effects" (type_ : _ Type_scheme.t)]
     | Var _ | Type_app _ | Tuple _ | Union _ | Intersection _ ->
       Compilation_error.raise
         Other
         ~msg:[%message "Effect operations must be functions" (type_ : _ Type_scheme.t)])
  | Module_sig _ | Trait_sig _ | Common_sig (Extern _ | Type_decl _ | Effect _ | Import _)
    ->
    Compilation_error.raise
      Other
      ~msg:
        [%message
          [%string "This kind of statement is not allowed inside an effect declaration"]
            (sig_ : _ Module.sig_)]
;;

let create_effect params sigs : _ Effect.t =
  let operations =
    Option.map sigs ~f:(List.map ~f:(Node.with_value ~f:create_effect_operation))
  in
  { params; operations }
;;

module Let_binding_group = struct
  type t = (Pattern.t Node.t * Fixity.t option * Expr.t Node.t) Nonempty.t
  [@@deriving sexp_of]
end

module Module = struct
  include Module

  type nonrec t = (Let_binding_group.t, Module_path.relative) t [@@deriving sexp_of]
  type nonrec sig_ = Module_path.relative sig_ [@@deriving sexp_of]
  type nonrec def = (Let_binding_group.t, Module_path.relative) def [@@deriving sexp_of]
end
