open Import
open Names
module Var_id = Unique_id.Int ()

module Param = struct
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
    val find_or_add : t -> Type_param_name.t -> Var_id.t
  end = struct
    type nonrec t = (Type_param_name.t, Var_id.t) Hashtbl.t

    let create () = Type_param_name.Table.create ()
    let find_or_add = Hashtbl.find_or_add ~default:Var_id.create
  end

  module Env_of_vars : sig
    type t

    val create : unit -> t
    val find_or_add : t -> Var_id.t -> Type_param_name.t
  end = struct
    type nonrec t =
      { table : (Var_id.t, Type_param_name.t) Hashtbl.t
      ; mutable next_param : Type_param_name.t
      }

    let create () =
      { table = Hashtbl.create (module Var_id); next_param = Type_param_name.default }
    ;;

    let find_or_add t =
      Hashtbl.find_or_add t.table ~default:(fun () ->
        let param = t.next_param in
        t.next_param <- Type_param_name.next t.next_param;
        param)
    ;;
  end
end

(* TODO: Type declarations/expressions should have spans like other parts of the AST do. *)

module Expr = struct
  type ('v, 'pf, 'n) t =
    | Var of 'v
    | Type_app of 'n Type_name.Qualified.t * ('v, 'pf, 'n) t list
    | Tuple of ('v, 'pf, 'n) t list
    | Function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * ('v, 'pf, 'n) t
    | Partial_function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) effects * 'pf

  (* FIXME: It should be an effect path, not a name *)
  and ('v, 'pf, 'n) effects =
    { effects : ('v, 'pf, 'n) t list Map.M(Effect_name).t
    ; effect_var : 'v option
    }
  [@@deriving hash, compare, equal, sexp]

  let var v = Var v
  let tuple list = Tuple list

  let rec map ?(f = Map_action.defer) typ ~var ~pf ~name =
    match f typ with
    | Halt typ -> typ
    | Retry typ -> map typ ~f ~var ~pf ~name
    | Defer typ ->
      (match typ with
       | Var v -> Var (var v)
       | Type_app (name', fields) ->
         Type_app (name name', List.map fields ~f:(map ~f ~var ~pf ~name))
       | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var ~pf ~name))
       | Function (args, effects, body) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
         let effects = map_effects effects ~f ~var ~pf ~name in
         Function (args, effects, map ~f ~var ~pf ~name body)
       | Partial_function (args, effects, v) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
         let effects = map_effects effects ~f ~var ~pf ~name in
         Partial_function (args, effects, pf v))

  and map_effects { effects; effect_var } ~f ~var ~pf ~name =
    let effects =
      Map.map effects ~f:(fun args -> List.map args ~f:(map ~f ~var ~pf ~name))
    in
    let effect_var = Option.map effect_var ~f:var in
    { effects; effect_var }
  ;;

  let rec map2 ?(f = Map_action.defer) ?(f_contra = f) type1 type2 ~var ~pf ~name ~eff =
    match f (type1, type2) with
    | Halt typ -> typ
    | Retry (type1, type2) -> map2 type1 type2 ~f ~f_contra ~var ~pf ~name ~eff
    | Defer (type1, type2) ->
      (match type1, type2 with
       | Var v1, Var v2 -> Var (var v1 v2)
       | Type_app (name1, args1), Type_app (name2, args2) ->
         assert_or_compiler_bug
           ~here:[%here]
           ([%equal: _ Type_name.Qualified.t] name1 name2);
         Type_app
           ( name name1
           , List.map2_exn args1 args2 ~f:(map2 ~f ~f_contra ~var ~pf ~name ~eff) )
       | Tuple fields1, Tuple fields2 ->
         Tuple (List.map2_exn fields1 fields2 ~f:(map2 ~f ~f_contra ~var ~pf ~name ~eff))
       | Function (args1, effect_row1, res1), Function (args2, effect_row2, res2) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~name ~eff)
         in
         let effect_row = eff effect_row1 effect_row2 in
         let res = map2 res1 res2 ~f ~f_contra ~var ~pf ~name ~eff in
         Function (args, effect_row, res)
       | ( Partial_function (args1, effect_row1, v1)
         , Partial_function (args2, effect_row2, v2) ) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~name ~eff)
         in
         let effect_row = eff effect_row1 effect_row2 in
         let v = pf v1 v2 in
         Partial_function (args, effect_row, v)
       | Partial_function _, Function _
       | Function _, Partial_function _
       | Var _, (Type_app _ | Tuple _ | Function _ | Partial_function _)
       | Type_app _, (Var _ | Tuple _ | Function _ | Partial_function _)
       | Tuple _, (Var _ | Type_app _ | Function _ | Partial_function _)
       | Function _, (Var _ | Type_app _ | Tuple _)
       | Partial_function _, (Var _ | Type_app _ | Tuple _) ->
         compiler_bug
           [%message
             "Incompatible types for map2" (type1 : (_, _, _) t) (type2 : (_, _, _) t)])
  ;;

  let rec fold_until typ ~init ~f =
    match (f init typ : _ Fold_action.t) with
    | Stop _ as stop -> stop
    | Continue init as continue ->
      (match typ with
       | Var _ -> continue
       | Type_app (_, fields) | Tuple fields ->
         List.fold_until fields ~init ~f:(fun init -> fold_until ~init ~f)
       | Function (args, _, body) ->
         let%bind.Fold_action init =
           Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
         in
         fold_until body ~init ~f
       | Partial_function (args, _, _) ->
         Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f))
  ;;

  let fold_vars typ ~init ~f =
    fold_until typ ~init ~f:(fun acc -> function
      | Var var -> Continue (f acc var)
      | _ -> Continue acc)
    |> Fold_action.id
  ;;

  let for_all_vars typ ~f =
    fold_until typ ~init:true ~f:(fun _ -> function
      | Var var -> if f var then Continue true else Stop false
      | _ -> Continue true)
    |> Fold_action.id
  ;;

  let exists_var typ ~f =
    fold_until typ ~init:false ~f:(fun _ -> function
      | Var var -> if f var then Stop true else Continue false
      | _ -> Continue false)
    |> Fold_action.id
  ;;

  (* FIXME: Idea: sequenced actions add effects to the environment. Conceptually, there
     are a set of effects. Complicating this is effect polymorphism. Calls to function
     parameters produce some variable set of effects, represented with effect variables.
     When combining calls to multiple functions, we want to include both effect variables.
     We can do this by unifying them.
     
     Let's remove effect variables which are unified later. Also want to remove
     duplicates later. *)
  (* let union_effects effects1 effects2 = effects1 @ effects2 *)

  (* FIXME: Maybe just make effects a set? *)
  (* FIXME: Since we don't consider unified type variables to be equal, this may give 
     stricter types than necessary. Integrate this and union with `Type_bindings` so it
     can deal with this.
     
     Is this even going to be correct? Maybe effect_row needs to explicitly model
     intersection? *)
  (* let intersect_effects effects1 effects2 =
    List.filter
      effects1
      ~f:(List.mem effects2 ~equal:[%equal: (Var_id.t, Var_id.t, _) effect])
  ;; *)

  let no_effects = { effects = Effect_name.Map.empty; effect_var = None }
end

module T = struct
  type t = (Var_id.t, Var_id.t, Module_path.absolute) Expr.t
  [@@deriving compare, hash, equal, sexp]
end

include T
include Hashable.Make (T)

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  type nonrec 'n t = (Param.t, Nothing.t, 'n) Expr.t
  [@@deriving compare, hash, equal, sexp]

  type nonrec 'n effects = (Param.t, Nothing.t, 'n) Expr.effects
  [@@deriving compare, hash, equal, sexp]

  module Bounded = struct
    type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
  end

  let instantiate ?params typ =
    let params = Option.value_or_thunk params ~default:Param.Env_to_vars.create in
    Expr.map
      typ
      ~var:(Param.Env_to_vars.find_or_add params)
      ~pf:Nothing.unreachable_code
      ~name:Fn.id
  ;;

  (* TODO: handle trait bounds *)
  let instantiate_bounded ?params typ =
    match typ with
    | [], typ -> instantiate ?params typ
    | _ -> raise_s [%message "Trait bounds not yet implemented"]
  ;;
end

module Concrete = struct
  module T = struct
    type t = (Nothing.t, Nothing.t, Module_path.absolute) Expr.t
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let cast t =
    Expr.map t ~var:Nothing.unreachable_code ~pf:Nothing.unreachable_code ~name:Fn.id
  ;;
end

module Decl = struct
  type 'n decl =
    | Abstract
    | Alias of 'n Scheme.t
    (* TODO: variant constructors should probably support fixity declarations *)
    | Variants of (Cnstr_name.t * 'n Scheme.t list) list
    (* TODO: probably just make records a type expression - you can trivially get nominal
       records with a single variant and an inline record. One problem with this is you
       can no longer define recursive record types, which is a bit annoying. *)
    | Record of (Value_name.t * 'n Scheme.t) Nonempty.t
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
      Expr.for_all_vars
        typ
        ~f:(List.mem (params :> Type_param_name.t list) ~equal:Type_param_name.equal)
    in
    fun (params, decl) ->
      match decl with
      | Alias expr -> check_params params expr
      | Abstract -> true
      | Variants cnstrs ->
        List.for_all cnstrs ~f:(fun (_, args) ->
          List.for_all args ~f:(check_params params))
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
end
