open Import
open Names
module Var_id = Unique_id.Int ()

module Param = struct
  (* TODO: need variance for type parameters (e.g. covariant, contravariant)
     Can probably wait a bit though -- it's needed for subtyping
     Variance can maybe be added as constraints on the type scheme *)

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

module Expr = struct
  type ('v, 'pf) t =
    | Var of 'v
    | Type_app of Type_name.Qualified.t * ('v, 'pf) t list
    | Tuple of ('v, 'pf) t list
    | Function of ('v, 'pf) t Nonempty.t * ('v, 'pf) effect_row * ('v, 'pf) t
    | Partial_function of ('v, 'pf) t Nonempty.t * ('v, 'pf) effect_row * 'pf

  (* FIXME: cleanup *)
  (* | Partial_effect of ('v, 'pf) t * ('pf, 'pf) effect_row *)
  and ('v, 'pf) effect_row = ('v, 'pf) effect list

  and ('v, 'pf) effect =
    | Effect of Effect_name.t * ('v, 'pf) t list
    | Effect_var of 'v
  [@@deriving hash, compare, equal, sexp]

  let var v = Var v
  let tuple list = Tuple list

  let rec map ?(f = Map_action.defer) typ ~var ~pf =
    match f typ with
    | Halt typ -> typ
    | Retry typ -> map typ ~f ~var ~pf
    | Defer typ ->
      (match typ with
       | Var v -> Var (var v)
       | Type_app (name, fields) -> Type_app (name, List.map fields ~f:(map ~f ~var ~pf))
       | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var ~pf))
       | Function (args, effect_row, body) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf) in
         let effect_row = List.map effect_row ~f:(map_effect ~f ~var ~pf) in
         Function (args, effect_row, map ~f ~var ~pf body)
       | Partial_function (args, effect_row, v) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf) in
         let effect_row = List.map effect_row ~f:(map_effect ~f ~var ~pf) in
         Partial_function (args, effect_row, pf v))

  and map_effect effect ~f ~var ~pf =
    match effect with
    | Effect_var v -> Effect_var (var v)
    | Effect (effect_name, args) ->
      Effect (effect_name, List.map args ~f:(map ~f ~var ~pf))
  ;;

  let rec map2 ?(f = Map_action.defer) ?(f_contra = f) type1 type2 ~var ~pf ~eff =
    match f (type1, type2) with
    | Halt typ -> typ
    | Retry (type1, type2) -> map2 type1 type2 ~f ~f_contra ~var ~pf ~eff
    | Defer (type1, type2) ->
      (match type1, type2 with
       | Var v1, Var v2 -> Var (var v1 v2)
       | Type_app (name1, args1), Type_app (name2, args2) ->
         assert_or_compiler_bug ~here:[%here] (Type_name.Qualified.equal name1 name2);
         Type_app (name1, List.map2_exn args1 args2 ~f:(map2 ~f ~f_contra ~var ~pf ~eff))
       | Tuple fields1, Tuple fields2 ->
         Tuple (List.map2_exn fields1 fields2 ~f:(map2 ~f ~f_contra ~var ~pf ~eff))
       | Function (args1, effect_row1, res1), Function (args2, effect_row2, res2) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~eff)
         in
         let effect_row = eff effect_row1 effect_row2 in
         let res = map2 res1 res2 ~f ~f_contra ~var ~pf ~eff in
         Function (args, effect_row, res)
       | ( Partial_function (args1, effect_row1, v1)
         , Partial_function (args2, effect_row2, v2) ) ->
         let args =
           Nonempty.map2 args1 args2 ~f:(map2 ~f:f_contra ~f_contra:f ~var ~pf ~eff)
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
           [%message "Incompatible types for map2" (type1 : (_, _) t) (type2 : (_, _) t)])
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
  let union_effects effects1 effects2 = effects1 @ effects2

  (* FIXME: Maybe just make effects a set? *)
  (* TODO: Since we don't consider unified type variables to be equal, this may give 
     stricter types than necessary. Integrate this and union with `Type_bindings` so it
     can deal with this. *)
  let intersect_effects effects1 effects2 =
    List.filter
      effects1
      ~f:(List.mem effects2 ~equal:[%equal: (Var_id.t, Var_id.t) effect])
  ;;

  (* FIXME: function variance goes from contra -> co.
     You can reduce the set of possible inputs or increase the set of possible outputs
     and get a supertype.
     A -> B is a subtype of C -> D iff C is a subtype of A and B is a subtype of D.

     When we take the "union" of two function types, we're trying to find a function
     type which is a supertype of both of them. Maybe we can just consider the
     effect types? Hmm, but now we've introduced subtyping in, it might be anywhere.

     e.g. For
     `Int -> <> (Int -> <Foo> Int)`
     and
     `Int -> <> (Int -> <Bar> Int)`
     we can say the union is `Int -> <> (Int -> <Foo,Bar> Int)`

     And for
     `(Int -> <Foo> Int) -> <> Int`
     and
     `(Int -> <Bar> Int) -> <> Int`
     we can say the union is `(Int -> <> Int) -> <> Int` by finding a type which is
     a subtype of both of the effects in the argument functions. So this is
     intersection instead of union.
  *)
  let rec union t1 t2 =
    map2 t1 t2 ~var:Fn.const ~pf:Fn.const ~eff:union_effects ~f_contra:(fun (t1, t2) ->
      Halt (intersect t1 t2))

  and intersect t1 t2 =
    map2
      t1
      t2
      ~var:Fn.const
      ~pf:Fn.const
      ~eff:intersect_effects
      ~f_contra:(fun (t1, t2) -> Halt (union t1 t2))
  ;;

  let total_effect = []
  let effect_is_total = List.is_empty
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  module T = struct
    (* TODO: add trait constraints to this type here *)
    type nonrec t = (Param.t, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp]

    type nonrec effect = (Param.t, Nothing.t) Expr.effect
    [@@deriving compare, hash, equal, sexp]

    type nonrec effect_row = (Param.t, Nothing.t) Expr.effect_row
    [@@deriving compare, hash, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  module Bounded = struct
    type nonrec t = Trait_bound.t * t [@@deriving compare, equal, hash, sexp]
  end

  let instantiate ?(map_name = Fn.id) ?params typ =
    let params = option_or_default params ~f:Param.Env_to_vars.create in
    Expr.map
      typ
      ~var:(Param.Env_to_vars.find_or_add params)
      ~f:
        (function
         | Type_app (name, args) -> Defer (Expr.Type_app (map_name name, args))
         | typ -> Defer typ)
      ~pf:Nothing.unreachable_code
  ;;

  (* TODO: handle trait bounds *)
  let instantiate_bounded ?map_name ?params typ =
    match typ with
    | [], typ -> instantiate ?map_name ?params typ
    | _ -> raise_s [%message "Trait bounds not yet implemented"]
  ;;
end

module Concrete = struct
  module T = struct
    type t = (Nothing.t, Nothing.t) Expr.t [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let cast t = Expr.map t ~var:Nothing.unreachable_code ~pf:Nothing.unreachable_code

  let of_polymorphic_exn t =
    let fail _ = compiler_bug [%message "Type.Concrete.of_polymorphic_exn: found var"] in
    Expr.map t ~var:fail ~pf:fail
  ;;
end

module Decl = struct
  type decl =
    | Abstract
    | Alias of Scheme.t
    (* TODO: variant constructors should probably support fixity declarations *)
    | Variants of (Cnstr_name.t * Scheme.t list) list
    | Record of (Value_name.t * Scheme.t) Nonempty.t
  [@@deriving compare, equal, hash, sexp]

  type t = Type_param_name.t list * decl [@@deriving compare, equal, hash, sexp]

  let arity (params, _) = List.length params

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
    let check_params params typ =
      Expr.for_all_vars typ ~f:(List.mem params ~equal:Type_param_name.equal)
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
end
