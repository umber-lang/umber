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
    | Function of ('v, 'pf) t Nonempty.t * ('v, 'pf) t
    | Partial_function of ('v, 'pf) t Nonempty.t * 'pf
  [@@deriving compare, equal, hash, sexp, variants]

  let rec map ?(f = Map_action.defer) typ ~var ~pf =
    match f typ with
    | Halt typ -> typ
    | Retry typ -> map typ ~f ~var ~pf
    | Defer typ ->
      (match typ with
      | Var v -> Var (var v)
      | Type_app (name, fields) -> Type_app (name, List.map fields ~f:(map ~f ~var ~pf))
      | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var ~pf))
      | Function (args, body) ->
        let args = Nonempty.map args ~f:(map ~f ~var ~pf) in
        Function (args, map ~f ~var ~pf body)
      | Partial_function (args, v) ->
        let args = Nonempty.map args ~f:(map ~f ~var ~pf) in
        Partial_function (args, pf v))
  ;;

  let rec fold_until typ ~init ~f =
    match (f init typ : _ Fold_action.t) with
    | Stop _ as stop -> stop
    | Continue init as continue ->
      (match typ with
      | Var _ -> continue
      | Type_app (_, fields) | Tuple fields ->
        List.fold_until fields ~init ~f:(fun init -> fold_until ~init ~f)
      | Function (args, body) ->
        let%bind.Fold_action init =
          Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
        in
        fold_until body ~init ~f
      | Partial_function (args, _) ->
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
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  module T = struct
    (* TODO: add trait constraints to this type here *)
    type nonrec t = (Param.t, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp]
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
      ~f:(function
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
