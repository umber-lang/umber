open Import
open Names
module Var_id = Unique_id.Int ()

module Param = struct
  (* TODO: need variance for type parameters (e.g. covariant, contravariant)
     Can probably wait a bit though -- it's needed for subtyping
     Variance can maybe be added as constraints on the type scheme *)
  module T = struct
    type t = Type_param_name.t [@@deriving compare, equal, hash, sexp]
  end

  include T

  module Env_to_vars : sig
    type param = t
    type t

    val create : unit -> t
    val find_or_add : t -> param -> Var_id.t
  end = struct
    type param = t
    type nonrec t = (t, Var_id.t) Hashtbl.t

    let create () = Hashtbl.create (module T)
    let find_or_add = Hashtbl.find_or_add ~default:Var_id.create
  end

  module Env_of_vars : sig
    type param = t
    type t

    val create : unit -> t
    val find_or_add : t -> Var_id.t -> param
  end = struct
    type param = t

    type nonrec t =
      { table : (Var_id.t, t) Hashtbl.t
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
  type 'var t =
    | Var of 'var
    | Type_app of Type_name.Qualified.t * 'var t list
    | Function of 'var t * 'var t
    | Tuple of 'var t list
  [@@deriving compare, equal, hash, sexp, variants]

  let rec map typ ~f ~var =
    match f typ with
    | `Halt typ -> typ
    | `Retry typ -> map typ ~f ~var
    | `Defer typ ->
      (match typ with
      | Var v -> Var (var v)
      | Type_app (name, fields) -> Type_app (name, List.map fields ~f:(map ~f ~var))
      | Tuple fields -> Tuple (List.map fields ~f:(map ~f ~var))
      | Function (arg, body) ->
        (* Ensure functions are evaluated left-to-right for consistency*)
        let arg = map ~f ~var arg in
        Function (arg, map ~f ~var body))
  ;;

  let map_vars typ ~f = map typ ~f:(fun typ -> `Defer typ) ~var:f

  (* TODO: make a functor or something to be able to used the more general map *)
  let map = map ~var:Fn.id

  let rec fold_vars typ ~init ~f =
    match typ with
    | Var var -> f init var
    | Type_app (_, fields) | Tuple fields ->
      List.fold fields ~init ~f:(fun init -> fold_vars ~init ~f)
    | Function (arg, body) -> fold_vars body ~f ~init:(fold_vars arg ~init ~f)
  ;;

  let rec for_all_vars typ ~f =
    match typ with
    | Var var -> f var
    | Type_app (_, fields) | Tuple fields -> List.for_all fields ~f:(for_all_vars ~f)
    | Function (arg, body) -> for_all_vars arg ~f && for_all_vars body ~f
  ;;

  module Bounded = struct
    type nonrec t = Trait_bound.t * Param.t t [@@deriving compare, equal, hash, sexp]
  end
end

module Decl = struct
  type decl =
    | Abstract
    | Alias of Param.t Expr.t
    (* TODO: variant constructors should probably support fixity declarations *)
    | Variants of (Cnstr_name.t * Param.t Expr.t list) list
    | Record of (Value_name.t * Param.t Expr.t) list
  [@@deriving compare, equal, hash, sexp]

  type t = Param.t list * decl [@@deriving compare, equal, hash, sexp]

  let arity (params, _) = List.length params

  let map_exprs (params, decl) ~f =
    ( params
    , match decl with
      | Abstract -> decl
      | Alias expr -> Alias (f expr)
      | Variants cnstrs -> Variants (List.map cnstrs ~f:(Tuple2.map_snd ~f:(List.map ~f)))
      | Record fields -> Record (List.map fields ~f:(Tuple2.map_snd ~f)) )
  ;;

  let fold_exprs (_, decl) ~init:acc ~f =
    match decl with
    | Abstract -> acc
    | Alias expr -> f acc expr
    | Variants cnstrs ->
      List.fold cnstrs ~init:acc ~f:(fun acc -> snd >> List.fold ~init:acc ~f)
    | Record fields -> List.fold fields ~init:acc ~f:(fun acc -> snd >> f acc)
  ;;

  let iter_exprs decl ~f = fold_exprs decl ~init:() ~f:(fun () -> f)

  let no_free_params =
    let check_params params typ =
      Expr.for_all_vars typ ~f:(List.mem params ~equal:Param.equal)
    in
    fun (params, decl) ->
      match decl with
      | Alias expr -> check_params params expr
      | Abstract -> true
      | Variants cnstrs ->
        List.for_all cnstrs ~f:(fun (_, args) ->
          List.for_all args ~f:(check_params params))
      | Record fields ->
        List.for_all fields ~f:(fun (_, field) -> check_params params field)
  ;;
end

type t = Var_id.t Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  (* TODO: add trait constraints to this type here
     Having this type not be the same as the type that plain type expressions get parsed
     into also seems highly desirable *)
  type nonrec t = Param.t Expr.t [@@deriving compare, hash, equal, sexp]

  let instantiate ?(map_name = Fn.id) ?params typ =
    let params = option_or_default params ~f:Param.Env_to_vars.create in
    Expr.map_vars typ ~f:(Param.Env_to_vars.find_or_add params)
    |> Expr.map ~f:(function
         | Type_app (name, args) -> `Defer (Expr.Type_app (map_name name, args))
         | typ -> `Defer typ)
  ;;

  (* TODO: handle trait bounds *)
  let instantiate_bounded ?map_name ?params typ =
    match typ with
    | [], typ -> instantiate ?map_name ?params typ
    | _ -> raise_s [%message "Trait bounds not yet implemented"]
  ;;
end

module Concrete = struct
  type t = Nothing.t Expr.t [@@deriving compare, equal, hash, sexp]

  let cast = Expr.map_vars ~f:(fun _ -> assert false)
end
