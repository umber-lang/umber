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
  type ('v, 'pf, 'n) t =
    | Var of 'v
    | Type_app of 'n Type_name.Qualified.t * ('v, 'pf, 'n) t list
    | Tuple of ('v, 'pf, 'n) t list
    | Function of ('v, 'pf, 'n) t Nonempty.t * ('v, 'pf, 'n) t
    | Partial_function of ('v, 'pf, 'n) t Nonempty.t * 'pf
  [@@deriving compare, equal, hash, sexp, variants]

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
       | Function (args, body) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
         Function (args, map ~f ~var ~pf ~name body)
       | Partial_function (args, v) ->
         let args = Nonempty.map args ~f:(map ~f ~var ~pf ~name) in
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

type t = (Var_id.t, Var_id.t, Module_path.absolute) Expr.t
[@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  type nonrec 'n t = (Param.t, Nothing.t, 'n) Expr.t
  [@@deriving compare, hash, equal, sexp]

  module Bounded = struct
    type nonrec 'n t = Trait_bound.t * 'n t [@@deriving compare, equal, hash, sexp]
  end

  let instantiate ?params typ =
    let params = option_or_default params ~f:Param.Env_to_vars.create in
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

  let of_polymorphic_exn t =
    let fail _ = compiler_bug [%message "Type.Concrete.of_polymorphic_exn: found var"] in
    Expr.map t ~var:fail ~pf:fail ~name:Fn.id
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

  type 'n t = Type_param_name.t list * 'n decl [@@deriving compare, equal, hash, sexp]

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
