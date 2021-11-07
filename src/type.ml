open Import
open Names
module Var_id = Unique_id.Int ()

module Param = struct
  (* TODO: need variance for type parameters (e.g. covariant, contravariant)
     Can probably wait a bit though -- it's needed for subtyping
     Variance can maybe be added as constraints on the type scheme *)

  module T = struct
    type t = Var_id.t * Type_param_name.t

    let compare (id, _) (id', _) = Var_id.compare id id'
    let hash_fold_t state (var, _) = Var_id.hash_fold_t state var
    let hash (id, _) = Var_id.hash id
    let sexp_of_t (_, name) = [%sexp (name : Type_param_name.t)]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  module Map = struct
    include Map
    include Map.Provide_hash (T)
  end

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

  let create = Tuple2.create
  let id = fst
  let name = snd
  let equal_names (_, name) (_, name') = Type_param_name.(name = name')
  let dummy = Var_id.create (), Type_param_name.default
  let of_name ~env name = Env_to_vars.find_or_add env name, name
  let of_var ~env var = var, Env_of_vars.find_or_add env var
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
    let open Fold_action.Let_syntax in
    match (f init typ : _ Fold_action.t) with
    | Stop _ as stop -> stop
    | Continue init as continue ->
      (match typ with
      | Var _ -> continue
      | Type_app (_, fields) | Tuple fields ->
        List.fold_until fields ~init ~f:(fun init -> fold_until ~init ~f)
      | Function (args, body) ->
        let%bind init =
          Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f)
        in
        fold_until body ~init ~f
      | Partial_function (args, _) ->
        Nonempty.fold_until args ~init ~f:(fun init -> fold_until ~init ~f))
  ;;

  let for_all_vars typ ~f =
    fold_until typ ~init:true ~f:(fun _ -> function
      | Var var -> if f var then Continue true else Stop false
      | _ -> Continue true)
    |> Fold_action.finish ~f:Fn.id
  ;;

  let exists_var typ ~f =
    fold_until typ ~init:false ~f:(fun _ -> function
      | Var var -> if f var then Stop true else Continue false
      | _ -> Continue false)
    |> Fold_action.finish ~f:Fn.id
  ;;
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module type Boundable = sig
  type nonrec t [@@deriving compare, hash, equal, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  module Bounded : sig
    type nonrec t = Trait_bound.t * t [@@deriving compare, equal, hash, sexp]
  end

  val instantiate
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> t
    -> (Var_id.t, _) Expr.t

  val instantiate_bounded
    :  ?map_name:(Type_name.Qualified.t -> Type_name.Qualified.t)
    -> ?params:Param.Env_to_vars.t
    -> Bounded.t
    -> (Var_id.t, _) Expr.t
end

module Make_boundable (T : sig
  type var
  type t = (var, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp]

  val param_name_of_var : var -> Type_param_name.t
end) : Boundable with type t = T.t = struct
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
      ~var:(Param.Env_to_vars.find_or_add params << param_name_of_var)
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

module Scheme_plain = Make_boundable (struct
  type var = Type_param_name.t
  type t = (Type_param_name.t, Nothing.t) Expr.t [@@deriving compare, equal, hash, sexp]

  let param_name_of_var = Fn.id
end)

module Scheme = struct
  include Make_boundable (struct
    type var = Param.t

    (* TODO: add trait constraints to this type here *)
    type nonrec t = (Param.t, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp_of]

    let param_name_of_var = Param.name

    let t_of_sexp sexp =
      let env = Param.Env_to_vars.create () in
      let param_of_sexp sexp =
        let name = [%of_sexp: Type_param_name.t] sexp in
        let id = Param.Env_to_vars.find_or_add env name in
        id, name
      in
      Expr.t_of_sexp param_of_sexp [%of_sexp: Nothing.t] sexp
    ;;
  end)

  let of_plain ?(env = Param.Env_to_vars.create ()) plain =
    Expr.map plain ~var:(Param.of_name ~env) ~pf:Nothing.unreachable_code
  ;;

  let infer_param_map =
    let add_consistent param_map ~key:param ~data:new_ =
      Param.Map.update param_map param ~f:(function
        | None -> new_
        | Some existing ->
          if equal existing new_
          then existing
          else
            compiler_bug
              [%message "Inconsistent type instantiation" (existing : t) (new_ : t)])
    in
    let rec loop param_map t t' =
      match (t : t), (t' : t) with
      | Var param, _ -> add_consistent param_map ~key:param ~data:t'
      | Type_app (type_name, args), Type_app (type_name', args') ->
        assert_or_compiler_bug
          (Type_name.Qualified.equal type_name type_name')
          ~here:[%here];
        List.fold2_exn args args' ~init:param_map ~f:loop
      | Tuple fields, Tuple fields' ->
        List.fold2_exn fields fields' ~init:param_map ~f:loop
      | Function (args, body), Function (args', body') ->
        let param_map = Nonempty.fold2_exn args args' ~init:param_map ~f:loop in
        loop param_map body body'
      | (Type_app _ | Tuple _ | Function _), _ ->
        compiler_bug
          [%message
            "infer_param_map: incompatible template and instance types"
              ~template:(t : t)
              ~instance:(t' : t)]
      | Partial_function _, _ -> .
    in
    fun ~template_type:t ~instance_type:t' -> loop Param.Map.empty t t'
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
    | Alias of Scheme_plain.t
    (* TODO: variant constructors should probably support fixity declarations *)
    | Variants of (Cnstr_name.t * Scheme_plain.t list) list
    | Record of (Value_name.t * Scheme_plain.t) Nonempty.t
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
