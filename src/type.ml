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
  include Comparable.Make (T)

  module Map = struct
    include Map
    include Map.Provide_hash (T)
  end

  include Hashable.Make (T)

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

  let rec fold_vars typ ~init ~f =
    match typ with
    | Var var -> f init var
    | Type_app (_, fields) | Tuple fields ->
      List.fold fields ~init ~f:(fun init -> fold_vars ~init ~f)
    | Function (args, body) ->
      let init = Nonempty.fold args ~init ~f:(fun init -> fold_vars ~init ~f) in
      fold_vars body ~f ~init
    | Partial_function (args, _) ->
      Nonempty.fold args ~init ~f:(fun init -> fold_vars ~init ~f)
  ;;

  let rec for_all_vars typ ~f =
    match typ with
    | Var var -> f var
    | Type_app (_, fields) | Tuple fields -> List.for_all fields ~f:(for_all_vars ~f)
    | Function (args, body) ->
      Nonempty.for_all args ~f:(for_all_vars ~f) && for_all_vars body ~f
    | Partial_function (args, _) -> Nonempty.for_all args ~f:(for_all_vars ~f)
  ;;

  module Bounded = struct
    type nonrec t = Trait_bound.t * (Param.t, Nothing.t) t
    [@@deriving compare, equal, hash, sexp]
  end
end

type t = (Var_id.t, Var_id.t) Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  module T = struct
    (* TODO: add trait constraints to this type here
       Having this type not be the same as the type that plain type expressions get parsed
       into also seems highly desirable. (Actually, it's probably just fine. They should
       ultimately be the same type (supposing an isomorphism between types expressable in
       the syntax and types expressable by the compiler), with the only possible
       difference being a phantom variable signifying whether the type has been checked). *)
    type nonrec t = (Param.t, Nothing.t) Expr.t [@@deriving compare, hash, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

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
    fun ~template_type ~instance_type -> loop Param.Map.empty template_type instance_type
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

  type t = Param.t list * decl [@@deriving compare, equal, hash, sexp]

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
        Nonempty.for_all fields ~f:(fun (_, field) -> check_params params field)
  ;;
end
