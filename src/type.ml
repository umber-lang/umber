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
  type 'var t =
    | Var of 'var
    | Type_app of Type_name.Qualified.t * 'var t list
    | Function of 'var t Nonempty.t * 'var t
    | Tuple of 'var t list
  [@@deriving compare, equal, hash, sexp, variants]

  let rec map' typ ~f ~var =
    match f typ with
    | `Halt typ -> typ
    | `Retry typ -> map' typ ~f ~var
    | `Defer typ ->
      (match typ with
      | Var v -> Var (var v)
      | Type_app (name, fields) -> Type_app (name, List.map fields ~f:(map' ~f ~var))
      | Tuple fields -> Tuple (List.map fields ~f:(map' ~f ~var))
      | Function (args, body) ->
        let args = Nonempty.map args ~f:(map' ~f ~var) in
        Function (args, map' ~f ~var body))
  ;;

  let map = map' ~var:Fn.id
  let map_vars typ ~f = map' typ ~f:(fun typ -> `Defer typ) ~var:f

  let rec fold_vars typ ~init ~f =
    match typ with
    | Var var -> f init var
    | Type_app (_, fields) | Tuple fields ->
      List.fold fields ~init ~f:(fun init -> fold_vars ~init ~f)
    | Function (args, body) ->
      let init = Nonempty.fold args ~init ~f:(fun init -> fold_vars ~init ~f) in
      fold_vars body ~f ~init
  ;;

  let rec for_all_vars typ ~f =
    match typ with
    | Var var -> f var
    | Type_app (_, fields) | Tuple fields -> List.for_all fields ~f:(for_all_vars ~f)
    | Function (args, body) ->
      Nonempty.for_all args ~f:(for_all_vars ~f) && for_all_vars body ~f
  ;;

  module Bounded = struct
    type nonrec t = Trait_bound.t * Param.t t [@@deriving compare, equal, hash, sexp]
  end
end

type t = Var_id.t Expr.t [@@deriving compare, hash, equal, sexp]

let fresh_var () = Expr.Var (Var_id.create ())

module Scheme = struct
  module T = struct
    (* TODO: add trait constraints to this type here
     Having this type not be the same as the type that plain type expressions get parsed
     into also seems highly desirable *)
    type nonrec t = Param.t Expr.t [@@deriving compare, hash, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

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
      | Function (args, body), Function (args', body') ->
        let param_map = Nonempty.fold2_exn args args' ~init:param_map ~f:loop in
        loop param_map body body'
      | Tuple fields, Tuple fields' ->
        List.fold2_exn fields fields' ~init:param_map ~f:loop
      | Type_app _, (Var _ | Function _ | Tuple _)
      | Function _, (Var _ | Type_app _ | Tuple _)
      | Tuple _, (Var _ | Type_app _ | Function _) ->
        compiler_bug
          [%message
            "infer_param_map: incompatible template and instance types"
              ~template:(t : t)
              ~instance:(t : t)]
    in
    fun ~template_type ~instance_type -> loop Param.Map.empty template_type instance_type
  ;;
end

module Concrete = struct
  module T = struct
    type t = Nothing.t Expr.t [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let cast = Expr.map_vars ~f:(function (_ : Nothing.t) -> .)
end

module Decl = struct
  type 'var decl =
    | Abstract
    | Alias of 'var Expr.t
    (* TODO: variant constructors should probably support fixity declarations *)
    | Variants of (Cnstr_name.t * 'var Expr.t list) list
    | Record of (Value_name.t * 'var Expr.t) list
  [@@deriving compare, equal, hash, sexp]

  type t = Param.t list * Param.t decl [@@deriving compare, equal, hash, sexp]

  let arity (params, _) = List.length params

  let map_exprs (params, decl) ~f =
    ( params
    , match decl with
      | Abstract -> Abstract
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

  module Monomorphic = struct
    type t = Nothing.t decl [@@deriving compare, equal, hash, sexp]
  end

  let monomorphize t ~(args : Concrete.t list) =
    let param_subs = List.zip_exn (fst t) args |> Param.Map.of_alist_exn in
    map_exprs
      t
      ~f:
        (Expr.map'
           ~f:(function
             | Var param -> `Halt (Map.find_exn param_subs param)
             | (Type_app _ | Function _ | Tuple _) as expr -> `Defer expr)
           ~var:(fun _ -> compiler_bug [%message "Type.Decl.monomorphize: var"]))
    |> snd
  ;;
end
