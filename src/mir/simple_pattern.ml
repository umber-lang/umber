open! Core
open! Import

type t =
  | Constant of Literal.t
  | Catch_all of Value_name.t option
  | As of t * Value_name.t
  | Cnstr_appl of Cnstr.t * (t * Module_path.absolute Type_scheme.type_) list
[@@deriving sexp, variants]

let names =
  let rec loop acc t =
    match t with
    | Constant _ | Catch_all None -> acc
    | Catch_all (Some name) -> Set.add acc name
    | As (t, name) -> loop (Set.add acc name) t
    | Cnstr_appl (_, args) ->
      List.fold args ~init:acc ~f:(fun acc (arg, _type) -> loop acc arg)
  in
  fun t -> loop Value_name.Set.empty t
;;

let flatten_typed_pattern pattern =
  let rec loop : Typed_ast.Pattern.generalized -> t Nonempty.t = function
    | Constant lit -> [ Constant lit ]
    | Catch_all name -> [ Catch_all name ]
    | As (pattern, name) -> Nonempty.map (loop pattern) ~f:(Fn.flip as_ name)
    | Cnstr_appl ((_, cnstr_name), args) -> of_cnstr_appl (Cnstr.Named cnstr_name) args
    | Tuple fields -> of_cnstr_appl Tuple fields
    | Record _fields ->
      (*let field_names, field_patterns =
          List.fold
            fields
            ~init:(Value_name.Set.empty, [])
            ~f:(fun (field_names, field_patterns) (name, pattern) ->
            let pattern =
              Option.value pattern ~default:(Catch_all (Some name))
            in
            Set.add field_names name, pattern :: field_patterns)
        in
        of_cnstr_appl (record_cnstr_name field_names) (List.rev field_patterns)*)
      failwith "TODO: flatten_typed_pattern: record patterns"
    | Union (pat1, pat2) -> Nonempty.(loop pat1 @ loop pat2)
  and of_cnstr_appl cnstr args =
    match Nonempty.of_list args with
    | None -> [ Cnstr_appl (cnstr, []) ]
    | Some args ->
      Nonempty.map args ~f:(fun (arg, (arg_type, _constraints)) ->
        Nonempty.map (loop arg) ~f:(fun arg -> arg, arg_type))
      |> Nonempty.cartesian_product_all
      |> Nonempty.map ~f:(fun args -> Cnstr_appl (cnstr, Nonempty.to_list args))
  in
  loop pattern
;;

let flatten_typed_pattern_no_unions pattern ~label =
  match flatten_typed_pattern pattern with
  | [ arg ] -> arg
  | _ :: _ :: _ ->
    let msg = [%string "Pattern unions in %{label} are not supported"] in
    Compilation_error.raise
      Mir_error
      ~msg:[%message msg (pattern : Typed_ast.Pattern.generalized)]
;;

module Coverage = struct
  type simple_pattern = t

  type t =
    (* TODO: We use `largest_seen` to keep track of an example of a literal value not
         included in a union of literal patterns. But this won't work if you match on the
         max value of an int and also match on the lowest value, since we will report
         `Int.max_value + 1 == Int.min_value` as the not-covered value, even though it was
         covered. There are similar issues for chars/floats. For ints/floats/chars, we
         can probably do something like keep track of the covered ranges. *)
    | Inexhaustive : { largest_seen : Literal.t } -> t
    | Exhaustive
    | By_cnstr : (t * Module_path.absolute Type_scheme.type_) list Cnstr.Map.t -> t

  let rec of_pattern : simple_pattern -> t = function
    | Constant lit -> Inexhaustive { largest_seen = lit }
    | Catch_all _ -> Exhaustive
    | As (pat, _) -> of_pattern pat
    | Cnstr_appl (cnstr, args) ->
      By_cnstr
        (Cnstr.Map.singleton
           cnstr
           (List.map args ~f:(fun (arg, arg_type) -> of_pattern arg, arg_type)))
  ;;

  let rec combine coverage coverage' =
    match coverage, coverage' with
    | Exhaustive, _ | _, Exhaustive -> Exhaustive
    | Inexhaustive { largest_seen = x }, Inexhaustive { largest_seen = y } ->
      Inexhaustive
        { largest_seen =
            (match x, y with
             | Int i, Int i' -> Int (max i i')
             | Float x, Float x' -> Float (Float.max x x')
             | Char c, Char c' -> Char (Uchar.max c c')
             | String s, String s' -> String (Ustring.max s s')
             | _ ->
               compiler_bug
                 [%message
                   "Incompatible literals in pattern" (x : Literal.t) (y : Literal.t)])
        }
    | Inexhaustive _, (By_cnstr _ as by_cnstr) | (By_cnstr _ as by_cnstr), Inexhaustive _
      -> by_cnstr
    | By_cnstr coverage_by_cnstr, By_cnstr coverage_by_cnstr' ->
      merge_by_cnstr coverage_by_cnstr coverage_by_cnstr'

  and merge_by_cnstr coverage_by_cnstr coverage_by_cnstr' =
    By_cnstr
      (Map.merge_skewed coverage_by_cnstr coverage_by_cnstr' ~combine:(fun ~key:_ ->
         List.map2_exn ~f:(fun (coverage1, type_) (coverage2, _) ->
           combine coverage1 coverage2, type_)))
  ;;

  let add_pattern coverage pattern =
    match coverage with
    | Exhaustive -> Exhaustive
    | Inexhaustive _ -> of_pattern pattern
    | By_cnstr coverage_by_cnstr ->
      (match of_pattern pattern with
       | Exhaustive -> Exhaustive
       | Inexhaustive _ -> coverage
       | By_cnstr coverage_by_cnstr' ->
         merge_by_cnstr coverage_by_cnstr coverage_by_cnstr')
  ;;

  let of_patterns Nonempty.(pattern :: patterns) =
    List.fold_until patterns ~init:(of_pattern pattern) ~f:(fun coverage pattern ->
      match add_pattern coverage pattern with
      | Exhaustive -> Stop Exhaustive
      | coverage -> Continue coverage)
    |> Fold_action.id
  ;;

  let asterisk = Ustring.of_string_exn "*"

  let rec missing_cases coverage ~ctx ~input_type =
    match coverage with
    | Exhaustive -> []
    | Inexhaustive { largest_seen } ->
      [ Constant
          (match largest_seen with
           | Int i -> Int (i + 1)
           | Float x -> Float (x +. 1.)
           | Char c -> Char (Option.value (Uchar.succ c) ~default:Uchar.min_value)
           | String s -> String (Ustring.( ^ ) asterisk s))
      ]
    | By_cnstr coverage_by_cnstr ->
      let cnstr_info = Context.find_cnstr_info ctx input_type in
      let all_cnstrs = Cnstr_info.cnstrs cnstr_info |> Cnstr.Set.of_list in
      let arity = Set.length all_cnstrs in
      let any_arg_pattern = Catch_all None, Type_scheme.Var Type_param_name.default in
      let missing_cnstrs =
        Set.diff all_cnstrs (Map.key_set coverage_by_cnstr)
        |> Set.to_list
        |> List.map ~f:(fun cnstr ->
             Cnstr_appl (cnstr, List.init arity ~f:(const any_arg_pattern)))
      in
      let missing_in_args =
        Map.to_alist coverage_by_cnstr
        |> List.concat_map ~f:(fun (cnstr, args) ->
             match Nonempty.of_list args with
             | None -> []
             | Some args ->
               let missing_cases_per_arg =
                 Nonempty.map args ~f:(fun (arg, input_type) ->
                   missing_cases ~ctx ~input_type arg
                   |> List.map ~f:(fun case -> case, input_type))
               in
               if Nonempty.for_all ~f:List.is_empty missing_cases_per_arg
               then []
               else
                 Nonempty.map missing_cases_per_arg ~f:(function
                   | x :: xs -> Nonempty.(x :: xs)
                   | [] -> [ any_arg_pattern ])
                 |> Nonempty.cartesian_product_all
                 |> Nonempty.map ~f:(fun args ->
                      Cnstr_appl (cnstr, Nonempty.to_list args))
                 |> Nonempty.to_list)
      in
      missing_cnstrs @ missing_in_args
  ;;
end
