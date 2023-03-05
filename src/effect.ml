open! Import
open Names

module Operation = struct
  type t =
    { name : Value_name.t
    ; args : Type.Scheme.t Nonempty.t
    ; result : Type.Scheme.t
    }
  [@@deriving sexp]

  let create sig_ =
    match (sig_ : Module.sig_) with
    | Common_sig (Val (_, Some _, _)) ->
      Compilation_error.raise
        Other
        ~msg:[%message "Fixity declarations are not supported on effect operations"]
    | Common_sig (Val (name, None, type_)) ->
      (match type_ with
       | [], Function (args, result) ->
         (* FIXME: Check for free params. *)
         { name; args; result }
       | _ :: _, _ -> failwith "TODO: trait bounds on effect operations"
       | [], (Var _ | Type_app _ | Tuple _) ->
         Compilation_error.raise
           Other
           ~msg:
             [%message
               "Effect operations must be functions" (type_ : Type.Scheme.Bounded.t)]
       | [], Partial_function _ -> .)
    | Module_sig _
    | Common_sig
        ( Extern _
        | Type_decl _
        | Effect _
        | Trait_sig _
        | Import _
        | Import_with _
        | Import_without _ ) ->
      Compilation_error.raise
        Other
        ~msg:
          [%message
            [%string "This kind of statement is not allowed inside an effect declaration"]
              (sig_ : Module.sig_)]
  ;;

  let map_exprs { name; args; result } ~f =
    { name; args = Nonempty.map args ~f; result = f result }
  ;;
end

type t =
  { params : Type_param_name.t list
  ; operations : Operation.t list option
  }
[@@deriving sexp]

let create params sigs =
  let operations =
    Option.map sigs ~f:(List.map ~f:(Node.with_value ~f:Operation.create))
  in
  { params; operations }
;;

let map_exprs { params; operations } ~f =
  { params; operations = Option.map operations ~f:(List.map ~f:(Operation.map_exprs ~f)) }
;;
