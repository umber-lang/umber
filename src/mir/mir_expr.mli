open! Core
open! Import

type t =
  | Primitive of Literal.t
  | Name of Mir_name.t
  | Let of Mir_name.t * t * t
  | Fun_call of Mir_name.t * t Nonempty.t
  | Make_block of
      { tag : Cnstr_tag.t
      ; fields : t list
      }
  | Get_block_field of Block_index.t * t
  | Cond_assign of
      { vars : Mir_name.t list
      ; conds : (cond * t list) Nonempty.t
      ; body : t
      ; if_none_matched : cond_if_none_matched
      }
  | Handle_effects of
      (* FIXME: I think vars might just have to be the vars used by the handled expr,
           not including the branches. Make that clear. *)
      
      { vars : (Mir_name.t * Mir_name.t) list
      ; value_handler : (Mir_name.t * t) option
      ; effect_handlers : effect_handler Nonempty.t
      ; expr : t
      }
  | Perform_effect of
      { effect_op : Effect_op_id.t
      ; args : t Nonempty.t
      }

and effect_handler =
  { effect_op : Effect_op_id.t
  ; args : Mir_name.t Nonempty.t
  ; resume : Mir_name.t
  ; handler : t
  }

and cond =
  | Equals of t * Literal.t
  | Constant_tag_equals of t * Cnstr_tag.t
  | Non_constant_tag_equals of t * Cnstr_tag.t
  | And of cond * cond

and cond_if_none_matched =
  | Otherwise of t
  | Use_bindings of t list
[@@deriving sexp_of]

module Fun_def : sig
  type nonrec t =
    { fun_name : Mir_name.t
    ; args : Mir_name.t Nonempty.t
    ; body : t
    }
  [@@deriving sexp_of]
end

module Just_bound : sig
  type t =
    | Rec of
        { this_name : Mir_name.t
        ; other_names : Mir_name.Set.t
        }
    | Nonrec of { this_pattern_names : Mir_name.Set.t }
  [@@deriving sexp_of]
end

val of_typed_expr
  :  just_bound:Just_bound.t
  -> ctx:Context.t
  -> add_fun_def:(Fun_def.t -> unit)
  -> add_fun_decl:(Fun_decl.t -> unit)
  -> Module_path.absolute Type_scheme.t Typed_ast.Expr.t
  -> Module_path.absolute Type_scheme.type_
  -> t

(* TODO: This function seems way too complicated. Any way to simplify it or at least make
   it clearer? *)
val generate_let_bindings
  :  ctx:Context.t
  -> ctx_for_body:Context.t
  -> rec_:bool
  -> init:'acc
  -> add_let:('acc -> Mir_name.t -> t -> Module_path.absolute Type_scheme.type_ -> 'acc)
  -> extract_binding:
       ('binding
        -> Typed_ast.Pattern.generalized Node.t
           * 'c Typed_ast.Expr.t Node.t
           * Module_path.absolute Type_scheme.t)
  -> process_expr:
       ('acc
        -> just_bound:Just_bound.t
        -> ctx:Context.t
        -> 'c Typed_ast.Expr.t Node.t
        -> Module_path.absolute Type_scheme.type_
        -> 'acc * t Node.t)
  -> ('binding * Mir_name.Set.t) Nonempty.t
  -> Context.t * 'acc
