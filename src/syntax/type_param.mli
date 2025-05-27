open! Core
open! Import
open Names

(* TODO: consider adding support for weak type variables here *)
type t = Type_param_name.t [@@deriving compare, equal, sexp]

val dummy : t

include Comparable.S_plain with type t := t

module Map : sig
  include module type of Map

  val hash_fold_t : (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state
end

include Hashable.S_plain with type t := t

module Generator : sig
  type type_param := t
  type t

  val create : unit -> t
  val next : t -> type_param
end

module Env_to_vars : sig
  type t

  val create : unit -> t
  val find_or_add : t -> Type_param_name.t -> Type_var.t
end

module Env_of_vars : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val find_or_add : t -> Type_var.t -> Type_param_name.t
  val find : t -> Type_var.t -> Type_param_name.t option
  val mem : t -> Type_var.t -> bool
end
