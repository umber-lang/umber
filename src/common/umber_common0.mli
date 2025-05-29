open! Core

val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

module Map_action : sig
  type ('a, 'b) t =
    | Defer of 'a
    | Halt of 'b
    | Retry of 'a
  [@@deriving variants]
end

module Fold_action : sig
  type ('acc, 'final) t =
    | Continue of 'acc
    | Stop of 'final
  [@@deriving variants]

  include Monad.S2 with type ('a, 'b) t := ('a, 'b) t

  val finish : ('a, 'b) t -> f:('b -> 'a) -> 'a
  val id : ('a, 'a) t -> 'a
  val result : ('a, 'b) t -> ('a, 'b) result
end

module Option : sig
  include module type of Option

  val fold_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t
end

module List : sig
  include module type of List

  val iter_pairs : 'a t -> f:('a -> 'a -> unit) -> unit
  val split_last : 'a t -> ('a t * 'a) option

  val fold_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t

  val fold_map_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc * 'b, 'final) Fold_action.t)
    -> ('acc * 'b t, 'final * 'b t) Fold_action.t
end

module Map : sig
  include module type of Map

  val fold_until
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> ('acc, 'final) Fold_action.t)
    -> ('acc, 'final) Fold_action.t
end

exception Compiler_bug of Sexp.t

val compiler_bug : Sexp.t -> 'a
val assert_or_compiler_bug : here:Source_code_position.t -> bool -> unit
val ok_or_compiler_bug : here:Source_code_position.t -> 'a Or_error.t -> 'a
val never_happens : Source_code_position.t -> 'a -> Core.never_returns
val eprint_s : Source_code_position.t -> Sexp.t lazy_t -> unit
