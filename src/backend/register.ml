open! Core
open! Import

module Real = struct
  module T = struct
    (* TODO: Floating point registers xmm0 to xmm7 *)
    type t =
      | Rax
      | Rcx
      | Rdx
      | Rbx
      | Rsp
      | Rbp
      | Rsi
      | Rdi
      | R8
      | R9
      | R10
      | R11
      | R12
      | R13
      | R14
      | R15
    [@@deriving equal, compare, hash, sexp, variants, enumerate]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let pp fmt t = Format.pp_print_string fmt (String.lowercase (Variants.to_name t))
end

module Virtual = Unique_counter.Make ()

module T = struct
  type t =
    | Real of Real.t
    | Virtual of Virtual.t
  [@@deriving compare, equal, hash, sexp_of]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
