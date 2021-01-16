open Import

module type General_name = sig
  type t = private Ustring.t [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val to_ustring : t -> Ustring.t
end

module type Unidentified_name = sig
  include General_name

  val of_ustring : Ustring.t -> t
  val of_string_exn : string -> t
end

module Unidentified_name : Unidentified_name = Ustring
module Extern_name : Unidentified_name = Ustring

module type Name = sig
  include General_name

  val unidentify : t -> Unidentified_name.t
  val of_ustring_unchecked : Ustring.t -> t
  val of_ustring_exn : Ustring.t -> t
  val of_string_unchecked : string -> t
  val of_string_exn : string -> t
  val of_string_lenient_exn : string -> t
end

module type Name_validator = sig
  val valid : string -> bool
  val coerce_exn : string -> string
  val error_msg : string
end

module Identified_ustring (V : Name_validator) : Name = struct
  include Ustring

  let unidentify = to_ustring >> Unidentified_name.of_ustring
  let of_ustring_unchecked = of_ustring

  let of_ustring_exn ustr =
    let str = Ustring.to_string ustr in
    if V.valid str then of_ustring ustr else raise_s [%message V.error_msg str]
  ;;

  let of_string_unchecked = of_string_exn

  let of_string_exn str =
    if V.valid str then of_string_unchecked str else raise_s [%message V.error_msg str]
  ;;

  let of_string_lenient_exn = of_string_unchecked << V.coerce_exn
end

module Name_lexing = struct
  let digit = [%sedlex.regexp? '0' .. '9']

  let valid_lower_name str =
    (* Should have identical semantics to those of the lexer *)
    let lexbuf = Sedlexing.Utf8.from_string str in
    match%sedlex lexbuf with
    | lowercase, Star (digit | alphabetic | '\'' | '_'), eof -> true
    | '_', Plus (digit | alphabetic | '\'' | '_'), eof -> true
    | _ -> false
  ;;

  let valid_upper_name str =
    (* Should have identical semantics to those of the lexer *)
    let lexbuf = Sedlexing.Utf8.from_string str in
    match%sedlex lexbuf with
    | uppercase, Star (digit | alphabetic | '\'' | '_'), eof -> true
    | _ -> false
  ;;
end

module Lower_name = Identified_ustring (struct
  let valid = Name_lexing.valid_lower_name
  let error_msg = "Invalid lower name"

  let coerce_exn str =
    (* TODO: add proper unicode capitalization to lower name/upper name checking *)
    let str' = String.uncapitalize str in
    if valid str' then str' else raise_s [%message error_msg str]
  ;;
end)

module Upper_name = Identified_ustring (struct
  let valid = Name_lexing.valid_upper_name
  let error_msg = "Invalid upper name"

  let coerce_exn str =
    let str' = String.capitalize str in
    if valid str' then str' else raise_s [%message error_msg str]
  ;;
end)

module Module_name : Name = Upper_name

module Module_path : sig
  type t = Module_name.t list [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val of_ustrings_unchecked : Ustring.t list -> t
  val of_ustrings_exn : Ustring.t list -> t
  val to_ustring : t -> Ustring.t
end = struct
  module T = struct
    type t = Module_name.t list [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_ustrings_unchecked = List.map ~f:Module_name.of_ustring_unchecked
  let of_ustrings_exn = List.map ~f:Module_name.of_ustring_exn

  let to_ustring path =
    let q = Queue.create ~capacity:(List.length path * 10) () in
    List.iter path ~f:(fun s ->
      Queue.enqueue q (Uchar.of_char '.');
      Ustring.iter (Module_name.to_ustring s) ~f:(Queue.enqueue q));
    ignore (Queue.dequeue q : Uchar.t option);
    Queue.to_array q |> Ustring.of_array_unsafe
  ;;
end

module type Name_qualified = sig
  include Name

  module Qualified : sig
    type name = t
    type t = Module_path.t * name [@@deriving compare, equal, hash, sexp]

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val with_path : Module_path.t -> name -> t
    val of_ustrings_unchecked : Ustring.t list * Ustring.t -> t
    val of_ustrings_exn : Ustring.t list * Ustring.t -> t
    val to_ustring : t -> Ustring.t
  end
end

module Ustring_qualified (N : Name) : Name_qualified = struct
  include N

  module Qualified = struct
    type name = t

    module T = struct
      type nonrec t = Module_path.t * t [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let with_path path name = path, name

    let of_ustrings_unchecked (path, name) =
      Module_path.of_ustrings_unchecked path, of_ustring_unchecked name
    ;;

    let of_ustrings_exn (path, name) =
      Module_path.of_ustrings_exn path, of_ustring_exn name
    ;;

    let to_ustring (path, name) =
      let name = to_ustring name in
      let q = Queue.create ~capacity:(Ustring.length name * 2) () in
      List.iter path ~f:(fun s ->
        Ustring.iter (Module_name.to_ustring s) ~f:(Queue.enqueue q);
        Queue.enqueue q (Uchar.of_char '.'));
      Ustring.iter name ~f:(Queue.enqueue q);
      Queue.to_array q |> Ustring.of_array_unsafe
    ;;
  end
end

module Lower_name_qualified = Ustring_qualified (Lower_name)
module Upper_name_qualified = Ustring_qualified (Upper_name)
module Type_name : Name_qualified = Upper_name_qualified
module Cnstr_name : Name_qualified = Upper_name_qualified
module Trait_name : Name_qualified = Upper_name_qualified

module Value_name : sig
  include Name_qualified

  val of_cnstr_name : Cnstr_name.t -> t

  module Qualified : sig
    include module type of Qualified

    val of_cnstr_name : Cnstr_name.Qualified.t -> t
  end
end = struct
  include Lower_name_qualified

  let of_cnstr_name = Cnstr_name.to_ustring >> of_ustring_unchecked

  module Qualified = struct
    include Qualified

    let of_cnstr_name (path, name) = path, of_cnstr_name name
  end
end

module Type_param_name : sig
  include Name_qualified

  val default : t
  val next : t -> t
end = struct
  include Lower_name_qualified

  let default = of_string_unchecked "a"

  (* Generates names like: "a", .., "z", "aa", "ab", .. *)
  let next param =
    let param = to_ustring param in
    let buf = Buffer.create (Ustring.length param) in
    let rec loop buf param len =
      if Int.(len - 1 < Ustring.length param)
      then (
        let letter = Ustring.get param (len - 1) in
        match Uchar.to_char letter with
        | Some ('a' .. 'y' as letter) ->
          Ustring.add_substring_to_buffer buf param ~pos:0 ~len:(len - 1);
          Buffer.add_char buf (Char.unsafe_of_int (Char.to_int letter + 1))
        | Some _ | None ->
          loop buf param (len - 1);
          Buffer.add_char buf 'a')
      else Buffer.add_char buf 'a'
    in
    loop buf param (Ustring.length param);
    of_string_unchecked (Buffer.contents buf)
  ;;
end
