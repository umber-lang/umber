open Import

module type General_name = sig
  type t = private Ustring.t [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val empty : t
  val to_ustring : t -> Ustring.t
end

(* TODO: add string/ustring interning. Should be super easy to just add it to a core
   module like [General_name] that everything else includes and make the
   [of_string]/[of_ustring], etc. do a global hashtable lookup. We can make the names be
   represented as integers and then interning module paths will be fast as you can just
   hash each int in the list. *)
module type Unidentified_name = sig
  include General_name

  val of_ustring : Ustring.t -> t
  val of_string_exn : string -> t
end

module Unidentified_name : Unidentified_name = Ustring

module Extern_name : sig
  include Unidentified_name

  val prim_op_prefix : Uchar.t
  val is_prim_op : t -> bool
end = struct
  include Ustring

  let prim_op_prefix = Uchar.of_char '%'
  let is_prim_op t = (not (is_empty t)) && Uchar.( = ) (get t 0) prim_op_prefix
end

module type Name = sig
  include General_name

  val unidentify : t -> Unidentified_name.t
  val of_ustring : Ustring.t -> t Or_error.t
  val of_ustring_unchecked : Ustring.t -> t
  val of_ustring_exn : Ustring.t -> t
  val of_string_unchecked : string -> t
  val of_string_exn : string -> t
  val of_string_lenient_exn : string -> t
  val arg_type_lenient : t Command.Arg_type.t
end

module type Name_validator = sig
  val coerce : Sedlexing.lexbuf -> Ustring.t Or_error.t
  val coerce_lenient : string -> Ustring.t Or_error.t
end

module Identified_ustring (V : Name_validator) : Name = struct
  include Ustring

  let unidentify = to_ustring >> Unidentified_name.of_ustring
  let of_ustring_unchecked = of_ustring

  let of_ustring ustr =
    let lexbuf = Sedlexing.from_uchar_array (to_array ustr) in
    V.coerce lexbuf
  ;;

  let of_ustring_exn = ok_exn << of_ustring
  let of_string_unchecked = of_string_exn

  let of_string_exn str =
    let lexbuf = Sedlexing.Utf8.from_string str in
    V.coerce lexbuf |> ok_exn
  ;;

  let of_string_lenient_exn str = V.coerce_lenient str |> ok_exn
  let arg_type_lenient = Command.Arg_type.create of_string_lenient_exn
end

module Lower_name = Identified_ustring (struct
  let coerce lexbuf =
    match Lex_helpers.lex_lower_name lexbuf with
    | Some name -> Ok name
    | None ->
      let input = Sedlexing.Utf8.lexeme lexbuf in
      error_s [%message "Invalid lower name" input]
  ;;

  let coerce_lenient str =
    (* TODO: add proper unicode capitalization to lower name/upper name checking *)
    String.uncapitalize str |> Sedlexing.Utf8.from_string |> coerce
  ;;
end)

module Upper_name = Identified_ustring (struct
  let coerce lexbuf =
    match Lex_helpers.lex_upper_name lexbuf with
    | Some name -> Ok name
    | None ->
      let input = Sedlexing.Utf8.lexeme lexbuf in
      error_s [%message "Invalid upper name" input]
  ;;

  let coerce_lenient str = String.capitalize str |> Sedlexing.Utf8.from_string |> coerce
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
      module U = struct
        type nonrec t = Module_path.t * t [@@deriving compare, equal, hash]

        let iter_chars (path, name) ~f =
          List.iter path ~f:(fun module_name ->
            Ustring.iter (Module_name.to_ustring module_name) ~f;
            f (Uchar.of_char '.'));
          Ustring.iter (to_ustring name) ~f
        ;;

        let total_len (path, name) =
          List.fold
            path
            ~init:(Ustring.length (to_ustring name))
            ~f:(fun len module_name ->
              (* Add 1 additional char for the '.' *)
              len + Ustring.length (Module_name.to_ustring module_name) + 1)
        ;;

        let to_string t =
          let buf = Buffer.create ((total_len t + 3) / 4) in
          iter_chars t ~f:(Uchar.add_to_buffer buf);
          Buffer.contents buf
        ;;

        let to_ustring t =
          let q = Queue.create ~capacity:(total_len t) () in
          iter_chars t ~f:(Queue.enqueue q);
          Queue.to_array q |> Ustring.of_array_unsafe
        ;;

        let of_string s =
          match String.split s ~on:'.' |> List.split_last with
          | Some (path, name) ->
            List.map ~f:Module_name.of_string_exn path, of_string_exn name
          | None -> failwithf "Bad qualified name: '%s'" s ()
        ;;
      end

      include U
      include Sexpable.Of_stringable (U)
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
  val is_cnstr_name : t -> bool

  module Qualified : sig
    include module type of Qualified

    val of_cnstr_name : Cnstr_name.Qualified.t -> t
  end
end = struct
  include Lower_name_qualified

  let of_cnstr_name = of_ustring_unchecked << Cnstr_name.to_ustring
  let is_cnstr_name = Or_error.is_ok << Cnstr_name.of_ustring << to_ustring

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

module Unique_name : sig
  type t [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val of_ustring : Ustring.t -> t
  val base_name : t -> Ustring.t
  val to_ustring : t -> Ustring.t
  val to_string : t -> string
  val map_id : t -> f:(int -> int) -> t
end = struct
  module Id = Unique_id.Int ()

  module T = struct
    module U = struct
      type t = Ustring.t * Id.t [@@deriving compare, hash]

      let to_string (ustr, id) = [%string "%{ustr#Ustring}.%{id#Id}"]
      let to_ustring (ustr, id) = Ustring.(ustr ^ of_string_exn [%string ".%{id#Id}"])

      let of_string str =
        let name, id = String.rsplit2_exn str ~on:'.' in
        Ustring.of_string_exn name, Id.of_string id
      ;;
    end

    include U
    include Sexpable.Of_stringable (U)
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_ustring ustr = ustr, Id.create ()
  let base_name = fst
  let map_id t ~f = Tuple2.map_snd t ~f:(Id.of_int_exn << f << Id.to_int_exn)
end
