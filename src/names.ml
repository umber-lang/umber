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
  let coerce = Lex_helpers.lex_lower_name

  let coerce_lenient str =
    (* TODO: add proper unicode capitalization to lower name/upper name checking *)
    String.uncapitalize str |> Sedlexing.Utf8.from_string |> coerce
  ;;
end)

module Upper_name = Identified_ustring (struct
  let coerce lexbuf = Lex_helpers.lex_upper_name lexbuf
  let coerce_lenient str = String.capitalize str |> Sedlexing.Utf8.from_string |> coerce
end)

module Module_name : Name = Upper_name

module Module_path : sig
  type +'a t = private Module_name.t list [@@deriving compare, equal, hash, sexp]
  type absolute = [ `Absolute ] [@@deriving compare, equal, hash, sexp]

  type relative =
    [ absolute
    | `Relative
    ]
  [@@deriving compare, equal, hash, sexp]

  val is_empty : _ t -> bool
  val is_prefix : prefix:'a t -> 'a t -> bool
  val append : 'a t -> Module_name.t list -> 'a t
  val append' : 'a t -> _ t -> 'a t
  val to_ustring : _ t -> Ustring.t

  module Relative : sig
    type nonrec t = relative t [@@deriving compare, equal, hash, sexp]

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val to_module_names : t -> Module_name.t list
    val of_module_names : Module_name.t list -> t
    val of_ustrings_unchecked : Ustring.t list -> t
    val of_ustrings_exn : Ustring.t list -> t
    val empty : t
  end

  module Absolute : sig
    type nonrec t = absolute t [@@deriving compare, equal, hash, sexp]

    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val to_relative : t -> Relative.t
    val of_relative_unchecked : Relative.t -> t
    val empty : t
  end
end = struct
  type 'a t = Module_name.t list [@@deriving compare, equal, hash, sexp]
  type absolute = [ `Absolute ] [@@deriving compare, equal, hash, sexp]

  type relative =
    [ absolute
    | `Relative
    ]
  [@@deriving compare, equal, hash, sexp]

  let is_empty = List.is_empty

  let rec is_prefix ~prefix:t t' =
    match t, t' with
    | [], ([] | _ :: _) -> true
    | _ :: _, [] -> false
    | module_name :: rest, module_name' :: rest' ->
      if Module_name.equal module_name module_name'
      then is_prefix ~prefix:rest rest'
      else false
  ;;

  let append = ( @ )
  let append' = ( @ )

  let to_ustring path =
    let q = Queue.create ~capacity:(List.length path * 10) () in
    List.iter path ~f:(fun s ->
      Queue.enqueue q (Uchar.of_char '.');
      Ustring.iter (Module_name.to_ustring s) ~f:(Queue.enqueue q));
    ignore (Queue.dequeue q : Uchar.t option);
    Queue.to_array q |> Ustring.of_array_unchecked
  ;;

  module Relative = struct
    (* TODO: Maybe the sexp of this type should use the nice ustring representation, rather
     than just being a sexp list. *)
    module T = struct
      type t = Module_name.t list [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let of_module_names = Fn.id
    let to_module_names = Fn.id
    let of_ustrings_unchecked = List.map ~f:Module_name.of_ustring_unchecked
    let of_ustrings_exn = List.map ~f:Module_name.of_ustring_exn
    let empty = []
  end

  module Absolute = struct
    include Relative

    let to_relative = Fn.id
    let of_relative_unchecked = Fn.id
  end
end

module type Name_qualified = sig
  include Name

  type name := t

  module Qualified : sig
    type 'a t = 'a Module_path.t * name [@@deriving compare, equal, hash, sexp]
  end

  module Relative : sig
    type t = Module_path.relative Qualified.t [@@deriving compare, equal, hash, sexp]

    include Stringable.S with type t := t
    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val with_path : Module_path.Relative.t -> name -> t
    val of_ustrings_unchecked : Ustring.t list * Ustring.t -> t
    val of_ustrings_exn : Ustring.t list * Ustring.t -> t
    val to_ustring : t -> Ustring.t
  end

  module Absolute : sig
    type t = Module_path.absolute Qualified.t [@@deriving compare, equal, hash, sexp]

    include Stringable.S with type t := t
    include Comparable.S with type t := t
    include Hashable.S with type t := t

    val of_relative_unchecked : Relative.t -> t
    val to_relative : t -> Relative.t
    val with_path : Module_path.Absolute.t -> name -> t
    val to_ustring : t -> Ustring.t
  end
end

module Ustring_qualified (N : Name) : Name_qualified = struct
  include N

  module Qualified = struct
    type nonrec 'a t = 'a Module_path.t * t [@@deriving compare, equal, hash, sexp]
  end

  module Make (Path : sig
    type t [@@deriving compare, equal, hash]

    val to_module_names : t -> Module_name.t list
    val of_module_names : Module_name.t list -> t
  end) =
  struct
    module T = struct
      module U = struct
        type nonrec t = Path.t * t [@@deriving compare, equal, hash]

        let iter_chars (path, name) ~f =
          List.iter (Path.to_module_names path) ~f:(fun module_name ->
            Ustring.iter (Module_name.to_ustring module_name) ~f;
            f (Uchar.of_char '.'));
          Ustring.iter (to_ustring name) ~f
        ;;

        let total_len ((path, name) : t) =
          List.fold
            (Path.to_module_names path)
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
          Queue.to_array q |> Ustring.of_array_unchecked
        ;;

        let split_dots s =
          let rec loop i j acc s =
            if Int.( >= ) j (String.length s)
            then List.rev acc, String.subo s ~pos:i
            else if Char.equal s.[j] '.'
            then
              if Int.equal i j
              then
                (* Multiple '.'s in a row. This is ok, seen in e.g.
                   `Std.Prelude.Operators..` This is only allowed in the name itself, so
                   we can just take the rest of the string *)
                List.rev acc, String.subo s ~pos:i
              else (
                let substring = String.sub s ~pos:i ~len:(j - i) in
                match Module_name.of_ustring (Ustring.of_string_exn substring) with
                | Error _ ->
                  (* This is not a module name, so it must be the last name in the path. *)
                  List.rev acc, String.subo s ~pos:i
                | Ok module_name -> loop (j + 1) (j + 1) (module_name :: acc) s)
            else loop i (j + 1) acc s
          in
          loop 0 0 [] s
        ;;

        let of_string s =
          let path, name = split_dots s in
          if String.is_empty name then failwithf "Bad qualified name: '%s'" s ();
          Path.of_module_names path, of_string_unchecked name
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
      ( Path.of_module_names (List.map path ~f:Module_name.of_ustring_unchecked)
      , of_ustring_unchecked name )
    ;;

    let of_ustrings_exn (path, name) =
      ( Path.of_module_names (List.map path ~f:Module_name.of_ustring_exn)
      , of_ustring_exn name )
    ;;
  end

  module Relative = Make (Module_path.Relative)

  module Absolute = struct
    include Make (struct
      include Module_path.Absolute

      let to_module_names t = (t : Module_path.Absolute.t :> Module_name.t list)

      let of_module_names =
        Module_path.Absolute.of_relative_unchecked << Module_path.Relative.of_module_names
      ;;
    end)

    let to_relative (path, name) = Module_path.Absolute.to_relative path, name

    let of_relative_unchecked (path, name) =
      Module_path.Absolute.of_relative_unchecked path, name
    ;;
  end
end

module Lower_name_qualified = Ustring_qualified (Lower_name)
module Upper_name_qualified = Ustring_qualified (Upper_name)

module Type_name : sig
  include Name_qualified

  (** Create a fresh type name that looks like `_Skolemized1`. For use in creating fresh
      abstract types ("skolemization"). *)
  val create_skolemized : unit -> t
end = struct
  include Upper_name_qualified
  module Skolemized_id = Unique_id.Int ()

  let create_skolemized () =
    let id = Skolemized_id.create () in
    of_string_unchecked [%string "_Skolemized%{id#Skolemized_id}"]
  ;;
end

module Cnstr_name : Name_qualified = Upper_name_qualified
module Trait_name : Name_qualified = Upper_name_qualified

module Value_name : sig
  include Name_qualified

  val of_cnstr_name : Cnstr_name.t -> t
  val to_cnstr_name : t -> Cnstr_name.t Or_error.t
  val is_cnstr_name : t -> bool

  module Relative : sig
    include module type of Relative

    val of_cnstr_name : Cnstr_name.Relative.t -> t
  end
end = struct
  include Lower_name_qualified

  let of_cnstr_name = of_ustring_unchecked << Cnstr_name.to_ustring
  let to_cnstr_name = Cnstr_name.of_ustring << to_ustring
  let is_cnstr_name = Or_error.is_ok << to_cnstr_name

  module Relative = struct
    include Relative

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

module Mir_name : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  module Name_table : sig
    type t [@@deriving sexp_of]

    val create : unit -> t
  end

  val create_value_name : Name_table.t -> Value_name.Relative.t -> t
  val copy_name : Name_table.t -> t -> t
  val to_ustring : t -> Ustring.t
  val to_string : t -> string
end = struct
  module Name_table = struct
    type t = int Value_name.Relative.Table.t [@@deriving sexp_of]

    let create () = Value_name.Relative.Table.create ()
  end

  module T = struct
    module U = struct
      type t = Value_name.Relative.t * int [@@deriving compare, equal, hash]

      let to_string (value_name, id) =
        if id = 0
        then Value_name.Relative.to_string value_name
        else [%string "%{value_name#Value_name.Relative}.%{id#Int}"]
      ;;

      let to_ustring (value_name, id) =
        let ustr = Value_name.Relative.to_ustring value_name in
        if id = 0 then ustr else Ustring.(ustr ^ of_string_exn [%string ".%{id#Int}"])
      ;;

      (* TODO: I don't think this is actually used. We should probably remove it. *)
      let of_string str =
        let name, id =
          match String.rsplit2 str ~on:'.' with
          | Some (name, id) ->
            if (not (String.is_empty id)) && String.for_all ~f:Char.is_digit id
            then name, Int.of_string id
            else str, 0
          | None -> str, 0
        in
        Value_name.Relative.of_string name, id
      ;;
    end

    include U
    include Sexpable.Of_stringable (U)
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create_value_name name_table value_name =
    let id = Option.value (Hashtbl.find name_table value_name) ~default:0 in
    Hashtbl.set name_table ~key:value_name ~data:(id + 1);
    value_name, id
  ;;

  let copy_name name_table (value_name, _) = create_value_name name_table value_name
end
