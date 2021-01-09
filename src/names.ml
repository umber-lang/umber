open Import

module type Unidentified_name = sig
  type t = private Ustring.t [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t

  val of_ustring : Ustring.t -> t
  val to_ustring : t -> Ustring.t
  val of_string_exn : string -> t
  val create_fresh : unit -> t
end

module Unidentified_ustring = struct
  include Ustring
  module Id = Unique_id.Int ()

  let create_fresh () = Id.create () |> Id.to_string |> Ustring.of_string_exn
end

module Unidentified_name : Unidentified_name = Unidentified_ustring

module type Name = sig
  include Unidentified_name

  val unidentify : t -> Unidentified_name.t
end

module Ustring = struct
  include Unidentified_ustring

  let unidentify = to_ustring >> Unidentified_name.of_ustring
end

module Module_name : Name = Ustring

module Module_path : sig
  type t = Module_name.t list [@@deriving compare, equal, hash, sexp]

  val of_ustrings : Ustring.t list -> t
  val to_ustring : t -> Ustring.t
end = struct
  type t = Module_name.t list [@@deriving compare, equal, hash, sexp]

  let of_ustrings = List.map ~f:Module_name.of_ustring

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

    val with_path : Module_path.t -> name -> t
    val of_ustrings : Ustring.t list * Ustring.t -> t
    val to_ustring : t -> Ustring.t
    val create_fresh : unit -> t
  end
end

module Ustring_qualified = struct
  include Ustring

  module Qualified = struct
    type name = t

    module T = struct
      type nonrec t = Module_path.t * t [@@deriving compare, equal, hash, sexp]
    end

    include T
    include Comparable.Make (T)

    let with_path path name = path, name
    let of_ustrings (path, name) = Module_path.of_ustrings path, name

    let to_ustring (path, name) =
      let name = to_ustring name in
      let q = Queue.create ~capacity:(Ustring.length name * 2) () in
      List.iter path ~f:(fun s ->
        Ustring.iter (Module_name.to_ustring s) ~f:(Queue.enqueue q);
        Queue.enqueue q (Uchar.of_char '.'));
      Ustring.iter name ~f:(Queue.enqueue q);
      Queue.to_array q |> Ustring.of_array_unsafe
    ;;

    let create_fresh () = [], create_fresh ()
  end
end

module Type_name : Name_qualified = Ustring_qualified
module Cnstr_name : Name_qualified = Ustring_qualified
module Trait_name : Name_qualified = Ustring_qualified

module Value_name : sig
  include Name_qualified

  val of_cnstr_name : Cnstr_name.t -> t

  module Qualified : sig
    include module type of Qualified

    val of_cnstr_name : Cnstr_name.Qualified.t -> t
  end
end = struct
  include Ustring_qualified

  let of_cnstr_name = Cnstr_name.to_ustring

  module Qualified = struct
    include Qualified

    let of_cnstr_name (path, name) = path, Cnstr_name.to_ustring name
  end
end

module Type_param_name : sig
  include Name_qualified

  val default : t
  val next : t -> t
end = struct
  include Ustring_qualified

  let default = of_string_exn "a"

  (* Generates names like: "a", .., "z", "aa", "ab", .. *)
  let next param =
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
    of_string_exn (Buffer.contents buf)
  ;;
end
