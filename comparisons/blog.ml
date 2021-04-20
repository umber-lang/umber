(* Inspired by: https://doc.rust-lang.org/book/ch17-03-oo-design-patterns.html 
   (but implemented with phantom types and polymorphic variants) *)

module Post_state = struct
  type t =
    [ `Draft
    | `Pending_review
    | `Published
    ]
end

module Post : sig
  type 'state t constraint 'state = [< Post_state.t ]

  val empty : [ `Draft ] t
  val add_text : [ `Draft ] t -> string -> [ `Draft ] t
  val request_review : [ `Draft ] t -> [ `Pending_review ] t
  val approve : [ `Pending_review ] t -> [ `Published ] t
  val reject : [ `Pending_review ] t -> [ `Draft ] t
  val content : [ `Published ] t -> string
end = struct
  type 'state t = { content : string } constraint 'state = [< Post_state.t ]

  let cast ({ content = _ } as t) = t
  let empty = { content = "" }
  let add_text t text = { content = t.content ^ text }
  let request_review = cast
  let approve = cast
  let reject = cast

  (* Note: this can't currently have the behavior that it's callable on any post, but
     just returns an empty string before the post is published because the state is a
     phantom type parameter and not available at runtime. This could be fixed by adding a
     [state] field to the type, although the casting we do would then be invalid. *)
  let content t = t.content
end

let () =
  let post = Post.empty in
  Post.add_text post "Here is some text."
  |> Post.request_review
  |> Post.approve
  |> Post.content
  |> print_endline
;;
