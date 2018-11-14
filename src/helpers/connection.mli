type t

val make : socket:Lwt_unix.file_descr -> t

val open_ : address:string -> port:int -> t Lwt.t
(** Opens a connection to the given address and port *)

val equal : t -> t -> bool

val host : t -> string

val send : t -> Irc_model.Message.t -> unit Lwt.t
val send_async : t -> Irc_model.Message.t -> unit

val send_l : t -> Irc_model.Message.t list -> unit Lwt.t
val send_async_l : t -> Irc_model.Message.t list -> unit

val receive : t -> (Irc_model.Message.t, unit) result Lwt.t
val receive_stream : t -> (Irc_model.Message.t, unit) result Lwt_stream.t

module Table : sig
  include Hashtbl.S with type key = t

  val to_list : 'a t -> (key * 'a) list

  val with_ : 'a t -> key -> ('a -> 'b) -> (unit -> 'b) -> 'b
end
