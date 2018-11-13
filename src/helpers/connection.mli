type t

val make : sockaddr:Unix.sockaddr -> ichan:Lwt_io.input_channel -> ochan:Lwt_io.output_channel -> t

val open_ : address:string -> port:int -> t Lwt.t
(** Opens a connection to the given address and port *)

val send : t -> Irc_model.Message.t -> unit Lwt.t
val send_async : t -> Irc_model.Message.t -> unit

val receive : t -> (Irc_model.Message.t, unit) result Lwt.t
val receive_stream : t -> (Irc_model.Message.t, unit) result Lwt_stream.t

val equal : t -> t -> bool

module Table : sig
  include Hashtbl.S with type key = t

  val to_list : 'a t -> (key * 'a) list
end
