open Irc_model
let (>>=) = Lwt.bind

class virtual m = object (self)
  (* FIXME: queue of messages
     val messages : Message.t Queue.t = Queue.create () *)

  method virtual on_privmsg : Server.t -> Identity.t -> Target.t -> string -> unit Lwt.t

  method on_message (server : Server.t) (message : Message.t) : unit Lwt.t =
    match Message.prefix message, Message.command message with
    | Some (Identity source), Privmsg (target, content) ->
       self#on_privmsg server source target content
    | _ ->
       Lwt.return () (*FIXME*)

  method loop (server : Server.t) : unit Lwt.t =
    Server.read server
    >>= self#on_message server
    >>= fun () -> self#loop server

  method thread () : unit Lwt.t =
    let server = Server.make address port in
    Server.connect server;
    self#loop server

  method start_async () : unit =
    Lwt.async self#thread

  method start () : unit =
    Lwt_main.run (self#thread ())
end
