
class generic = object (self)
  inherit Irc_model.Command.handler

  method on_message message =
    let open Irc_model in
    self#on_command (Message.prefix message) (Message.command message)

  method loop () : unit Lwt.t =
    Server.read server
    >>= self#on_message
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
