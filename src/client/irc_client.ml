open Irc_common
module Connection = Connection

class virtual skeleton conn = object (self)
  inherit Irc_model.Message.handler

  method loop () : unit Lwt.t =
    let%lwt message = Connection.receive conn in
    self#on_message message;
    self#loop ()

  method start () =
    Lwt.return ()

  method build () =
    self#start () >>= fun () ->
    self#loop ()

  method run_async () : unit =
    Lwt.async self#build

  method run () : unit =
    Lwt_main.run (self#build ())
end

class dummy conn = object
  inherit skeleton conn

  method on_welcome _ _ = ()
  method on_pass _ _ = ()
  method on_privmsg _ _ _ = ()
  method on_nosuchnick _ _ = ()
end
