open Irc_model

class virtual skeleton address port = object (self)
  inherit Irc_model.Message.handler

  method loop (ichan, ochan) : unit Lwt.t =
    let%lwt message = Lwt_io.read ichan in
    self#on_message (Message.from_string message);
    self#loop (ichan, ochan)

  method start_async () : unit =
    Lwt.async self#lwt

  method start () : unit =
    Lwt_main.run (self#lwt ())
end

class dummy address port = object
  inherit skeleton address port

  method on_welcome _ _ = ()
  method on_pass _ _ = ()
  method on_nosuchnick _ _ = ()
end
