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

  method on_welcome _ _ _ = ()
  method on_yourhost _ _ _ = ()
  method on_created _ _ _ = ()
  method on_myinfo _ _ _ _ _ _ = ()
  method on_bounce _ _ _ = ()

  method on_pass _ _ = ()
  method on_privmsg _ _g _ = ()
  method on_notice _ _ _ = ()

  method on_nosuchnick _ _ = ()
end

open Irc_model

type params =
  { nicks : string list ;
    user : string ;
    realname : string }

class full conn params = object
  inherit dummy conn as super

  method! start () =
    Connection.send_nick conn (Nickname.from_string (List.hd params.nicks));
    Connection.send_user conn params.user 0 params.realname;
    super#start ()
end
