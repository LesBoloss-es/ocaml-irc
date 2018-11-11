open Irc.Common open Irc.Model open Irc.Client

let config =
  { nicks = ["mirror"; "fallback_nickname"] ;
    user = "mexample" ;
    realname = "Mirror Example" }

let mirror conn = object (_self)
  inherit full conn config

  method! on_welcome _ _ _ =
    Connection.send_async conn (Helpers.join [Channel.from_string "#abcdefgh", None])

  method! on_privmsg prefix target content =
    let source =
      match unwrap prefix with
      | Identity identity -> Identity.nick identity
      | _ -> assert false
    in
    (
      match target with
      | All -> assert false
      | Channel channel -> Helpers.privmsg (Channel channel) content
      | Nickname _ -> Helpers.privmsg (Nickname source) content
    )
    |> Connection.send_async conn
end

let main () =
  let%lwt conn =
    Connection.open_
      ~address:"104.200.152.162" (* freenode *)
      ~port:6667
  in
  (mirror conn)#build()

let () =
  Lwt_main.run (main ())
