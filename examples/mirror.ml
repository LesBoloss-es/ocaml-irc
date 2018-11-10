open Irc.Common open Irc.Model open Irc.Client

let params =
  { nicks = ["skdjfh"] ;
    user = "sldkfj" ;
    realname = "dflkg jdlfk" }

let mirror conn = object (_self)
  inherit full conn params

  method! on_welcome _ _ _ =
    Connection.send conn (Message.from_command (Command.Join [Channel.from_string "#abcdefgh",None]))

  method! on_privmsg prefix target content =
    let source =
      match unwrap prefix with
      | Identity identity -> Identity.nick identity
      | _ -> assert false
    in
    let open Command in
    (
      match target with
      | All ->
         assert false
      | Channel channel ->
         Privmsg (Channel channel, content)
      | Nickname _ ->
         Privmsg (Nickname source, content)
    )
    |> Message.from_command
    |> Connection.send conn
end

let main () =
  let%lwt conn =
    Irc.Client.Connection.open_
      ~address:"104.200.152.162" (* freenode *)
      ~port:6667
  in
  (mirror conn)#build()

let () =
  Lwt_main.run (main ())
