open Irc.Common open Irc.Model open Irc.Client

let mirror conn = object (_self)
  inherit dummy conn (* as super *)

  (* method! start () =
   *   Connection.send_async conn (Message.make_noprefix (Command (Privmsg (Nickname (Nickname.from_string "Niols"), "Yo"))));
   *   super#start () *)

  method! on_privmsg prefix target content =
    let source =
      match unwrap prefix with
      | Identity identity -> Identity.nick identity
      | _ -> assert false
    in
    let open Command in
    (
      match target with
      | Channel channel ->
         Privmsg (Channel channel, content)
      | Nickname _ ->
         Privmsg (Nickname source, content)
    )
    |> (fun command -> Message.Command command)
    |> Message.make_noprefix
    |> Irc.Client.Connection.send_async conn
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
