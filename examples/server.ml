let () =
  let module Config = struct
      let listen_address = "any"
      let listen_port = 6667
      let servername = "ocaml-irc"
    end
  in
  let module Server = Irc.Server.GenericServer(Config) in
  let _server = Server.run_async () in
  Lwt_main.run (Lwt_unix.sleep 100000.)
