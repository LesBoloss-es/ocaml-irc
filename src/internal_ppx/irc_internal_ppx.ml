
let () =
  Format.eprintf "Yo!@.";
  Ppx_deriving.(
    register (
        create
          "irc_internal_ppx"

          ~type_decl_str:(
            fun ~options ~path type_decls ->
            Format.eprintf "type_decl_str@.";
            [%expr class handler = object end]
          )

          ~type_decl_sig:(
            fun ~options ~path type_decls ->
            Format.eprintf "type_decl_sig@.";
            [%expr class handler = object end]
          )
          ()));
  Format.eprintf "Done.@."
