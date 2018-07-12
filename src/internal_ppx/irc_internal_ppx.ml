
let type_decl_str ~options ~path type_decls =
  ignore options;
  ignore path;
  ignore type_decls;
  Format.printf "#options: %d@." (List.length options);
  Format.printf "#path: %d %s@." (List.length path) (List.hd path);
  Format.printf "#type_decls: %d@." (List.length type_decls);
  [%str class handler = object end]

let () = Ppx_deriving.(register (create "irc_internal_ppx" ~type_decl_str ()))
