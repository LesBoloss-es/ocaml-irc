open Parsetree
open Asttypes
open Ast_helper
   
let type_decl_str ~options ~path = function
  | [type_decl] ->
     Ast_helper.default_loc := type_decl.ptype_loc;
     [
       (* The 'handler' class *)
       Str.mk (Handler.handler ~options ~path type_decl);

       (* The to_strings function *)
       Str.value Nonrecursive [Printer.to_strings ~options ~path type_decl];

       (* The from_strings function (FIXME) *)
     ]
  | _ ->
     assert false

let () = Ppx_deriving.(register (create "irc_internal_ppx" ~type_decl_str ()))
