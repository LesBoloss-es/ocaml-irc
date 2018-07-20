open Parsetree
open Ast_helper

let type_decl_str ~options ~path = function
  | [type_decl] ->
     Ast_helper.default_loc := type_decl.ptype_loc;
     let str =
       (* The handling class *)
       [Str.mk (Handler.handler ~options ~path type_decl)]

       @ (* The printing functions *)
         Printer.all ~options ~path type_decl

       @ (* The parsing functions *)
         []
     in
     (* Pprintast.structure Format.err_formatter str; *)
     str

  | _ ->
     assert false

let () = Ppx_deriving.(register (create Common.deriver ~type_decl_str ()))
