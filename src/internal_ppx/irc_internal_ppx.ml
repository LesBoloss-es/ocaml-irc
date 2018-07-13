
open Parsetree
open Asttypes
open Ast_helper

let type_decl_to_everything ~options ~path = function
  (* We expect exactly one type declaration. *)
  | [type_decl] ->
     (
       match type_decl.ptype_kind with
       (* This type declaration must be a variant. *)
       | Ptype_variant cstr_decls ->
          Ast_helper.default_loc := type_decl.ptype_loc;
          [Str.mk
             (Handler.cstr_decls_to_handler_class
                ~options
                ~path
                type_decl.ptype_name.txt
                cstr_decls);

           Str.value
             Nonrecursive
             [Parser.cstr_decls_to_value_binding ~options ~path type_decl.ptype_name.txt cstr_decls]
          ]
       | _ ->
          assert false
     )
  | _ ->
     assert false

let () = Ppx_deriving.(register (create "irc_internal_ppx" ~type_decl_str:type_decl_to_everything ()))
