
open Parsetree
open Asttypes

let name_of_name_and_suffix name suffix =
  if name = "t" then
    suffix
  else
    name ^ "_" ^ suffix

let cstr_decl_to_handler_class_method ~options ~path cstr_decl =
  ignore options; ignore path;
  {
    pcf_loc = Location.none ;
    pcf_attributes = [] ;
    pcf_desc =
      Pcf_method (
          Location.mknoloc ("on_" ^ cstr_decl.pcd_name.txt), Public,
          Cfk_concrete (Fresh , [%expr ()])
        )
  }

let cstr_decls_to_handler_class_main_method ~options ~path name _cstr_decls =
  ignore options;
  {
    pcf_loc = Location.none ;
    pcf_attributes = [] ;
    pcf_desc =
      Pcf_method (
          Location.mknoloc ("on_" ^ (if name = "t" then List.hd path else name)), Public,
          Cfk_concrete (Fresh, [%expr ()])
        )
  }

let cstr_decls_to_handler_class ~options ~path name cstr_decls =
  Pstr_class
    [{
        pci_virt = Concrete ;
        pci_params = [] ;
        pci_name = Location.mknoloc (name_of_name_and_suffix name "handler") ;
        pci_expr =
          {
            pcl_desc =
              Pcl_structure
                {
                  pcstr_self = [%pat? _self];
                  pcstr_fields =
                    (cstr_decls_to_handler_class_main_method ~options ~path name cstr_decls)
                    :: List.map
                         (cstr_decl_to_handler_class_method ~options ~path)
                         cstr_decls
                };
            pcl_loc = Location.none ;
            pcl_attributes = []
          } ;
        pci_loc = Location.none ;
        pci_attributes = []
    }]


let type_decl_to_everything ~options ~path = function
  (* We expect exactly one type declaration. *)
  | [type_decl] ->
     (
       match type_decl.ptype_kind with
       (* This type declaration must be a variant. *)
       | Ptype_variant cstr_decls ->
          [{
              pstr_desc =
                cstr_decls_to_handler_class
                  ~options
                  ~path
                  type_decl.ptype_name.txt
                  cstr_decls ;
              pstr_loc =
                Location.none
          }]
       | _ ->
          assert false
     )
  | _ ->
     assert false

let () = Ppx_deriving.(register (create "irc_internal_ppx" ~type_decl_str:type_decl_to_everything ()))
