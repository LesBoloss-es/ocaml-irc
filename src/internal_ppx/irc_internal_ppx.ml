
open Parsetree
open Asttypes
open Ast_helper
   
let rec list_ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: q -> list_ft q

let cstr_decl_to_ppat_construct ~loc cstr_decl =
  let name = Location.mkloc (Longident.Lident cstr_decl.pcd_name.txt) loc in
  match cstr_decl.pcd_args with
  | Pcstr_tuple [] ->
     Ppat_construct (name, None)
  | Pcstr_tuple [_] ->
     Ppat_construct (name, Some (Pat.var ~loc (Location.mkloc "c0" loc)))
  | Pcstr_tuple ctl ->
     Ppat_construct (
         name,
         Some (
             Pat.tuple
               ~loc
               (List.mapi
                  (fun i _ -> Pat.var ~loc (Location.mkloc ("c"^(string_of_int i)) loc))
                  ctl)
           )
       )
  | _ -> assert false

let name_of_name_and_suffix name suffix =
  if name = "t" then
    suffix
  else
    name ^ "_" ^ suffix

let handler_name_of_name name = "on_" ^ name

let cstr_decl_to_handler_class_method ~loc ~options ~path cstr_decl =
  (* This function takes one constructor of the variant type and
     creates the method `on_<constructor name>` that takes the
     arguments and does nothing. *)
  ignore options; ignore path;
  {
    pcf_loc = loc ;
    pcf_attributes = [] ;
    pcf_desc =
      Pcf_method (
          Location.mkloc (handler_name_of_name cstr_decl.pcd_name.txt) loc, Public,
          Cfk_concrete (Fresh , [%expr ()])
        )
  }

let cstr_decls_to_handler_class_main_method ~loc ~options ~path name cstr_decls =
  (* This function takes the constructors of the variant type and
     creates the method `on_<type name>` that matches over its value
     and call the right handler method. *)
  ignore options;
  {
    pcf_loc = loc ;
    pcf_attributes = [] ;
    pcf_desc =
      Pcf_method (
          Location.mkloc ("on_" ^ (if name = "t" then list_ft path else name)) loc, Public,
          Cfk_concrete (
              Fresh,
              [%expr fun prefix ->
                  [%e {
                        pexp_loc = loc ;
                        pexp_attributes = [] ;
                        pexp_desc =
                          (* function | _ -> _ | ... *)
                          Pexp_function
                            (
                              List.map
                                (fun cstr_decl ->
                                  {
                                    pc_lhs =
                                      {
                                        ppat_loc = loc ;
                                        ppat_attributes = [] ;
                                        ppat_desc = cstr_decl_to_ppat_construct ~loc cstr_decl
                                      } ;
                                    pc_guard = None ;
                                    pc_rhs =
                                      {
                                        pexp_loc = loc ;
                                        pexp_attributes = [] ;
                                        pexp_desc =
                                          (* We call self#on_<constructor name> with that right number of arguments. *)
                                          Pexp_apply (
                                              {
                                                pexp_loc = loc ;
                                                pexp_attributes = [] ;
                                                pexp_desc =
                                                  Pexp_field (
                                                      {
                                                        pexp_loc = loc ;
                                                        pexp_attributes = [] ;
                                                        pexp_desc =
                                                          Pexp_ident (Location.mkloc (Longident.Lident "self") loc)
                                                      },
                                                      Location.mkloc (Longident.Lident (handler_name_of_name cstr_decl.pcd_name.txt)) loc
                                                    )
                                              },
                                              List.mapi
                                                (fun i _ ->
                                                  Nolabel,
                                                  {
                                                    pexp_loc = loc ;
                                                    pexp_attributes = [] ;
                                                    pexp_desc =
                                                      Pexp_ident (Location.mkloc (Longident.Lident ("c"^(string_of_int i))) loc)
                                                  })
                                                (match cstr_decl.pcd_args with
                                                 | Pcstr_tuple ctl -> ctl
                                                 | _ -> assert false)
                                            )
                                      }
                                  }
                                )
                                cstr_decls
                            )
                      }
                  ]
              ]
            )
        )
  }

let cstr_decls_to_handler_class ~loc ~options ~path name cstr_decls =
  Pstr_class
    [{
        pci_virt = Concrete ;
        pci_params = [] ;
        pci_name = Location.mkloc (name_of_name_and_suffix name "handler") loc;
        pci_expr =
          {
            pcl_desc =
              Pcl_structure
                {
                  pcstr_self = [%pat? self];
                  pcstr_fields =
                    (cstr_decls_to_handler_class_main_method ~loc ~options ~path name cstr_decls)
                    :: List.map
                         (cstr_decl_to_handler_class_method ~loc ~options ~path)
                         cstr_decls
                };
            pcl_loc = loc ;
            pcl_attributes = []
          } ;
        pci_loc = loc ;
        pci_attributes = []
    }]


let type_decl_to_everything ~options ~path = function
  (* We expect exactly one type declaration. *)
  | [type_decl] ->
     (
       match type_decl.ptype_kind with
       (* This type declaration must be a variant. *)
       | Ptype_variant cstr_decls ->
          let loc = type_decl.ptype_loc in
          [{
              pstr_desc =
                cstr_decls_to_handler_class
                  ~loc
                  ~options
                  ~path
                  type_decl.ptype_name.txt
                  cstr_decls ;
              pstr_loc = loc
          }]
       | _ ->
          assert false
     )
  | _ ->
     assert false

let () = Ppx_deriving.(register (create "irc_internal_ppx" ~type_decl_str:type_decl_to_everything ()))
