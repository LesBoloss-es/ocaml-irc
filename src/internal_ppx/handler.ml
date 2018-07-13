
open Parsetree
open Asttypes
open Ast_helper

let str s =
  Location.mkloc s !Ast_helper.default_loc

let ident s =
  str (Longident.Lident s)

let rec list_ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: q -> list_ft q

let name_of_name_and_suffix name suffix =
  if name = "t" then
    suffix
  else
    name ^ "_" ^ suffix

let handler_name_of_name name =
  "on_" ^ (String.lowercase_ascii name)

let cstr_decl_to_var_names cstr_decl =
  match cstr_decl.pcd_args with
  | Pcstr_tuple ctl -> List.mapi (fun i _ -> "c"^(string_of_int i)) ctl
  | _ -> assert false

let cstr_decl_to_ppat_construct cstr_decl =
  (* Take the declaration of a constructor and returns the pattern
     that matches it into variables c0 ... cn *)
  let name = ident cstr_decl.pcd_name.txt in
  match cstr_decl.pcd_args with
  | Pcstr_tuple [] ->
     Pat.construct name None
  | Pcstr_tuple [_] ->
     Pat.construct name (Some (Pat.var (str "c0")))
  | Pcstr_tuple ctl ->
     Pat.construct
       name
       (Some (
            Pat.tuple
              (List.mapi
                 (fun i _ -> Pat.var (str ("c"^(string_of_int i))))
                 ctl)
       ))
  | _ -> assert false

let cstr_decl_to_pexp_apply cstr_decl =
  (* Take the declaration of a constructor and returns the
     corresponding function application *)
  let args =
    match cstr_decl.pcd_args with
    | Pcstr_tuple [] ->
       [Nolabel, [%expr ()]]
    | Pcstr_tuple ctl ->
       List.mapi
         (fun i _ ->
           Nolabel,
           Exp.ident (ident ("c"^(string_of_int i))))
         ctl
    | _ -> assert false
  in
  Exp.apply
    (Exp.send
       (Exp.ident (ident "self"))
       (str (handler_name_of_name cstr_decl.pcd_name.txt)))
    args

let cstr_decl_to_handler_class_method ~options ~path cstr_decl =
  (* This function takes one constructor of the variant type and
     creates the method `on_<constructor name>` that takes the
     arguments and does nothing. *)
  ignore options; ignore path;
  Cf.method_
    (str (handler_name_of_name cstr_decl.pcd_name.txt))
    Public
    (Cfk_concrete
       (Fresh,
        match cstr_decl.pcd_args with
        | Pcstr_tuple [] ->
           Exp.fun_ Nolabel None [%pat? ()] [%expr ()]
        | Pcstr_tuple ctl ->
           let var_names =
             List.mapi
               (fun i _ -> "c" ^ (string_of_int i))
               ctl
           in
           List.fold_right
             (fun var rhs ->
               Exp.fun_ Nolabel None (Pat.var (str var)) rhs)
             var_names
             [%expr ()]
        | _ -> assert false))

let cstr_decls_to_handler_class_main_method ~options ~path name cstr_decls =
  (* This function takes the constructors of the variant type and
     creates the method `on_<type name>` that matches over its value
     and call the right handler method. *)
  ignore options;
  Cf.method_
    (str (handler_name_of_name (if name = "t" then list_ft path else name)))
    Public
    (Cfk_concrete (
         Fresh,
         [%expr fun prefix ->
             [%e Exp.function_
                 (List.map
                    (fun cstr_decl ->
                      Exp.case
                        (cstr_decl_to_ppat_construct cstr_decl)
                        (cstr_decl_to_pexp_apply cstr_decl))
                    cstr_decls)
             ]
         ]
    ))

let cstr_decls_to_handler_class ~options ~path name cstr_decls =
  (* Create a class named handler, with the method generated by
     cstr_decls_to_handler_class_main_method and one method generated
     by cstr_decl_to_handler_class_method for each cstr_decl. *)
  Pstr_class
    [Ast_helper.Ci.mk
       ~virt:Concrete
       (str (name_of_name_and_suffix name "handler"))
       (Cl.structure
          { pcstr_self = [%pat? self];
            pcstr_fields =
              (cstr_decls_to_handler_class_main_method ~options ~path name cstr_decls)
              :: List.map
                   (cstr_decl_to_handler_class_method ~options ~path)
                   cstr_decls })]