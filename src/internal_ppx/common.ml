open Parsetree
open Ast_helper

let str s =
  Location.mkloc s !Ast_helper.default_loc

let ident s =
  str (Longident.Lident s)

(** assumes that the type declaration is a variant and return the list
   of constructor declarations in that variant. *)
let cstr_decls_of_type_decl type_decl =
  match type_decl.ptype_kind with
  | Ptype_variant cstr_decls ->
     cstr_decls
  | _ -> failwith "cstr_decl_of_type_decl"

let cstr_pattern cstr_decl =
  (* 
     C (c_0, .., c_n)
   *)
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
