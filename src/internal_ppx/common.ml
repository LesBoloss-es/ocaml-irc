
open Parsetree
open Ast_helper

let str s =
  Location.mkloc s !Ast_helper.default_loc

let ident s =
  str (Longident.Lident s)

let rec list_ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: q -> list_ft q

let name_of_name_and_prefix_suffix prefix name suffix =
  match prefix, name, suffix with
  | "",   _, "" -> assert false
  | "", "t",  _ -> suffix
  | "",   _,  _ -> name ^ "_" ^ suffix
  |  _, "t", "" -> prefix
  |  _,   _, "" -> prefix ^ "_" ^ name
  |  _, "t",  _ -> prefix ^ "_" ^ suffix
  |  _,   _,  _ -> prefix ^ "_" ^ name ^ "_" ^ suffix

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
