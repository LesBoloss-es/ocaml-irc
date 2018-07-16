open Asttypes
open Ast_helper
open Common
open Parsetree

let core_type_to_to_string = function
  | [%type: string] -> [%expr fun s -> s]
  | [%type: char] -> [%expr String.make 1]
  | [%type: int] -> [%expr string_of_int]
  | {ptyp_desc=Ptyp_constr(lid,_); _} ->
     Exp.ident (Location.mkloc (Ppx_deriving.mangle_lid (`Suffix "to_string") lid.txt) !Ast_helper.default_loc)
  | _ -> assert false

let cstr_strings ~options ~path cstr_decl =
  ignore (String.uppercase_ascii cstr_decl.pcd_name.txt);
  ignore options;
  ignore path;
  match cstr_decl.pcd_args with
  | Pcstr_tuple ctl ->
     let casts = List.map core_type_to_to_string ctl in
     let result =
       List.fold_right
         (fun cast (i, expr) ->
           (i-1,
            Exp.construct
              (ident "::")
              (Some
                 (Exp.tuple
                    [Exp.apply cast [Nolabel, Exp.ident (ident ("c"^(string_of_int i)))];
                     expr]))))
         casts
         (List.length casts - 1, [%expr []])
       |> snd
     in
     Exp.construct
       (ident "::")
       (Some (Exp.tuple [Exp.constant (Pconst_string (String.uppercase_ascii cstr_decl.pcd_name.txt, None)); result]))

  | _ -> assert false

let to_strings ~options ~path type_decl =
  (*
     let <name>_to_strings = function
       ...
       | C (c0, .., cn) -> ["C"; <c0>_to_string c0; ...; <cn>_to_string cn]
   *)
  Vb.mk
    (Pat.var (str (Ppx_deriving.mangle_type_decl (`Suffix "to_strings") type_decl)))
    (Exp.function_
       (List.map
          (fun cstr_decl ->
            Exp.case
              (cstr_pattern cstr_decl)
              (cstr_strings ~options ~path cstr_decl))
          (cstr_decls_of_type_decl type_decl)))
