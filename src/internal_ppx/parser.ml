
open Asttypes
open Ast_helper
open Common

let cstr_decls_to_value_binding ~options ~path name cstr_decls =
  ignore options;
  ignore path;
  Vb.mk
    (Pat.var (str (name_of_name_and_prefix_suffix "pp_print" (String.lowercase_ascii name) "")))
    (Exp.fun_
       Nolabel
       None
       (Pat.var (str "fmt"))
       (Exp.function_
          (List.map
             (fun cstr_decl ->
               Exp.case
                 (cstr_decl_to_ppat_construct cstr_decl)
                 (*FIXME: for now, we only print the name of the command. we need to print the rest also*)
                 (Exp.apply
                    [%expr Format.pp_print_string]
                    [(Nolabel, [%expr fmt]);
                     (Nolabel, Exp.constant (Const.string (String.uppercase_ascii cstr_decl.pcd_name.txt)))]))
             cstr_decls)))
