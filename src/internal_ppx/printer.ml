open Asttypes
open Ast_helper
open Common

let to_strings ~options ~path type_decl =
  (*
     let pp_print_<name> = fun fmt -> function
       ...
       | C (c0, .., cn) -> ["C"; <c0>_to_string c0; ...; <cn>_to_string cn]
   *)
  Vb.mk
    (Pat.var (str (Ppx_deriving.mangle_type_decl (`Suffix "to_strings") type_decl)))
    (Exp.fun_
       Nolabel
       None
       (Pat.var (str "fmt"))
       (Exp.function_
          (List.map
             (fun cstr_decl ->
               Exp.case
                 (cstr_pattern cstr_decl)
                 (cstr_strings ~options ~path cstr_decl))
             (cstr_decls_of_type_decl type_decl))))
