open Parsetree
open Asttypes
open Ast_helper
open Common

let handler_name_of_name name =
  "on_" ^ (String.lowercase_ascii name)

let cstr_vars cstr_decl =
  match cstr_decl.pcd_args with
  | Pcstr_tuple ctl -> List.mapi (fun i _ -> "c"^(string_of_int i)) ctl
  | _ -> assert false

let cstr_apply cstr_decl =
  (*
     self#on_C env x_1 ... x_n
   *)
  let args =
    match cstr_decl.pcd_args with
    | Pcstr_tuple [] ->
       [Nolabel, [%expr ()]]
    | Pcstr_tuple _ ->
       List.map
         (fun id -> Nolabel, Exp.ident (ident id))
         ("env" :: cstr_vars cstr_decl)
    | _ -> assert false
  in
  Exp.apply
    (Exp.send
       (Exp.ident (ident "self"))
       (str (handler_name_of_name cstr_decl.pcd_name.txt)))
    args

let cstr_method ~options ~path cstr_decl =
  (*
     method on_C env x_1 ... x_n =
       ()
   *)
  ignore options; ignore path;
  Cf.method_
    (str (handler_name_of_name cstr_decl.pcd_name.txt))
    Public
    (Cfk_concrete
       (Fresh,
        Exp.fun_
          Nolabel
          None
          (Pat.var (str "env"))
          (List.fold_right
             (fun var rhs ->
               Exp.fun_ Nolabel None (Pat.var (str var)) rhs)
             (cstr_vars cstr_decl)
             [%expr ()])))

let main_method ~options ~path type_decl =
  (*
     method on_<name> = fun (env: 'a) -> function
       ...
       | C (x_1, .., x_n) -> self#on_C env x_1 ... x_n
   *)
  ignore options; ignore path;
  Cf.method_
    (str (Ppx_deriving.mangle_type_decl (`Prefix "on") type_decl))
    Public
    (Cfk_concrete (
         Fresh,
         Exp.fun_
           Nolabel
           None
           (Pat.constraint_ (Pat.var (str "env")) (Typ.var "'a"))
           (Exp.function_
              (List.map
                 (fun cstr_decl ->
                   Exp.case
                     (cstr_pattern cstr_decl)
                     (cstr_apply cstr_decl))
                 (cstr_decls_of_type_decl type_decl)))))

let handler ~options ~path type_decl =
  (*
     class ['a] <name>_handler = object (self)

       method on_C
       (* for each constructor *)

       method on_<name>
     end
   *)
  Pstr_class
    [Ci.mk
       ~virt:Concrete
       ~params:[Typ.var "'a", Invariant]
       (str (Ppx_deriving.mangle_type_decl (`Suffix "handler") type_decl))
       (Cl.structure
          { pcstr_self = [%pat? self];
            pcstr_fields =
              (main_method ~options ~path type_decl)
              :: List.map
                   (cstr_method ~options ~path)
                   (cstr_decls_of_type_decl type_decl) })]
