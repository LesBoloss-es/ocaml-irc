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

let cstr_name ~options ~path cstr_decl =
  (* If there is a constructor attribute [@repr] carrying a string, we
     use that. If not, we use the uppercase of the constructor
     name. *)
  ignore options; ignore path;
  let name =
    match Ppx_deriving.attr ~deriver "repr" cstr_decl.pcd_attributes with
    | None -> String.uppercase_ascii cstr_decl.pcd_name.txt
    | Some (_, PStr [{pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant (Pconst_string (name, None)); _}, []); _}]) -> name
    | _ -> assert false
  in
  Exp.constant (Pconst_string (name, None))
  
let cstr_strings ~options ~path cstr_decl =
  ignore options;
  ignore path;
  match cstr_decl.pcd_args with
  | Pcstr_tuple ctl ->
     (
       (* If there is a constructor attribute [@to_strings] carrying
          an expression, we apply this expression to the arguments of
          the constructor. *)
       let args =
         match Ppx_deriving.attr ~deriver "to_strings" cstr_decl.pcd_attributes with
         | None ->
            (
              let casts = List.map core_type_to_to_string ctl in
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
            )
         | Some (_, PStr [{pstr_desc=Pstr_eval (exp, []); _}]) ->
            (
              Exp.apply
                exp
                (List.mapi
                   (fun i _ ->
                     (Nolabel, Exp.ident (ident ("c"^(string_of_int i)))))
                   ctl)
            )
         | _ -> assert false
       in
       Exp.construct
         (ident "::")
         (Some (Exp.tuple [cstr_name ~options ~path cstr_decl; args]))
     )
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

let pp_print ~options ~path type_decl =
  ignore options; ignore path; ignore type_decl;
  Vb.mk
    (Pat.var (str (Ppx_deriving.mangle_type_decl (`Prefix "pp_print") type_decl)))
    [%expr fun fmt command ->
        match [%e Exp.ident (ident (Ppx_deriving.mangle_type_decl (`Suffix "to_strings") type_decl))] command with
        | [] -> assert false
        | name :: args ->
           let rec print_args = function
             | [] -> ()
             | [last] ->
                Format.fprintf fmt " :%s" last
             | arg :: args ->
                assert (String.index_opt arg ' ' = None);
                Format.fprintf fmt " %s" arg;
                print_args args
           in
           assert (String.index_opt name ' ' = None);
           Format.fprintf fmt "%s" name;
           print_args args]

let all ~options ~path type_decl =
  [to_strings ~options ~path type_decl;
   pp_print ~options ~path type_decl]
  |> List.map (fun vb -> Str.value Nonrecursive [vb])
