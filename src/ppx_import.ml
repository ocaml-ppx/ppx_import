open Migrate_parsetree
open Ast_403

open Longident
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

module Ast_current = Versions.OCaml_current.Ast

let ocaml_version = Versions.ocaml_403
let from_current = Versions.migrate Versions.ocaml_current ocaml_version
let to_current = Versions.migrate ocaml_version Versions.ocaml_current

let type_declaration config mapper type_decl =
  match type_decl with
  | { ptype_attributes; ptype_name; ptype_manifest = Some {
        ptyp_desc = Ptyp_extension ({ txt = "import"; loc }, payload) } } ->
    begin match payload with
    | PTyp ({ ptyp_desc = Ptyp_constr ({ txt = lid; loc }, _) } as manifest) ->
      if config.Driver.tool_name = "ocamldep" then
        (* Just put it as manifest *)
        if Ppx_utils.is_self_reference lid then
          { type_decl with ptype_manifest = None }
        else
          { type_decl with ptype_manifest = Some manifest }
      else
        with_default_loc loc (fun () ->
          let ttype_decl = Ppx_utils.locate_ttype_decl ~loc (Ppx_utils.locate_sig ~loc lid) lid in
          let m, s = if Ppx_utils.is_self_reference lid then
              None, []
          else begin
            let manifest = to_current.Versions.copy_core_type manifest in
            let subst = Ppx_utils.subst_of_manifest manifest
            in
            let subst = subst @ Ast_current.([
                `Lid (Lident (Longident.last lid)),
                Ast_helper.Typ.constr { txt = Lident ptype_name.txt; loc = ptype_name.loc } []
              ])
            in
            Some manifest, subst
          end
          in
          let ptype_decl =
            Ppx_utils.ptype_decl_of_ttype_decl ~manifest:m ~subst:s ptype_name ttype_decl
            |> from_current.Versions.copy_type_declaration
          in
          { ptype_decl with ptype_attributes })
    | _ -> Ppx_utils.raise_errorf ~loc "Invalid [%%import] syntax"
    end
  | _ -> default_mapper.type_declaration mapper type_decl

let module_type config mapper modtype_decl =
  match modtype_decl with
  | { pmty_attributes; pmty_desc = Pmty_extension ({ txt = "import"; loc }, payload) } ->
    begin match payload with
    | PTyp ({ ptyp_desc = Ptyp_package({ txt = lid; loc } as alias, subst) }) ->
      if config.Driver.tool_name = "ocamldep" then
        if Ppx_utils.is_self_reference lid then
          (* Create a dummy module type to break the circular dependency *)
          { modtype_decl with pmty_desc = Pmty_signature [] }
        else
          (* Just put it as alias *)
          { modtype_decl with pmty_desc = Pmty_alias alias }
      else
        with_default_loc loc (fun () ->
          let subst = List.map (fun (a, b) -> (a, to_current.Versions.copy_core_type b)) subst
          in
          match Ppx_utils.locate_tmodtype_decl ~loc (Ppx_utils.locate_sig ~loc lid) lid with
          | { Types.mtd_type = Some (Types.Mty_signature tsig) } ->
            let subst = List.map (fun ({ txt; }, typ) -> `Lid txt, typ) subst in
            let psig  =
              Ppx_utils.psig_of_tsig ~subst tsig
              |> from_current.Versions.copy_signature
            in
            { modtype_decl with pmty_desc = Pmty_signature psig }
          | { Types.mtd_type = None } ->
            Ppx_utils.raise_errorf ~loc "Imported module is abstract"
          | _ ->
            Ppx_utils.raise_errorf ~loc "Imported module is indirectly defined")
    | _ -> Ppx_utils.raise_errorf ~loc "Invalid [%%import] syntax"
    end
  | _ -> default_mapper.module_type mapper modtype_decl

let rewriter config cookies = {
  Ast_mapper.default_mapper with
  module_type      = module_type config;
  type_declaration = type_declaration config;
}

let () =
  Driver.register ~name:"ppx_import" ~args:[] Versions.ocaml_403 rewriter;
  Driver.run_main ()
;;
