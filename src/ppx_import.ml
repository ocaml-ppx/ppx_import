open Longident
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
    let err = Location.error ?sub ?if_highlight ?loc str in
    raise (Location.Error err))

let replace_loc loc =
  { default_mapper with location = fun _ _ -> loc }

let import_type_decl lid =
  let _cu, _path, _typ =
    match lid with
    | Lident typ -> !Location.input_name, [], typ
    | Lapply _ -> assert false
    | Ldot (lid, typ) ->
      match Longident.flatten lid with
      | cu :: path -> cu, path, typ
      | _ -> assert false
  in
  assert false

let type_declaration mapper type_decl =
  match type_decl with
  | { ptype_attributes; ptype_manifest = Some {
        ptyp_desc = Ptyp_extension ({ txt = "import"; loc }, payload) } } ->
    begin match payload with
    | PTyp { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, []) } ->
      let loc_mapper = replace_loc loc in
      { (loc_mapper.type_declaration loc_mapper (import_type_decl lid)) with
        ptype_attributes }
    | _ -> raise_errorf ~loc "Invalid [%%import] syntax"
    end
  | _ -> default_mapper.type_declaration mapper type_decl

let () =
  Ast_mapper.register "ppx_import" (fun argv ->
    { default_mapper with
      type_declaration; })
