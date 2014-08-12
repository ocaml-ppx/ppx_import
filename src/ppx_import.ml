open Longident
open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper
open Types
open Ident

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
    let err = Location.error ?sub ?if_highlight ?loc str in
    raise (Location.Error err))

let replace_loc loc =
  { default_mapper with location = fun _ _ -> loc }

let locate_sig ~loc lid =
  let cu, path =
    match lid with
    | Lident _ -> !Location.input_name, []
    | Lapply _ -> assert false
    | Ldot (lid, _) ->
      match Longident.flatten lid with
      | cu :: path -> cu, path
      | _ -> assert false
  in
  let cmi_paths =
    !Config.load_path |>
    List.map (fun dir ->
      [Filename.concat dir (cu ^ ".cmi");
       Filename.concat dir ((String.uncapitalize cu) ^ ".cmi")]) |>
    List.flatten
  in
  let cmi =
    try
      cmi_paths |>
      List.find (fun intf ->
        Sys.file_exists intf) |>
      Cmi_format.read_cmi
    with Not_found ->
      raise_errorf ~loc "[%%import]: cannot locate module %s" cu
  in
  List.fold_left (fun sig_items path_item ->
      let rec loop sig_items =
        match sig_items with
        | Sig_module ({ name }, { md_type = Mty_signature sig_items }, _) :: _
              when name = path_item ->
          sig_items
        | _ :: sig_items ->
          loop sig_items
        | [] ->
          raise_errorf ~loc "[%%import]: cannot locate module %s"
                            (String.concat "." (cu :: path))
      in
      loop sig_items)
    cmi.Cmi_format.cmi_sign path

let locate_ttype_decl ~loc sig_items lid =
  let elem = Longident.last lid in
  let rec loop sig_items =
    match sig_items with
    | Sig_type ({ name }, type_decl, _) :: _ when name = elem -> type_decl
    | _ :: sig_items -> loop sig_items
    | [] -> raise_errorf ~loc "[%%import]: cannot locate type %s"
                              (String.concat "." (Longident.flatten lid))
  in
  loop sig_items

let rec longident_of_path path =
  match path with
  | Path.Pident { name } -> Lident name
  | Path.Pdot (path, name, _) -> Ldot (longident_of_path path, name)
  | Path.Papply (lhs, rhs) -> Lapply (longident_of_path lhs, longident_of_path rhs)

let rec core_type_of_type_expr ~subst type_expr =
  match type_expr.desc with
  | Tvar None -> Typ.any ()
  | Tvar (Some var) -> Typ.var var
  | Tarrow (label, lhs, rhs, _) ->
    Typ.arrow label (core_type_of_type_expr ~subst lhs)
                    (core_type_of_type_expr ~subst rhs)
  | Ttuple xs ->
    Typ.tuple (List.map (core_type_of_type_expr ~subst) xs)
  | Tconstr (path, args, _) ->
    let lid  = longident_of_path path in
    let args = (List.map (core_type_of_type_expr ~subst) args) in
    begin match List.assoc lid subst with
    | { ptyp_desc = Ptyp_constr (lid, _) } as typ ->
      { typ with ptyp_desc = Ptyp_constr (lid, args) }
    | _ -> assert false
    | exception Not_found ->
      Typ.constr { txt = longident_of_path path; loc = !default_loc } args
    end
  | Tvariant { row_fields; row_closed } ->
    let fields =
      row_fields |> List.map (fun (label, row_field) ->
        match row_field with
        | Rpresent None -> Rtag (label, [], true, [])
        | Rpresent (Some ttyp) ->
          Rtag (label, [], false, [core_type_of_type_expr ~subst ttyp])
        | _ -> assert false)
    in
    Typ.variant fields Closed None
  | _ ->
    assert false

let ptype_decl_of_ttype_decl ?manifest ~subst ptype_name ttype_decl =
  let ptype_params =
    List.map2 (fun param variance ->
        core_type_of_type_expr ~subst param,
        Invariant (* TODO *))
      ttype_decl.type_params ttype_decl.type_variance
  and ptype_kind =
    match ttype_decl.type_kind with
    | Type_abstract -> Ptype_abstract
    | Type_open -> Ptype_open
    | Type_record (labels, _) ->
      Ptype_record (labels |> List.map (fun ld ->
        { pld_name       = { txt = ld.ld_id.name; loc = ld.ld_loc };
          pld_mutable    = ld.ld_mutable;
          pld_type       = core_type_of_type_expr ~subst ld.ld_type;
          pld_loc        = ld.ld_loc;
          pld_attributes = ld.ld_attributes; }))
    | Type_variant constrs ->
      Ptype_variant (constrs |> List.map (fun cd ->
        { pcd_name       = { txt = cd.cd_id.name; loc = cd.cd_loc };
          pcd_args       = List.map (core_type_of_type_expr ~subst) cd.cd_args;
          pcd_res        = (match cd.cd_res with Some x -> Some (core_type_of_type_expr ~subst x)
                                               | None -> None);
          pcd_loc        = cd.cd_loc;
          pcd_attributes = cd.cd_attributes; }))
  and ptype_manifest =
    match ttype_decl.type_manifest with
    | Some typ -> Some (core_type_of_type_expr ~subst typ)
    | None -> manifest
  in
  { ptype_name; ptype_params; ptype_kind; ptype_manifest;
    ptype_cstrs      = [];
    ptype_private    = Public;
    ptype_attributes = ttype_decl.type_attributes;
    ptype_loc        = ttype_decl.type_loc; }

let subst_of_manifest { ptyp_attributes; ptyp_loc } =
  let rec subst_of_expr expr =
    match expr with
    | [%expr [%e? { pexp_desc = Pexp_ident ({ txt = src }) }] :=
             [%e? { pexp_desc = Pexp_ident (dst); pexp_attributes; pexp_loc }]] ->
      [src, { ptyp_loc = pexp_loc; ptyp_attributes = pexp_attributes;
              ptyp_desc = Ptyp_constr (dst, []) }]
    | [%expr [%e? { pexp_desc = Pexp_ident ({ txt = src }) }] :=
             [%e? { pexp_desc = Pexp_ident (dst); pexp_attributes; pexp_loc }]; [%e? rest]] ->
      (src, { ptyp_loc = pexp_loc; ptyp_attributes = pexp_attributes;
              ptyp_desc = Ptyp_constr (dst, []) }) :: subst_of_expr rest
    | { pexp_loc } ->
      raise_errorf ~loc:pexp_loc "Invalid [@with] syntax"
  in
  match Ast_convenience.find_attr "with" ptyp_attributes with
  | None -> []
  | Some (PStr [{ pstr_desc = Pstr_eval (expr, []) }]) ->
    subst_of_expr expr
  | Some _ ->
    raise_errorf ~loc:ptyp_loc "Invalid [@with] syntax"

let type_declaration mapper type_decl =
  match type_decl with
  | { ptype_attributes; ptype_name; ptype_manifest = Some {
        ptyp_desc = Ptyp_extension ({ txt = "import"; loc }, payload) } } ->
    begin match payload with
    | PTyp ({ ptyp_desc = Ptyp_constr ({ txt = lid; loc }, []) } as manifest) ->
      if Ast_mapper.tool_name () = "ocamldep" then
        (* Just put it as manifest *)
        { type_decl with ptype_manifest = Some manifest }
      else
        with_default_loc loc (fun () ->
          let subst = subst_of_manifest manifest in
          let subst = subst @ [Lident (Longident.last lid),
                               Typ.constr { txt = Lident ptype_name.txt; loc = ptype_name.loc } []] in
          let ttype_decl = locate_ttype_decl ~loc (locate_sig ~loc lid) lid in
          let ptype_decl = ptype_decl_of_ttype_decl ~manifest ~subst ptype_name ttype_decl in
          { ptype_decl with ptype_attributes })
    | _ -> raise_errorf ~loc "Invalid [%%import] syntax"
    end
  | _ -> default_mapper.type_declaration mapper type_decl

let () =
  Ast_mapper.register "ppx_import" (fun argv ->
    { default_mapper with
      type_declaration; })
