module Tt = Ppx_types_migrate

type error = {loc : Location.t; error : string}

exception Error of error

let raise_error ~loc error = raise (Error {loc; error})

let lazy_env =
  lazy
    ( (* It is important that the typing environment is not evaluated
         right away, but only once the ppx-context has been loaded from
         the AST, so that Config.load_path and the rest of the environment
         context are correctly set.

         The environment setting should happen when reading the
         ppx-context attribute which is the very first structure/signature
         item sent to ppx rewriters. In particular, this happens before
         the [%import ] extensions are traversed, which are the places in
         this code where 'env' is forced.

         We would also have the option to not have a global environment, but
         recompute the typing environment on each [%import ] extension. We don't
         see any advantage in doing this, given that we compute the global/initial
         environment that is the same at all program points.
      *)
      (* We need to set recursive_types manually, because it is not part
         of the context automatically saved by Ast_mapper (as of 4.06),
         and this prevents loading the interface of recursive-types-using
         modules. On the other hand, setting recursive_types more often
         than necessary does not seem harmful. *)
      Ocaml_common.Clflags.recursive_types := true;
      Compat.init_path ();
      Ocaml_common.Compmisc.initial_env () )

let string_of_lid lid =
  let rec print lid acc =
    match lid with
    | Longident.Lident s -> s :: acc
    | Longident.Ldot (lid, id) -> print lid ("." :: id :: acc)
    | Longident.Lapply (la, lb) -> print la ("(" :: print lb (")" :: acc))
  in
  String.concat "" (print lid [])

let try_find_module ~loc env lid =
  (* Note: we are careful to call `Env.lookup_module` and not
     `Typetexp.lookup_module`, because we want to reason precisely
     about the possible failures: we want to handle the case where
     the module path does not exist, but let all the other errors
     (invalid .cmi format, etc.) bubble up to the error handler.

     `Env.lookup_module` allows to do this easily as it raises
     a well-identified `Not_found` exception, while
     `Typetexp.lookup_module` wraps the Not_found failure in
     user-oriented data and is not meant for catching.

     `Env.find_module` can raise `Not_found` again; we suspect that
     it will not in the cases where `lookup_module` returned correctly,
     but better be safe and bundle them in the same try..with.
  *)
  try
    let path = Compat.lookup_module ~loc lid env in
    let module_decl = Ocaml_common.Env.find_module path env in
    Some module_decl.md_type
  with Not_found -> None

let try_find_module_type ~loc env lid =
  (* Here again we prefer to handle the `Not_found` case, so we
     use `Env.lookup_module` rather than `Typetexp.lookup_module`. *)
  try
    let _path, modtype_decl = Ocaml_common.Env.lookup_modtype ~loc lid env in
    Some
      ( match modtype_decl.mtd_type with
      | None ->
        let error =
          Printf.sprintf
            "[%%import]: cannot access the signature of the abstract module %s"
            (string_of_lid lid)
        in
        raise_error ~loc error
      | Some module_type -> module_type )
  with Not_found -> None

let rec try_open_module_type env module_type =
  match Compat.migrate_module_type module_type with
  | Mty_signature sig_items -> Some sig_items
  | Mty_functor _ -> None
  | Mty_ident path | Mty_alias (_, path) -> (
    match
      try Some (Ocaml_common.Env.find_module path env) with Not_found -> None
    with
    | None -> None
    | Some module_decl -> try_open_module_type env module_decl.md_type )

let open_module_type ~loc env lid module_type =
  match try_open_module_type env module_type with
  | Some sig_items -> sig_items
  | None ->
    let error =
      Printf.sprintf "[%%import]: cannot find the components of %s"
        (string_of_lid lid)
    in
    raise_error ~loc error

let locate_sig ~loc env lid =
  let head, path =
    try
      match Ppxlib.Longident.flatten_exn lid with
      | head :: path -> (Longident.Lident head, path)
      | _ -> assert false
    with Invalid_argument _ ->
      let error =
        Printf.sprintf "[%%import] cannot import a functor application %s"
          (string_of_lid lid)
      in
      raise_error ~loc error
  in
  let head_module_type =
    match
      (try_find_module ~loc env head, lazy (try_find_module_type ~loc env head))
    with
    | Some mty, _ -> mty
    | None, (lazy (Some mty)) -> mty
    | None, (lazy None) ->
      let error =
        Printf.sprintf "[%%import]: cannot locate module %s" (string_of_lid lid)
      in
      raise_error ~loc error
  in
  let get_sub_module_type (lid, module_type) path_item =
    let sig_items = open_module_type ~loc env lid module_type in
    let rec loop sig_items =
      match (sig_items : Compat.signature_item_407 list) with
      | Sig_module (id, {md_type; _}, _) :: _ when Ident.name id = path_item ->
        md_type
      | Sig_modtype (id, {mtd_type = Some md_type; _}) :: _
        when Ident.name id = path_item ->
        md_type
      | _ :: sig_items -> loop sig_items
      | [] ->
        let error =
          Printf.sprintf "[%%import]: cannot find the signature of %s in %s"
            path_item (string_of_lid lid)
        in
        raise_error ~loc error
    in
    let sub_module_type =
      loop (List.map Compat.migrate_signature_item sig_items)
    in
    (Longident.Ldot (lid, path_item), sub_module_type)
  in
  let _lid, sub_module_type =
    List.fold_left get_sub_module_type (head, head_module_type) path
  in
  open_module_type ~loc env lid sub_module_type

let try_get_tsig_item f ~loc:_ sig_items elem =
  let rec loop sig_items =
    match sig_items with
    | item :: sig_items -> (
      match f elem item with Some x -> Some x | None -> loop sig_items )
    | [] -> None
  in
  loop sig_items

let get_type_decl ~loc sig_items parent_lid elem =
  let select_type elem sigi =
    match Compat.migrate_signature_item sigi with
    | Sig_type (id, type_decl, _) when Ident.name id = elem -> Some type_decl
    | _ -> None
  in
  match try_get_tsig_item select_type ~loc sig_items elem with
  | None ->
    let error =
      Printf.sprintf "[%%import]: cannot find the type %s in %s" elem
        (string_of_lid parent_lid)
    in
    raise_error ~loc error
  | Some decl -> decl

let get_modtype_decl ~loc sig_items parent_lid elem =
  let select_modtype elem sigi =
    match Compat.migrate_signature_item sigi with
    | Sig_modtype (id, type_decl) when Ident.name id = elem -> Some type_decl
    | _ -> None
  in
  match try_get_tsig_item select_modtype ~loc sig_items elem with
  | None ->
    let error =
      Printf.sprintf "[%%import]: cannot find the module type %s in %s" elem
        (string_of_lid parent_lid)
    in
    raise_error ~loc error
  | Some decl -> decl

let longident_of_path = Untypeast.lident_of_path

let mknoloc (txt : 'a) : 'a Ppxlib.Location.loc =
  {txt; loc = Ppxlib.Location.none}

let rec core_type_of_type_expr ~subst (type_expr : Ocaml_common.Types.type_expr)
    : Ppxlib.core_type =
  match Compat.get_desc type_expr with
  | Tvar None -> Ppxlib.Ast_helper.Typ.any ()
  | Tvar (Some var) -> (
    match List.assoc (`Var var) subst with
    | typ -> typ
    | exception Not_found -> Ppxlib.Ast_helper.Typ.var var )
  | Tarrow (label, lhs, rhs, _) ->
    let label = Tt.copy_arg_label label in
    let lhs = core_type_of_type_expr ~subst lhs in
    let lhs =
      match label with
      | Optional _ -> (
        match lhs with [%type: [%t? lhs] option] -> lhs | _ -> assert false )
      | _ -> lhs
    in
    Ppxlib.Ast_helper.Typ.arrow label lhs (core_type_of_type_expr ~subst rhs)
  | Ttuple xs ->
    Ppxlib.Ast_helper.Typ.tuple (List.map (core_type_of_type_expr ~subst) xs)
  | Tconstr (path, args, _) -> (
    let lid = longident_of_path path in
    let args = List.map (core_type_of_type_expr ~subst) args in
    match List.assoc (`Lid lid) subst with
    | {ptyp_desc = Ptyp_constr (lid, _); _} as typ ->
      {typ with ptyp_desc = Ptyp_constr (lid, args)}
    | _ -> assert false
    | exception Not_found ->
      Ppxlib.Ast_helper.Typ.constr
        {txt = longident_of_path path; loc = !Ppxlib.Ast_helper.default_loc}
        args )
  | Tvariant row_desc ->
    let fields =
      Compat.row_fields row_desc
      |> List.map (fun (label, row_field) ->
             let label = mknoloc label in
             let desc =
               match Compat.row_field_repr row_field with
               | Types.Rpresent None -> Ppxlib.Rtag (label, true, [])
               | Types.Rpresent (Some ttyp) ->
                 Ppxlib.Rtag (label, false, [core_type_of_type_expr ~subst ttyp])
               | _ -> assert false
             in
             Ppxlib.
               { prf_desc = desc
               ; prf_loc = !Ppxlib.Ast_helper.default_loc
               ; prf_attributes = [] } )
    in
    Ppxlib.Ast_helper.Typ.variant fields Closed None
  | _ -> assert false

let ptype_decl_of_ttype_decl ~manifest ~subst ptype_name
    (ttype_decl : Ocaml_common.Types.type_declaration) : Ppxlib.type_declaration
    =
  let subst =
    let open Ppxlib in
    match manifest with
    | Some {ptyp_desc = Ptyp_constr (_, ptype_args); ptyp_loc; _} -> (
      subst
      @
      try
        List.map2
          (fun (tparam : Ocaml_common.Types.type_expr) pparam ->
            match Compat.get_desc tparam with
            | Tvar (Some var) -> [(`Var var, pparam)]
            | Tvar None -> []
            | _ -> assert false )
          ttype_decl.type_params ptype_args
        |> List.concat
      with Invalid_argument _ ->
        let error =
          Printf.sprintf "Imported type has %d parameter(s), but %d are passed"
            (List.length ttype_decl.type_params)
            (List.length ptype_args)
        in
        raise_error ~loc:ptyp_loc error )
    | None -> []
    | _ -> assert false
  in
  let ptype_params =
    List.map2
      (fun param _variance ->
        ( core_type_of_type_expr ~subst param
        , (* The equivalent of not specifying the variance explicitly.
             Since the very purpose of ppx_import is to include the full definition,
             it should always be sufficient to rely on the inferencer to deduce variance. *)
          (Ppxlib.Asttypes.NoVariance, Ppxlib.Asttypes.NoInjectivity) ) )
      ttype_decl.type_params ttype_decl.type_variance
  and ptype_kind =
    let map_labels =
      List.map (fun (ld : Ocaml_common.Types.label_declaration) ->
          Ppxlib.
            { pld_name =
                {txt = Ocaml_common.Ident.name ld.ld_id; loc = ld.ld_loc}
            ; pld_mutable = Tt.copy_mutable_flag ld.ld_mutable
            ; pld_type = core_type_of_type_expr ~subst ld.ld_type
            ; pld_loc = ld.ld_loc
            ; pld_attributes = Tt.copy_attributes ld.ld_attributes } )
    in
    Ppxlib.(
      match Compat.migrate_type_kind ttype_decl.type_kind with
      | Type_abstract -> Ptype_abstract
      | Type_open -> Ptype_open
      | Type_record (labels, _) -> Ptype_record (map_labels labels)
      | Type_variant constrs ->
        let map_args (constrs : Ocaml_common.Types.constructor_arguments) =
          match constrs with
          | Cstr_tuple args ->
            Pcstr_tuple (List.map (core_type_of_type_expr ~subst) args)
          | Cstr_record labels -> Pcstr_record (map_labels labels)
        in
        Ptype_variant
          ( constrs
          |> List.map (fun (cd : Ocaml_common.Types.constructor_declaration) ->
                 let pcd_res =
                   match cd.cd_res with
                   | Some x -> Some (core_type_of_type_expr ~subst x)
                   | None -> None
                 in
                 { pcd_name =
                     {txt = Ocaml_common.Ident.name cd.cd_id; loc = cd.cd_loc}
                 ; pcd_vars = []
                 ; pcd_args = map_args cd.cd_args
                 ; pcd_res
                 ; pcd_loc = cd.cd_loc
                 ; pcd_attributes = Tt.copy_attributes cd.cd_attributes } ) ) )
  and ptype_manifest =
    match ttype_decl.type_manifest with
    | Some typ -> Some (core_type_of_type_expr ~subst typ)
    | None -> manifest
  in
  { ptype_name
  ; ptype_params
  ; ptype_kind
  ; ptype_manifest
  ; ptype_cstrs = []
  ; ptype_private = Tt.copy_private_flag ttype_decl.type_private
  ; ptype_attributes = Tt.copy_attributes ttype_decl.type_attributes
  ; ptype_loc = ttype_decl.type_loc }

let subst_of_manifest ({ptyp_attributes; ptyp_loc; _} : Ppxlib.core_type) =
  let open Ppxlib in
  let rec subst_of_expr expr =
    match expr with
    | [%expr
        [%e? {pexp_desc = Pexp_ident {txt = src; _}; _}]
        := [%e?
             { pexp_desc = Pexp_ident dst
             ; pexp_attributes
             ; pexp_loc
             ; pexp_loc_stack }]] ->
      [ ( `Lid src
        , { ptyp_loc = pexp_loc
          ; ptyp_loc_stack = pexp_loc_stack
          ; ptyp_attributes = pexp_attributes
          ; ptyp_desc = Ptyp_constr (dst, []) } ) ]
    | [%expr
        [%e? {pexp_desc = Pexp_ident {txt = src; _}; _}]
        := [%e?
             { pexp_desc = Pexp_ident dst
             ; pexp_attributes
             ; pexp_loc
             ; pexp_loc_stack }];
        [%e? rest]] ->
      ( `Lid src
      , { ptyp_loc = pexp_loc
        ; ptyp_loc_stack = pexp_loc_stack
        ; ptyp_attributes = pexp_attributes
        ; ptyp_desc = Ptyp_constr (dst, []) } )
      :: subst_of_expr rest
    | {pexp_loc; _} -> raise_error ~loc:pexp_loc "Invalid [@with] syntax"
  in
  let find_attr s attrs =
    try
      Some (List.find (fun {attr_name = x; _} -> x.txt = s) attrs).attr_payload
    with Not_found -> None
  in
  match find_attr "with" ptyp_attributes with
  | None -> []
  | Some (PStr [{pstr_desc = Pstr_eval (expr, []); _}]) -> subst_of_expr expr
  | Some _ -> raise_error ~loc:ptyp_loc "Invalid [@with] syntax"

let uncapitalize = String.uncapitalize_ascii

let is_self_reference ~input_name ~loc lid =
  let fn =
    input_name |> Filename.basename |> Filename.chop_extension |> uncapitalize
  in
  match lid with
  | Ppxlib.Ldot _ -> (
    try
      let mn = Ppxlib.Longident.flatten_exn lid |> List.hd |> uncapitalize in
      fn = mn
    with Invalid_argument _ ->
      let error =
        Printf.sprintf "[%%import] cannot import a functor application %s"
          (string_of_lid lid)
      in
      raise_error ~loc error )
  | _ -> false

let type_declaration ~tool_name ~input_name (type_decl : Ppxlib.type_declaration)
    =
  let open Ppxlib in
  match type_decl with
  | { ptype_attributes
    ; ptype_name
    ; ptype_manifest =
        Some ({ptyp_desc = Ptyp_constr ({txt = lid; loc}, _); _} as manifest)
    ; _ } -> (
    try
      if tool_name = "ocamldep" then
        (* Just put it as manifest *)
        if is_self_reference ~input_name ~loc lid then
          {type_decl with ptype_manifest = None}
        else {type_decl with ptype_manifest = Some manifest}
      else
        Ast_helper.with_default_loc loc (fun () ->
            let ttype_decl =
              let env = Lazy.force lazy_env in
              match lid with
              | Lapply _ ->
                let error =
                  Printf.sprintf
                    "[%%import] cannot import a functor application %s"
                    (string_of_lid lid)
                in
                raise_error ~loc error
              | Lident _ as head_id ->
                (* In this case, we know for sure that the user intends this lident
                   as a type name, so we use Typetexp.find_type and let the failure
                   cases propagate to the user. *)
                Compat.find_type env ~loc head_id |> snd
              | Ldot (parent_id, elem) ->
                let sig_items = locate_sig ~loc env parent_id in
                get_type_decl ~loc sig_items parent_id elem
            in
            let m, s =
              if is_self_reference ~input_name ~loc lid then (None, [])
              else
                let subst = subst_of_manifest manifest in
                let subst =
                  subst
                  @ [ ( `Lid (Lident (Longident.last_exn lid))
                      , Ast_helper.Typ.constr
                          {txt = Lident ptype_name.txt; loc = ptype_name.loc}
                          [] ) ]
                in
                (Some manifest, subst)
            in
            let ptype_decl =
              ptype_decl_of_ttype_decl ~manifest:m ~subst:s ptype_name
                ttype_decl
            in
            {ptype_decl with ptype_attributes} )
    with Error {loc; error} ->
      let ext = Ppxlib.Location.error_extensionf ~loc "%s" error in
      let core_type = Ast_builder.Default.ptyp_extension ~loc ext in
      {type_decl with ptype_manifest = Some core_type} )
  | _ -> type_decl

let rec cut_tsig_block_of_rec_types accu (tsig : Compat.signature_item_407 list)
    =
  match tsig with
  | Sig_type (id, ttype_decl, Trec_next) :: rest ->
    cut_tsig_block_of_rec_types ((id, ttype_decl) :: accu) rest
  | _ -> (List.rev accu, tsig)

let rec psig_of_tsig ~subst (tsig : Compat.signature_item_407 list) :
    Ppxlib.signature_item list =
  let open Ppxlib in
  match tsig with
  | Sig_type (id, ttype_decl, rec_flag) :: rest ->
    let accu = [(id, ttype_decl)] in
    let rec_flag, (block, rest) =
      match rec_flag with
      | Trec_not -> (Nonrecursive, (accu, rest))
      | Trec_first -> (Recursive, cut_tsig_block_of_rec_types accu rest)
      | Trec_next -> assert false
    in
    let block =
      block
      |> List.map (fun (id, ttype_decl) ->
             ptype_decl_of_ttype_decl ~manifest:None ~subst
               (mknoloc (Ocaml_common.Ident.name id))
               ttype_decl )
    in
    let psig_desc = Psig_type (rec_flag, block) in
    {psig_desc; psig_loc = Location.none} :: psig_of_tsig ~subst rest
  | Sig_value (id, {val_type; val_kind; val_loc; val_attributes; _}) :: rest ->
    let pval_prim =
      match val_kind with
      | Val_reg -> []
      | Val_prim p ->
        if p.prim_native_name <> "" then [p.prim_name; p.prim_native_name]
        else [p.prim_name]
      | _ -> assert false
    in
    { psig_desc =
        Psig_value
          { pval_name = mknoloc (Ocaml_common.Ident.name id)
          ; pval_loc = val_loc
          ; pval_attributes = Tt.copy_attributes val_attributes
          ; pval_prim
          ; pval_type = core_type_of_type_expr ~subst val_type }
    ; psig_loc = val_loc }
    :: psig_of_tsig ~subst rest
  | [] -> []
  | _ -> assert false

let subst_of_constraint (const : Ppxlib.with_constraint) =
  let open Ppxlib in
  match const with
  | Parsetree.Pwith_type (longident, type_decl) -> (
    match type_decl with
    | {ptype_manifest = Some core_type; _} -> (longident, core_type)
    | {ptype_loc; _} ->
      raise_error ~loc:ptype_loc "[%%import]: Not supported type_decl" )
  | Parsetree.Pwith_module ({loc; _}, _) ->
    raise_error ~loc "[%%import]: Pwith_module constraint is not supported."
  | Parsetree.Pwith_modtype ({loc; _}, _) ->
    raise_error ~loc "[%%import]: Pwith_modtype constraint is not supported."
  | Parsetree.Pwith_modtypesubst ({loc; _}, _) ->
    raise_error ~loc
      "[%%import]: Pwith_modtypesubst constraint is not supported."
  | Parsetree.Pwith_typesubst ({loc; _}, _) ->
    raise_error ~loc "[%%import]: Pwith_typesubst constraint is not supported."
  | Parsetree.Pwith_modsubst ({loc; _}, _) ->
    raise_error ~loc "[%%import]: Pwith_modsubst constraint is not supported."

let rec module_type ~tool_name ~input_name ?(subst = []) modtype =
  let open Ppxlib in
  let {pmty_desc; pmty_loc; _} = modtype in
  match pmty_desc with
  | Pmty_signature _ ->
    (* Ex: module type%import Hashable = sig ... end *)
    raise_error ~loc:pmty_loc
      "[%%import] inline module type declaration is not supported"
  | Pmty_with (modtype, constraints) ->
    let subst = constraints |> List.map subst_of_constraint in
    module_type ~tool_name ~input_name ~subst modtype
  | Pmty_functor (_, _) ->
    raise_error ~loc:pmty_loc "[%%import] module type doesn't support functor"
  | Pmty_typeof _ ->
    raise_error ~loc:pmty_loc "[%%import] module type doesn't support typeof"
  | Pmty_extension _ ->
    raise_error ~loc:pmty_loc "[%%import] module type doesn't support extension"
  | Pmty_alias _ ->
    raise_error ~loc:pmty_loc "[%%import] module type doesn't support alias"
  | Pmty_ident longident ->
    let {txt = lid; loc} = longident in
    if tool_name = "ocamldep" then
      if is_self_reference ~input_name ~loc lid then
        (* Create a dummy module type to break the circular dependency *)
        Ast_helper.Mty.mk ~attrs:[] (Pmty_signature [])
      else
        (* Just put it as alias *)
        Ast_helper.Mty.mk ~attrs:[] (Pmty_alias longident)
    else
      Ppxlib.Ast_helper.with_default_loc loc (fun () ->
          let env = Lazy.force lazy_env in
          let tmodtype_decl =
            match lid with
            | Longident.Lapply _ ->
              let error =
                Printf.sprintf
                  "[%%import] cannot import a functor application %s"
                  (string_of_lid lid)
              in
              raise_error ~loc error
            | Longident.Lident _ as head_id ->
              (* In this case, we know for sure that the user intends this lident
                 as a module type name, so we use Typetexp.find_type and
                 let the failure cases propagate to the user. *)
              Compat.find_modtype env ~loc head_id |> snd
            | Longident.Ldot (parent_id, elem) ->
              let sig_items = locate_sig ~loc env parent_id in
              get_modtype_decl ~loc sig_items parent_id elem
          in
          match tmodtype_decl with
          | {mtd_type = Some (Mty_signature tsig); _} ->
            let subst =
              List.map (fun ({txt; _}, typ) -> (`Lid txt, typ)) subst
            in
            let psig =
              psig_of_tsig ~subst (List.map Compat.migrate_signature_item tsig)
            in
            Ast_helper.Mty.mk ~attrs:[] (Pmty_signature psig)
          | {mtd_type = None; _} ->
            raise_error ~loc "Imported module is abstract"
          | _ -> raise_error ~loc "Imported module is indirectly defined" )

let module_type_decl ~tool_name ~input_name
    (modtype_decl : Ppxlib.module_type_declaration) =
  let open Ppxlib in
  try
    let {pmtd_type; pmtd_loc; _} = modtype_decl in
    match pmtd_type with
    | None ->
      (* when there's nothing after the equal sign. Ex: module type%import Hashable *)
      raise_error ~loc:pmtd_loc
        "[%%import] module type declaration is missing the module type \
         definition"
    | Some modtype -> module_type ~tool_name ~input_name modtype
  with Error {loc; error} ->
    let ext = Ppxlib.Location.error_extensionf ~loc "%s" error in
    Ast_builder.Default.pmty_extension ~loc ext

let type_declaration_expand ~ctxt rec_flag type_decls =
  let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
  let tool_name = Ppxlib.Expansion_context.Extension.tool_name ctxt in
  let input_name = Ppxlib.Expansion_context.Extension.input_name ctxt in
  let type_decls =
    type_decls |> List.map (type_declaration ~tool_name ~input_name)
  in
  Ppxlib.Ast_builder.Default.(pstr_type ~loc rec_flag type_decls)

let type_declaration_expand_intf ~ctxt rec_flag type_decls =
  let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
  let tool_name = Ppxlib.Expansion_context.Extension.tool_name ctxt in
  let input_name = Ppxlib.Expansion_context.Extension.input_name ctxt in
  let type_decls =
    type_decls |> List.map (type_declaration ~tool_name ~input_name)
  in
  Ppxlib.Ast_builder.Default.(psig_type ~loc rec_flag type_decls)

let module_declaration_expand ~ctxt modtype_decl =
  let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
  let tool_name = Ppxlib.Expansion_context.Extension.tool_name ctxt in
  let input_name = Ppxlib.Expansion_context.Extension.input_name ctxt in
  let modtype = module_type_decl ~tool_name ~input_name modtype_decl in
  let Ppxlib.{pmtd_name; pmtd_attributes; pmtd_loc; _} = modtype_decl in
  let md_decl =
    Ppxlib.Ast_helper.Mtd.mk ~loc:pmtd_loc ~attrs:pmtd_attributes pmtd_name
      ~typ:modtype
  in
  Ppxlib.{pstr_desc = Pstr_modtype md_decl; pstr_loc = loc}

let module_declaration_expand_intf ~ctxt modtype_decl =
  let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
  let tool_name = Ppxlib.Expansion_context.Extension.tool_name ctxt in
  let input_name = Ppxlib.Expansion_context.Extension.input_name ctxt in
  let modtype = module_type_decl ~tool_name ~input_name modtype_decl in
  let Ppxlib.{pmtd_name; pmtd_attributes; pmtd_loc; _} = modtype_decl in
  let md_decl =
    Ppxlib.Ast_helper.Mtd.mk ~loc:pmtd_loc ~attrs:pmtd_attributes pmtd_name
      ~typ:modtype
  in
  Ppxlib.{psig_desc = Psig_modtype md_decl; psig_loc = loc}

let type_declaration_expander ~ctxt payload =
  let return_error e =
    let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
    let ext = Ppxlib.Location.error_extensionf ~loc "%s" e in
    Ppxlib.Ast_builder.Default.pstr_extension ext [] ~loc
  in
  match payload with
  | Parsetree.PStr [{pstr_desc = Pstr_type (rec_flag, type_decls); _}]
   |Parsetree.PSig [{psig_desc = Psig_type (rec_flag, type_decls); _}] ->
    type_declaration_expand ~ctxt rec_flag type_decls
  | Parsetree.PStr [{pstr_desc = Pstr_modtype modtype_decl; _}]
   |Parsetree.PSig [{psig_desc = Psig_modtype modtype_decl; _}] ->
    module_declaration_expand ~ctxt modtype_decl
  | Parsetree.PStr [{pstr_desc = _; _}] | Parsetree.PSig [{psig_desc = _; _}] ->
    return_error
      "[%%import] Expected a type declaration or a module type declaration"
  | Parsetree.PStr (_ :: _) | Parsetree.PSig (_ :: _) ->
    return_error
      "[%%import] Expected exactly one item in the structure or signature, but \
       found multiple items"
  | Parsetree.PStr [] | Parsetree.PSig [] ->
    return_error
      "[%%import] Expected exactly one item in the structure or signature, but \
       found none"
  | Parsetree.PTyp _ ->
    return_error
      "[%%import] Type pattern (PTyp) is not supported, only type and module \
       type declarations are allowed"
  | Parsetree.PPat (_, _) ->
    return_error
      "[%%import] Pattern (PPat) is not supported, only type and module type \
       declarations are allowed"

let type_declaration_extension =
  Ppxlib.Extension.V3.declare "import" Ppxlib.Extension.Context.structure_item
    Ppxlib.Ast_pattern.(__)
    type_declaration_expander

let type_declaration_expander_intf ~ctxt payload =
  let return_error e =
    let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
    let ext = Ppxlib.Location.error_extensionf ~loc "%s" e in
    Ppxlib.Ast_builder.Default.psig_extension ext [] ~loc
  in
  match payload with
  | Parsetree.PStr [{pstr_desc = Pstr_type (rec_flag, type_decls); _}]
   |Parsetree.PSig [{psig_desc = Psig_type (rec_flag, type_decls); _}] ->
    type_declaration_expand_intf ~ctxt rec_flag type_decls
  | Parsetree.PStr [{pstr_desc = Pstr_modtype modtype_decl; _}]
   |Parsetree.PSig [{psig_desc = Psig_modtype modtype_decl; _}] ->
    module_declaration_expand_intf ~ctxt modtype_decl
  | Parsetree.PStr [{pstr_desc = _; _}] | Parsetree.PSig [{psig_desc = _; _}] ->
    return_error
      "[%%import] Expected a type declaration or a module type declaration"
  | Parsetree.PStr (_ :: _) | Parsetree.PSig (_ :: _) ->
    return_error
      "[%%import] Expected exactly one item in the structure or signature, but \
       found multiple items"
  | Parsetree.PStr [] | Parsetree.PSig [] ->
    return_error
      "[%%import] Expected exactly one item in the structure or signature, but \
       found none"
  | Parsetree.PTyp _ ->
    return_error
      "[%%import] Type pattern (PTyp) is not supported, only type and module \
       type declarations are allowed"
  | Parsetree.PPat (_, _) ->
    return_error
      "[%%import] Pattern (PPat) is not supported, only type and module type \
       declarations are allowed"

let type_declaration_extension_intf =
  Ppxlib.Extension.V3.declare "import" Ppxlib.Extension.Context.signature_item
    Ppxlib.Ast_pattern.(__)
    type_declaration_expander_intf

let type_declaration_rule =
  Ppxlib.Context_free.Rule.extension type_declaration_extension

let type_declaration_rule_intf =
  Ppxlib.Context_free.Rule.extension type_declaration_extension_intf

let () =
  Ppxlib.Driver.V2.register_transformation
    ~rules:[type_declaration_rule; type_declaration_rule_intf]
    "ppx_import"
