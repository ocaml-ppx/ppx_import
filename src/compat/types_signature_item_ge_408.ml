type signature_item_407 =
  | Sig_value of Ident.t * Types.value_description
  | Sig_type of Ident.t * Types.type_declaration * Types.rec_status
  | Sig_typext of Ident.t * Types.extension_constructor * Types.ext_status
  | Sig_module of Ident.t * Types.module_declaration * Types.rec_status
  | Sig_modtype of Ident.t * Types.modtype_declaration
  | Sig_class of Ident.t * Types.class_declaration * Types.rec_status
  | Sig_class_type of Ident.t * Types.class_type_declaration * Types.rec_status

let migrate_signature_item : Types.signature_item -> signature_item_407 =
  function
  | Sig_value (id, vd, _) -> Sig_value (id, vd)
  | Sig_type (id, td, r, _) -> Sig_type (id, td, r)
  | Sig_typext (id, ec, es, _) -> Sig_typext (id, ec, es)
  | Sig_module (id, _, md, rs, _) -> Sig_module (id, md, rs)
  | Sig_modtype (id, td, _) -> Sig_modtype (id, td)
  | Sig_class (id, cd, rs, _) -> Sig_class (id, cd, rs)
  | Sig_class_type (id, ctd, rs, _) -> Sig_class_type (id, ctd, rs)
