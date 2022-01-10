type ('lbl, 'cstr) type_kind_407 = 
  | Type_abstract
  | Type_record of 'lbl list * Types.record_representation
  | Type_variant of 'cstr list
  | Type_open

let migrate_type_kind : ('lbl, 'cstr) Types.type_kind -> ('lbl, 'cstr) type_kind_407 = function
  | Type_abstract -> Type_abstract
  | Type_record (lbl, repr) -> Type_record (lbl, repr)
  | Type_variant (cstr, _) -> Type_variant cstr
  | Type_open -> Type_open
