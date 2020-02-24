type module_type_407 =
  | Mty_ident of Path.t
  | Mty_signature of Types.signature
  | Mty_functor of Ident.t * Types.module_type option * Types.module_type
  | Mty_alias of unit * Path.t

let migrate_module_type : Types.module_type -> module_type_407 = function
  | Mty_ident p -> Mty_ident p
  | Mty_signature s -> Mty_signature s
  | Mty_functor (i, mto, mt) -> Mty_functor (i, mto, mt)
  | Mty_alias p -> Mty_alias ((), p)
