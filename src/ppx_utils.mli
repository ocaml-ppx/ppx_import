val raise_errorf :
  ?sub:Location.error list ->
  ?if_highlight:string ->
  ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a
val replace_loc : Location.t -> Ast_mapper.mapper
val locate_sig : loc:Location.t -> Longident.t -> Types.signature
val locate_tsig_item :
  (string -> 'a -> 'b option) ->
  loc:Location.t -> 'a list -> Longident.t -> 'b
val locate_ttype_decl :
  loc:Location.t ->
  Types.signature_item list -> Longident.t -> Types.type_declaration
val locate_tmodtype_decl :
  loc:Location.t ->
  Types.signature_item list -> Longident.t -> Types.modtype_declaration
val longident_of_path : Path.t -> Longident.t
val core_type_of_type_expr :
  subst:([> `Lid of Longident.t | `Var of string ] * Parsetree.core_type)
        list ->
  Types.type_expr -> Parsetree.core_type
val ptype_decl_of_ttype_decl :
  manifest:Parsetree.core_type option ->
  subst:([> `Lid of Longident.t | `Var of string ] * Parsetree.core_type)
        list ->
  string Asttypes.loc -> Types.type_declaration -> Parsetree.type_declaration
val subst_of_manifest :
  Parsetree.core_type ->
  ([> `Lid of Longident.t ] * Parsetree.core_type) list
val is_self_reference : Longident.t -> bool
val psig_of_tsig :
  subst:([> `Lid of Longident.t | `Var of string ] * Parsetree.core_type)
        list ->
  ?trec:Parsetree.type_declaration list ->
  Types.signature_item list -> Parsetree.signature_item list
