module At = Asttypes
module Conv = Ppxlib_ast.Select_ast (Ppxlib_ast.Compiler_version)
module To_ppxlib = Conv.Of_ocaml

(* copy_mutable_flag / private_flag / arg_label are not exported by
   OMP so not worth the pain of the hack *)
let copy_mutable_flag (l : At.mutable_flag) : Ppxlib.mutable_flag =
  match l with At.Immutable -> Ppxlib.Immutable | At.Mutable -> Ppxlib.Mutable

let copy_private_flag (l : At.private_flag) : Ppxlib.private_flag =
  match l with At.Private -> Ppxlib.Private | At.Public -> Ppxlib.Public

let copy_arg_label (l : At.arg_label) : Ppxlib.arg_label =
  match l with
  | At.Nolabel -> Ppxlib.Nolabel
  | At.Labelled l -> Ppxlib.Labelled l
  | At.Optional x -> Ppxlib.Optional x

(* Here we want to do a hack due to the large type *)
let copy_attributes (attrs : Parsetree.attributes) =
  let td = Ast_helper.Typ.any ~attrs () in
  let tb = To_ppxlib.copy_core_type td in
  tb.ptyp_attributes
