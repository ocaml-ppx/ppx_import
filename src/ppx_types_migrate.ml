module At = Asttypes
module Pt = Parsetree
module Ot = Outcometree

module Ab = Migrate_parsetree.Ast_406.Asttypes
module Pb = Migrate_parsetree.Ast_406.Parsetree
module Ob = Migrate_parsetree.Ast_406.Outcometree

module IMigrate = Migrate_parsetree.Convert(Migrate_parsetree.Versions.OCaml_current)(Migrate_parsetree.Versions.OCaml_406)

(* copy_mutable_flag / private_flag / arg_label are not exported by
   OMP so not worth the pain of the hack *)
let copy_mutable_flag (l : At.mutable_flag) : Ab.mutable_flag =
  match l with
  | At.Immutable -> Ab.Immutable
  | At.Mutable -> Ab.Mutable

let copy_private_flag (l : At.private_flag) : Ab.private_flag =
  match l with
  | At.Private -> Ab.Private
  | At.Public -> Ab.Public

let copy_arg_label (l : At.arg_label) : Ab.arg_label =
  match l with
  | At.Nolabel -> Ab.Nolabel
  | At.Labelled l -> Ab.Labelled l
  | At.Optional x -> Ab.Optional x

(* Here we want to do a hack due to the large type *)
let copy_attributes (l : Pt.attributes) : Pb.attributes =
  (* Hack *)
  let td = Pt.({ ptyp_desc = Ptyp_any;
                 ptyp_loc = Location.none;
                 ptyp_attributes = l;
               } ) in
  let td = IMigrate.copy_core_type td in
  td.ptyp_attributes
