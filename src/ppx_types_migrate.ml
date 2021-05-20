module At = Asttypes
module Pt = Parsetree
module Ot = Outcometree
open Migrate_parsetree
module Ab = Ast_411.Asttypes
module Pb = Ast_411.Parsetree
module Ob = Ast_411.Outcometree
module IMigrate = Convert (Versions.OCaml_current) (Versions.OCaml_411)

(* copy_mutable_flag / private_flag / arg_label are not exported by
   OMP so not worth the pain of the hack *)
let copy_mutable_flag (l : At.mutable_flag) : Ab.mutable_flag =
  match l with At.Immutable -> Ab.Immutable | At.Mutable -> Ab.Mutable

let copy_private_flag (l : At.private_flag) : Ab.private_flag =
  match l with At.Private -> Ab.Private | At.Public -> Ab.Public

let copy_arg_label (l : At.arg_label) : Ab.arg_label =
  match l with
  | At.Nolabel -> Ab.Nolabel
  | At.Labelled l -> Ab.Labelled l
  | At.Optional x -> Ab.Optional x

(* Here we want to do a hack due to the large type *)
let copy_attributes (attrs : Pt.attributes) : Pb.attributes =
  (* Hack *)
  let td = Ast_helper.Typ.any ~attrs () in
  let tb = IMigrate.copy_core_type td in
  tb.ptyp_attributes
