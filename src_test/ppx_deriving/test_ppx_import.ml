open OUnit2

[%%import type a = Stuff.a]
type%import b = Stuff.b

type%import c = Stuff.c
and d = Stuff.d

[%%import: type e = Stuff.e]
[%%import: type f = Stuff.S.f]
[%%import: type 'a g = 'a Stuff.g]
[%%import: type 'b g' = 'b Stuff.g]
[%%import: type h = Stuff.h]

module MI = Stuff.MI

[%%import: type i = Stuff.i]

module type%import S_rec = Stuff.S_rec

let test_constr _ctxt =
  ignore [A1; A2 "a"];
  ignore (Stuff.A1 = A1);
  ignore (Test_intf.A1 = A1);
  ignore {b1 = A1; b2 = "x"; b3 = Int64.zero};
  ignore (`A : c);
  ignore (Int64.zero : d);
  ignore (("a", 1) : e);
  ignore (Succ Zero : h);
  ignore (I 1 : i)

[%%import: type a' = Stuff.a [@@deriving show]]

let test_deriving _ctxt =
  assert_equal ~printer:(fun x -> x) "(Stuff.A2 \"a\")" (show_a' (A2 "a"))

module type%import S_optional = Stuff.S_optional

module Test_optional : S_optional = struct
  let f ?(opt = 0) () = ignore opt
end

[%%import: type longident = Longident.t [@@deriving show]]

[%%import:
type package_type =
  (Parsetree.package_type
  [@with
    core_type := (Parsetree.core_type [@printer Pprintast.core_type]);
    Asttypes.loc :=
      (Asttypes.loc [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt]);
    Longident.t := (Longident.t [@printer pp_longident])] )
[@@deriving show]]

module type%import Hashable = Hashtbl.HashedType
module type%import HashableWith = Hashtbl.HashedType with type t = string

module HashableWith : HashableWith = struct
  type t

  let equal = String.equal
  let hash = int_of_string
end

[%%import: type self_t = Test_self_import.t]

let test_self_import _ctxt =
  let v : self_t = `OptionA in
  Test_self_import.validate_option v

module type%import Self_S = Test_self_import.S

module Self_M : Self_S = struct
  let test () = "test"
end

let test_self_import_module_type _ctxt =
  let m = (module Self_M : Self_S) in
  Test_self_import.validate_module_type m

let suite =
  "Test ppx_import"
  >::: [ "test_constr" >:: test_constr
       ; "test_deriving" >:: test_deriving
       ; "test_self_import" >:: test_self_import
       ; "test_self_import_module_type" >:: test_self_import_module_type ]

let _ = run_test_tt_main suite
