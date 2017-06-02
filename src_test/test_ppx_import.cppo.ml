open OUnit2

type a = [%import: Stuff.a]
type b = [%import: Stuff.b]
type c = [%import: Stuff.c]
type d = [%import: Stuff.d]
type e = [%import: Stuff.e]
type f = [%import: Stuff.S.f]
type 'a g = [%import: 'a Stuff.g]
type 'b g' = [%import: 'b Stuff.g]
type h = [%import: Stuff.h]
#if OCAML_VERSION >= (4, 03, 0)
module MI = Stuff.MI
type i = [%import: Stuff.i]
#endif

let test_constr ctxt =
  ignore ([A1; A2 "a"]);
  ignore (Stuff.A1 = A1);
  ignore { b1 = A1; b2 = "x"; b3 = Int64.zero };
  ignore ((`A) : c);
  ignore (Int64.zero : d);
  ignore (("a", 1) : e);
  ignore ((Succ (Zero)) : h);
#if OCAML_VERSION >= (4, 03, 0)
  ignore ((I 1) : i);
#endif
;;

type a' = [%import: Stuff.a] [@@deriving show]

let test_deriving ctxt =
  assert_equal ~printer:(fun x -> x)
               "(Stuff.A2 \"a\")" (show_a' (A2 "a"))

type longident = [%import: Longident.t] [@@deriving show]

type package_type =
[%import: Parsetree.package_type
          [@with core_type    := Parsetree.core_type [@printer Pprintast.core_type];
                 Asttypes.loc := Asttypes.loc [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt];
                 Longident.t  := Longident.t [@printer pp_longident]]]
[@@deriving show]

module type Hashable = [%import: (module Hashtbl.HashedType)]

type self_t = [%import: Test_self_import.t]

let test_self_import ctxt =
  let v : self_t = `OptionA
  in Test_self_import.validate_option v

module type Self_S = [%import: (module Test_self_import.S)]

module Self_M : Self_S = struct
  let test () = "test"
end

let test_self_import_module_type ctxt =
  let m = (module Self_M : Self_S)
  in Test_self_import.validate_module_type m

let suite = "Test ppx_import" >::: [
    "test_constr"                  >:: test_constr;
    "test_deriving"                >:: test_deriving;
    "test_self_import"             >:: test_self_import;
    "test_self_import_module_type" >:: test_self_import_module_type;
  ]

let _ =
  run_test_tt_main suite
