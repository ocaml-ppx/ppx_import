open OUnit2

let test_longident ctxt =
  assert false

let suite = "Test ppx_import" >::: [
    "test_longident" >:: test_longident;
  ]

let _ =
  run_test_tt_main suite
