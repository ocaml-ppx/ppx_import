[%%import: type t = Test_self_import.t]

module type%import S = Test_self_import.S

let validate_option = function
  | `OptionA -> assert true
  | `OptionB -> assert true
  | _ -> assert false

let validate_module_type m =
  let module M = (val m : S) in
  assert (M.test () = "test")
