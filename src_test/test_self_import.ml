type t = [%import: Test_self_import.t]

let validate_option = function
  | `OptionA -> assert true
  | `OptionB -> assert true
  | _ -> assert false
