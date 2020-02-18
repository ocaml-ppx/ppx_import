let include_ path =
  let ic = open_in path in
  let size = in_channel_length ic in
  let s = really_input_string ic size in
  print_endline s;
  close_in ic

let () =
  let version = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) in
  if version >= (4, 8) then
    include_ "compat/types_ge_408.ml"
  else
    include_ "compat/types_lt_408.ml";
  if version >= (4, 9) then
    include_ "compat/init_path_ge_409.ml"
  else
    include_ "compat/init_path_lt_409.ml"
