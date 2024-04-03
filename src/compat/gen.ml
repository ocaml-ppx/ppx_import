let include_ path =
  let ic = open_in path in
  let size = in_channel_length ic in
  let s = really_input_string ic size in
  print_endline s; close_in ic

let make_version ~version f_prefix =
  let major, minor = version in
  let file = Format.asprintf "%s_ge_%1d%02d.ml" f_prefix major minor in
  Filename.concat "compat" file

(* List of versions that need special treatment, check is greater or
   equal than. Order is important! *)
let include_table =
  [ ("types_module_type", [(4, 10); (4, 8)])
  ; ("types_signature_item", [(4, 8)])
  ; ("types_type_kind", [(5, 2); (4, 13)])
  ; ("init_path", [(4, 9)])
  ; ("env_lookup", [(4, 10)])
  ; ("types_desc", [(4, 14)]) ]

let rec gen_compat real_version (f_prefix, version_list) =
  match version_list with
  | [] -> include_ (make_version ~version:(0, 0) f_prefix)
  | version :: vlist ->
    if real_version >= version then include_ (make_version ~version f_prefix)
    else gen_compat real_version (f_prefix, vlist)

let () =
  let version = Scanf.sscanf Sys.ocaml_version "%d.%d" (fun a b -> (a, b)) in
  List.iter (gen_compat version) include_table
