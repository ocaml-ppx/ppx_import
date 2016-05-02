open Ocamlbuild_plugin

let () = dispatch (fun phase ->
  Ocamlbuild_cppo.dispatcher phase;
  match phase with
  | After_rules ->
    flag ["ocaml"; "ocamldep"; "use_import"] & S[A"-ppx"; A"src/ppx_import.native"];
    flag ["ocaml"; "compile";  "use_import"] & S[A"-ppx"; A"src/ppx_import.native"]
  | _ -> ())
