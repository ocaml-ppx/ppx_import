open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "compile"; "safe_string"] & A"-safe-string";
    flag ["ocaml"; "compile"; "use_import"] &
      S[A"-ppx"; A"src/ppx_import.byte"];

  | _ -> ())
