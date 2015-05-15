#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ppx_import" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.libexec ~auto:true "src/ppx_import";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
