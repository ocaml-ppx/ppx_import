#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let ocamlbuild =
  "ocamlbuild -use-ocamlfind -classic-display -plugin-tag 'package(cppo_ocamlbuild)'"

let () =
  Pkg.describe "ppx_import" ~builder:(`Other (ocamlbuild, "_build")) [
    Pkg.lib "pkg/META";
    Pkg.lib "src/ppx_import.o";
    Pkg.lib "src/ppx_import.cmo";
    Pkg.lib "src/ppx_import.cmx";
    Pkg.lib "src/ppx_import.cma";
    Pkg.lib "src/ppx_import.cmxs";
    Pkg.libexec ~auto:true "src/ppx_import";
    Pkg.doc "README.md";
    Pkg.doc "LICENSE.txt";
    Pkg.doc "CHANGELOG.md"; ]
