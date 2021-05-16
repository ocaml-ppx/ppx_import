unreleased
-----
  * drop support for OCaml `4.04.2`. Minimal supported version is now `4.05.0` #54 (tatchi)
  
  * migrate to ppxlib #54 (tatchi)

  * bump minimum dune version to 1.11 #56 (tatchi)

  * update CI to test OCaml 4.12.0, no changes required
    (#53, Emilio J. Gallego Arias)

  * remove the OCaml upper bound in the opam file
    (#53, Emilio J. Gallego Arias, kit-ty-kate)

1.8.0
-----

  * Upgrade the internal AST from 4.07 to 4.11
    #52
    (Gabriel Scherer, review by Emilio J. Gallego Arias)

  * Update lower bound for `ppx_tools_versioned` and
    `ocaml-migrate-parsetree` to 4.11 capable versions
    (Emilio J. Gallego Arias)

1.7.2
-----

  * Remove a warning in OCaml 4.11
    #49
    (Kate Deplaix)

1.7.1
-----

  * Support for OCaml 4.10
    #47
    (Emilio J. Gallego Arias)

1.7.0
-----

  * OCaml 4.08 and 4.09 support
    #46
    (Etienne Millon)

1.6.2
-----

  * Fix import of module types with optional arguments
    (Thierry Martinez #37, review by whitequark)

1.6.1
-----

  * Fix import of signatures with mutually recursive types
    (Thierry Martinez #35, review and tweaks by Gabriel Scherer)

1.6
---

  * ocaml-migrate-parsetree + dune support #26
    (Jérémie Dimino & Emilio Jesús Gallego Arias)

  * Move to OPAM 2.0, adapt Travis CI.
    (Emilio Jesús Gallego Arias)

1.5
---

  * OCaml 4.07 support
    #24
    (Damien Doligez)

  * Call the type-checker (through compiler-libs) instead of reading
    `.cmi` files directly, to correctly resolve module aliases.
    #25
    (Gabriel Scherer)

1.4
---

  * OCaml 4.06 support
    #19
    (Gabriel Scherer)

1.3
---

  * Also allow extraction of module types from the current module's interface file.
    #12
    (Xavier Guérin)

1.2
---

  * Allow extracting types from the current module's interface file.
    (Xavier Guérin)

1.1
---

  * OCaml 4.03 support.
    (whitequark)

1.0
---

  * Allow extracting types from module types.
    (whitequark)

0.1
---

  * Initial release.
    (whitequark)
