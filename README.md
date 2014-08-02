[%%import]
==========

_import_ is a syntax extension that allows to pull in types or signatures from other compiled interface files.

Installation
------------

_import_ can be installed via [OPAM](https://opam.ocaml.org):

    $ opam install ppx_import

Usage
-----

In order to use _import_, require the package `ppx_import`.

Syntax
------

Assume the following interface `stuff.mli` exists:

``` ocaml
type foo = A1 | A2 of string
and  bar = foo list
```

### Single declarations

The following two implementations are equivalent:

``` ocaml
type a = [%import: Stuff.foo]
```

``` ocaml
type a = Stuff.foo = A1 | A2 of string
```

### More?

_import_ is alpha-quality software. If you have an use case in mind that it does not cover, please [open an issue](https://github.com/whitequark/ppx_import/issues/new).

Advanced usage
--------------

It's possible to combine _import_ and [_deriving_][] to derive functions for types that you do not own, e.g.:

``` ocaml
type longident = [%import: Longident.t] [@@deriving Show]
let () =
  print_endline (show_longident (Longident.parse "Foo.Bar.baz"))
(* Longident.Ldot (Longident.Ldot (Longident.Lident ("Foo"), "Bar"), "baz") *)
```

License
-------

[MIT](LICENSE.txt)
