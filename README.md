[%%import]
==========

_import_ is a syntax extension that allows to pull in types or signatures from other compiled interface files.

Sponsored by [Evil Martians](http://evilmartians.com).

Installation
------------

_import_ can be installed via [OPAM](https://opam.ocaml.org):

    $ opam install ppx_import

Usage
-----

In order to use _import_, require the package `ppx_import`.

#### Using `ppx_import` from Dune

To use `ppx_import` from Dune you should use the [`staged_pps`](https://dune.readthedocs.io/en/latest/dune-files.html#preprocessing-specification) field to declare the preprocessing specification. Example:

```
(library
  (name foo)
  (preprocess (staged_pps ppx_import ppx_deriving.show))
```

Syntax
------

### Single declarations

For example:

``` ocaml
# type loc = [%import: Location.t];;
type loc = Location.t = { loc_start : Lexing.position; loc_end : Lexing.position; loc_ghost : bool; }
# module type Hashable = [%import: (module Hashtbl.HashedType)];;
module type Hashable = sig type t val equal : t -> t -> bool val hash : t -> int end
```

It is also possible to import items from your own .mli file.

### Combining with [@@deriving]

It's possible to combine _import_ and [_deriving_][deriving] to derive functions for types that you do not own, e.g.:

[deriving]: https://github.com/whitequark/ppx_deriving

``` ocaml
type longident = [%import: Longident.t] [@@deriving show]
let () =
  print_endline (show_longident (Longident.parse "Foo.Bar.baz"))
(* Longident.Ldot (Longident.Ldot (Longident.Lident ("Foo"), "Bar"), "baz") *)
```

Note that you need to require _import_ before any _deriving_ plugins, as otherwise _deriving_ will not be able to observe the complete type.

### [@with] replacements

It is possible to syntactically replace a type with another while importing a definition. This can be used to import only a few types from a group, or to attach attributes to selected referenced types.

For example, this snippet imports a single type from Parsetree and specifies a custom pretty-printer for _deriving show_.

``` ocaml
type package_type =
[%import: Parsetree.package_type
          [@with core_type    := Parsetree.core_type [@printer Pprintast.core_type];
                 Asttypes.loc := Asttypes.loc [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt];
                 Longident.t  := Longident.t [@printer pp_longident]]]
[@@deriving show]
```

For module types, the replacements are specified using the standard `with` construct. However, the replacement is still syntactic.

### More?

If you have a use case in mind that _ppx_import_ does not cover (in particular, object-oriented features are not implemented), please [open an issue](https://github.com/ocaml-ppx/ppx_import/issues/new).

License
-------

_import_ is distributed under the terms of [MIT license](LICENSE.md).
