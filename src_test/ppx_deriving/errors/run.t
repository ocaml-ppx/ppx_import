  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (preprocess
  >   (staged_pps ppx_import)))
  > EOF

Functor error
  $ cat >test.ml <<EOF
  > [%%import: type t =  Map.Make(String).t]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 21-39:
  1 | [%%import: type t =  Map.Make(String).t]
                           ^^^^^^^^^^^^^^^^^^
  Error: [%import] cannot import a functor application Map.Make(String)
  [1]

Parameters error
  $ cat >test.ml <<EOF
  > [%%import: type t =  List.t]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 21-27:
  1 | [%%import: type t =  List.t]
                           ^^^^^^
  Error: Imported type has 1 parameter(s), but 0 are passed
  [1]

Abstract module error
  $ cat >stuff.ml <<EOF
  > module type T
  > EOF

  $ cat >test.ml <<EOF
  > module type T = [%import: (module Stuff.T)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 34-41:
  1 | module type T = [%import: (module Stuff.T)]
                                        ^^^^^^^
  Error: Imported module is abstract
  [1]

With error
  $ cat >test.ml <<EOF
  > [%%import: type p = (String.t[@with])]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 21-29:
  1 | [%%import: type p = (String.t[@with])]
                           ^^^^^^^^
  Error: Invalid [@with] syntax
  [1]

Cannot find signature error
  $ cat >test.ml <<EOF
  > [%%import: type t =  Map.K.t]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 21-28:
  1 | [%%import: type t =  Map.K.t]
                           ^^^^^^^
  Error: [%import]: cannot find the signature of K in Map
  [1]

Cannot find component error
  $ cat >test.ml <<EOF
  > [%%import: type t =  Map.Make.t]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 21-31:
  1 | [%%import: type t =  Map.Make.t]
                           ^^^^^^^^^^
  Error: [%import]: cannot find the components of Map.Make
  [1]

Cannot find module error
  $ cat >stuff.ml <<EOF
  > module type S = sig
  >  module M: sig end
  > end
  > EOF

  $ cat >test.ml <<EOF
  > module type A = [%import: (module Stuff.S.M)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 34-43:
  1 | module type A = [%import: (module Stuff.S.M)]
                                        ^^^^^^^^^
  Error: [%import]: cannot find the module type M in Stuff.S
  [1]

Multiple signature items
  $ cat >test.ml <<EOF
  > [%%import:
  > type b = int
  > type a = string]
  > EOF

OCaml 4.08 reports different numbers. 
It's been fixed for later versions in https://github.com/ocaml/ocaml/pull/8541
  $ dune build 2>&1 | sed -r 's/(line|character)s? [0-9]+(-[0-9]+)?/\1s %NUMBER%/g'
  File "test.ml", lines %NUMBER%, characters %NUMBER%:
  1 | [%%import:
  2 | type b = int
  3 | type a = string]
  Error: [] expected

Ptyp
  $ cat >test.ml <<EOF
  > [%%import: string]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 0-18:
  1 | [%%import: string]
      ^^^^^^^^^^^^^^^^^^
  Error: PSig expected
  [1]

Inline module type declaration
  $ cat >test.ml <<EOF
  > module type Hashable = [%import: (module sig type t end)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 41-55:
  1 | module type Hashable = [%import: (module sig type t end)]
                                               ^^^^^^^^^^^^^^
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]

Functor
  $ cat >test.ml <<EOF
  > module type Foo = [%import: (module functor (M : sig end) -> sig end)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 44-68:
  1 | module type Foo = [%import: (module functor (M : sig end) -> sig end)]
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]

Module type of
  $ cat >test.ml <<EOF
  > module type Example = [%import: (module type of A)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 40-44:
  1 | module type Example = [%import: (module type of A)]
                                              ^^^^
  Error: Syntax error
  [1]

Pmty_extension
  $ cat >test.ml <<EOF
  > module type M = [%import: [%extension]]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 26-38:
  1 | module type M = [%import: [%extension]]
                                ^^^^^^^^^^^^
  Error: package expected
  [1]

Pwith_module
  $ cat >test.ml <<EOF
  > module type StringHashable = sig
  >   type t = string
  >   val equal : t -> t -> bool
  >   val hash : t -> int
  > end
  > 
  > module StringHashable = struct
  >   type t = string
  >   let equal = (=)
  >   let hash = Hashtbl.hash
  > end
  > 
  > module type HashableWith = [%import: (module sig
  >   include module type of StringHashable
  > end with module StringHashable = StringHashable)]
  > EOF

  $ dune build
  File "test.ml", lines 13-15, characters 45-47:
  13 | .............................................sig
  14 |   include module type of StringHashable
  15 | end with module StringHashable = StringHashable..
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]

Pwith_modtype
  $ cat >test.ml <<EOF
  > module type StringHashable = sig
  >   type t = string
  >   val equal : t -> t -> bool
  >   val hash : t -> int
  > end
  > 
  > module StringHashable = struct
  >   type t = string
  >   let equal = (=)
  >   let hash = Hashtbl.hash
  > end
  > 
  > module type HashableWith = [%import: (module sig
  >   include module type of StringHashable
  > end with module type StringHashable = StringHashable)]
  > EOF

  $ dune build
  File "test.ml", lines 13-15, characters 45-52:
  13 | .............................................sig
  14 |   include module type of StringHashable
  15 | end with module type StringHashable = StringHashable..
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]

Pwith_typesubst
  $ cat >test.ml <<EOF
  > module type HashableWith = [%import: (module Hashtbl.HashedType with type t := string)]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 45-85:
  1 | module type HashableWith = [%import: (module Hashtbl.HashedType with type t := string)]
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: invalid package type: only 'with type t =' constraints are supported
  [1]

Pwith_modtypesubst
  $ cat >test.ml <<EOF
  > module type StringHashable = sig
  >   type t = string
  >   val equal : t -> t -> bool
  >   val hash : t -> int
  > end
  > 
  > module StringHashable = struct
  >   type t = string
  >   let equal = (=)
  >   let hash = Hashtbl.hash
  > end
  > 
  > module type HashableWith = [%import: (module sig
  >   include module type of StringHashable
  > end with module type StringHashable := StringHashable)]
  > EOF

  $ dune build
  File "test.ml", lines 13-15, characters 45-53:
  13 | .............................................sig
  14 |   include module type of StringHashable
  15 | end with module type StringHashable := StringHashable..
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]

Pwith_modsubst
  $ cat >test.ml <<EOF
  > module type StringHashable = sig
  >   type t = string
  >   val equal : t -> t -> bool
  >   val hash : t -> int
  > end
  > 
  > module StringHashable = struct
  >   type t = string
  >   let equal = (=)
  >   let hash = Hashtbl.hash
  > end
  > 
  > module type HashableWith = [%import: (module sig
  >   include module type of StringHashable
  > end with module StringHashable := StringHashable)]
  > EOF

  $ dune build
  File "test.ml", lines 13-15, characters 45-48:
  13 | .............................................sig
  14 |   include module type of StringHashable
  15 | end with module StringHashable := StringHashable..
  Error: invalid package type: only module type identifier and 'with type' constraints are supported
  [1]
