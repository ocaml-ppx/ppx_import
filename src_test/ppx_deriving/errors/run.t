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
  > module type%import T = Stuff.T
  > EOF

  $ dune build
  File "test.ml", line 1, characters 23-30:
  1 | module type%import T = Stuff.T
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
  > module type%import A = Stuff.S.M
  > EOF

  $ dune build
  File "test.ml", line 1, characters 23-32:
  1 | module type%import A = Stuff.S.M
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
  Error: [%%import] Expected exactly one item in the structure or signature,
         but found multiple items

Ptyp
  $ cat >test.ml <<EOF
  > [%%import: string]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 0-18:
  1 | [%%import: string]
      ^^^^^^^^^^^^^^^^^^
  Error: [%%import] Type pattern (PTyp) is not supported, only type and module
         type declarations are allowed
  [1]

Inline module type declaration
  $ cat >test.ml <<EOF
  > module type%import Hashable = sig type t end
  > EOF

  $ dune build
  File "test.ml", line 1, characters 30-44:
  1 | module type%import Hashable = sig type t end
                                    ^^^^^^^^^^^^^^
  Error: [%%import] inline module type declaration is not supported
  [1]

Functor
  $ cat >test.ml <<EOF
  > module type%import Foo = functor (M : sig end) -> sig end
  > EOF

  $ dune build
  File "test.ml", line 1, characters 33-57:
  1 | module type%import Foo = functor (M : sig end) -> sig end
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: [%%import] module type doesn't support functor
  [1]

Module type of
  $ cat >test.ml <<EOF
  > module type%import Example = module type of A
  > EOF

  $ dune build
  File "test.ml", line 1, characters 29-45:
  1 | module type%import Example = module type of A
                                   ^^^^^^^^^^^^^^^^
  Error: [%%import] module type doesn't support typeof
  [1]

Pmty_extension
  $ cat >test.ml <<EOF
  > module type%import M = [%extension]
  > EOF

  $ dune build
  File "test.ml", line 1, characters 23-35:
  1 | module type%import M = [%extension]
                             ^^^^^^^^^^^^
  Error: [%%import] module type doesn't support extension
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
  > module type%import HashableWith = sig
  >   include module type of StringHashable
  > end with module StringHashable = StringHashable
  > EOF

  $ dune build
  File "test.ml", line 15, characters 16-30:
  15 | end with module StringHashable = StringHashable
                       ^^^^^^^^^^^^^^
  Error: [%%import]: Pwith_module constraint is not supported.
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
  > module type%import HashableWith = sig
  >   include module type of StringHashable
  > end with module type StringHashable = StringHashable
  > EOF

  $ dune build
  File "test.ml", line 15, characters 21-35:
  15 | end with module type StringHashable = StringHashable
                            ^^^^^^^^^^^^^^
  Error: [%%import]: Pwith_modtype constraint is not supported.
  [1]

Pwith_typesubst
  $ cat >test.ml <<EOF
  > module type%import HashableWith = Hashtbl.HashedType with type t := string
  > EOF

  $ dune build
  File "test.ml", line 1, characters 63-64:
  1 | module type%import HashableWith = Hashtbl.HashedType with type t := string
                                                                     ^
  Error: [%%import]: Pwith_typesubst constraint is not supported.
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
  > module type%import HashableWith = sig
  >   include module type of StringHashable
  > end with module type StringHashable := StringHashable
  > EOF

  $ dune build
  File "test.ml", line 15, characters 21-35:
  15 | end with module type StringHashable := StringHashable
                            ^^^^^^^^^^^^^^
  Error: [%%import]: Pwith_modtypesubst constraint is not supported.
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
  > module type%import HashableWith = sig
  >   include module type of StringHashable
  > end with module StringHashable := StringHashable
  > EOF

  $ dune build
  File "test.ml", line 15, characters 16-30:
  15 | end with module StringHashable := StringHashable
                       ^^^^^^^^^^^^^^
  Error: [%%import]: Pwith_modsubst constraint is not supported.
  [1]
