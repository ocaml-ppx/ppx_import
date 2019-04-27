type a = A1 | A2 of string
type b = {
  b1 : a;
  b2 : string;
  b3 : Int64.t;
}
type c = [ `A | `B | `C of string ]
type d = Int64.t
type e = string * int

module type S = sig
  type f = int
end

type 'a g = Foo of 'a

type h = Zero | Succ of h
module MI = struct
  type i = int
end
open MI
type nonrec i = I of i

module type S_rec = sig
  type t = A of u
  and u = B of t
end
