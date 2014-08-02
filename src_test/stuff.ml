type a = A1 | A2 of string
type b = {
  b1 : a;
  b2 : string;
  b3 : Int64.t;
}
type c = [ `A | `B | `C of string ]
type d = Int64.t
type e = string * int
