type sorts =
  [%import: Sorts.family]
  [@@deriving sexp]

let main () =
  let test = Sorts.InType in
  let sexp = sexp_of_sorts test in
  let orig = sorts_of_sexp sexp in
  assert (orig = test)

let _ = main ()
