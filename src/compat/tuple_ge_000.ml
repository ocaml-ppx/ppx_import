let tuple f xs =
  Ppxlib.Ast_helper.Typ.tuple (List.map (fun x -> f x) xs)
