let tuple f xs =
  Ppxlib.Ast_helper.Typ.tuple (List.map (fun (_,x) -> f x) xs)
