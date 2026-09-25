let rec get_desc texp =
    match Types.get_desc texp with
    | Tpoly (tt, []) -> get_desc tt
    | tt -> tt
let row_fields = Types.row_fields
let row_field_repr = Types.row_field_repr
