let init_path () = Compmisc.init_path ()
let lookup_module ~loc lid env = Env.lookup_module ~loc lid env |> fst
let find_type env ~loc head_id = Env.lookup_type ~loc head_id env
let find_modtype env ~loc:_ head_id = Env.find_modtype_by_name head_id env
