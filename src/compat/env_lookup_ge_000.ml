let lookup_module ~loc:_ lid env = Env.lookup_module ~load:true lid env
let find_type env ~loc head_id = Typetexp.find_type env loc head_id
let find_modtype env ~loc head_id = Typetexp.find_modtype env loc head_id
