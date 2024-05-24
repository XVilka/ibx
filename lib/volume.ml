open Core
open Tws_prot

include Int

let t_of_tws = of_string
let tws_of_t = to_string
let val_type = Val_type.create tws_of_t t_of_tws
