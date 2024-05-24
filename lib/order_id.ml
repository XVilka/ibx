open Core
open Tws_prot

include Unique_id.Int63 (struct end)
let tws_of_t = to_string
let t_of_tws = of_string
let val_type = Val_type.create tws_of_t t_of_tws
let decoder =
  Decoder.create ~name:"Order_Id"
    Decoder.Spec.(value (required val_type) ~name:"order_id")
    Fn.id
