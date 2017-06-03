open Core_kernel
open Tws_prot

include String
let tws_of_t = to_string
let t_of_tws = of_string
let val_type = Val_type.create tws_of_t t_of_tws

let decoder =
  Decoder.create ~name:"Account_code"
    Decoder.Spec.(value (required val_type) ~name:"account_code")
    Fn.id
