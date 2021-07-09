open Core_kernel
open Tws_prot

let tws_of_t x =
  let s = Float.to_string x in
  let n = String.length s in
  if Char.(s.[n-1] = '.') then s^"0" else s

include Float
let t_of_tws = of_string

let val_type = Val_type.create tws_of_t t_of_tws
