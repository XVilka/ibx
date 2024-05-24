open Core
open Tws_prot

include Unique_id.Int63 ()

let default = of_int_exn (-1)

let increase t num = of_int_exn (to_int_exn t + num)

let val_type = Val_type.create to_string of_string
