open Tws_prot

let to_tws e x = Encoder.run e x
let of_tws d x = Ibx_result.try_with_decode (fun () -> Decoder.run_exn d x)
