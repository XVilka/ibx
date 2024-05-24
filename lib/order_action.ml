open Core
open Tws_prot

module T = struct
  type t =
    [ `Buy
    | `Sell
    | `Sell_short
    ] [@@deriving sexp, eq]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Buy -> "BUY"
  | `Sell -> "SELL"
  | `Sell_short -> "SSHORT"

let t_of_tws = function
  | "BUY" -> `Buy
  | "SELL" -> `Sell
  | "SSHORT" -> `Sell_short
  | s -> invalid_argf "Order_action.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
