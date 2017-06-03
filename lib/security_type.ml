open Core
open Tws_prot

module T = struct
  type t =
    [ `Stock
    | `Index
    | `Futures
    | `Option
    | `Fut_opt
    | `Forex
    ] [@@deriving sexp]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Stock   -> "STK"
  | `Index   -> "IND"
  | `Futures -> "FUT"
  | `Option  -> "OPT"
  | `Fut_opt -> "FOP"
  | `Forex   -> "CASH"

let t_of_tws = function
  | "STK"  -> `Stock
  | "IND"  -> `Index
  | "FUT"  -> `Futures
  | "OPT"  -> `Option
  | "FOP"  -> `Fut_opt
  | "CASH" -> `Forex
  | s -> invalid_argf "Security_type.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
