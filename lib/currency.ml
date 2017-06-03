open Core_kernel
open Tws_prot

type t =
  [ `USD
  | `AUD
  | `CAD
  | `CHF
  | `CNH
  | `DKK
  | `EUR
  | `GBP
  | `HKD
  | `HUF
  | `ILS
  | `JPY
  | `MXN
  | `NOK
  | `NZD
  | `RUB
  | `SEK
  | `SGD
  | `KRW
  ] [@@deriving sexp]

let tws_of_t = function
  | `USD -> "USD"
  | `AUD -> "AUD"
  | `CAD -> "CAD"
  | `CHF -> "CHF"
  | `CNH -> "CNH"
  | `DKK -> "DKK"
  | `EUR -> "EUR"
  | `GBP -> "GBP"
  | `HKD -> "HKD"
  | `HUF -> "HUF"
  | `ILS -> "ILS"
  | `JPY -> "JPY"
  | `MXN -> "MXN"
  | `NOK -> "NOK"
  | `NZD -> "NZD"
  | `RUB -> "RUB"
  | `SEK -> "SEK"
  | `SGD -> "SGD"
  | `KRW -> "KRW"

let t_of_tws = function
  | "USD" -> `USD
  | "AUD" -> `AUD
  | "CAD" -> `CAD
  | "CHF" -> `CHF
  | "CNH" -> `CNH
  | "DKK" -> `DKK
  | "EUR" -> `EUR
  | "GBP" -> `GBP
  | "HKD" -> `HKD
  | "HUF" -> `HUF
  | "ILS" -> `ILS
  | "JPY" -> `JPY
  | "MXN" -> `MXN
  | "NOK" -> `NOK
  | "NZD" -> `NZD
  | "RUB" -> `RUB
  | "SEK" -> `SEK
  | "SGD" -> `SGD
  | "KRW" -> `KRW
  | s -> failwithf "Currency.to_string: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws

let to_string = tws_of_t
let of_string = t_of_tws
