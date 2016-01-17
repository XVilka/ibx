(* File: currency.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
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
