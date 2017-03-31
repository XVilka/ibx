(* File: order_type.ml

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

open Core
open Tws_prot

module T = struct
  type t =
    [ `Auction
    | `Basket
    | `Bracket
    | `Hidden
    | `Iceberg
    | `Limit
    | `Limit_if_touched
    | `Limit_on_close
    | `Limit_on_open
    | `Market
    | `Market_if_touched
    | `Market_on_close
    | `Market_on_open
    | `Market_to_limit
    | `Pegged_to_benchmark
    | `Pegged_to_market
    | `Pegged_to_midpoint
    | `Stop
    | `Stop_limit
    | `TWAP
    | `Trailing_limit_if_touched
    | `Trailing_market_if_touched
    | `Trailing_stop
    | `Trailing_stop_limit
    | `VWAP
    ] [@@deriving sexp]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Auction -> "AUC"
  | `Basket -> "BASKET"
  | `Bracket -> "BRACKET"
  | `Hidden -> "HID"
  | `Iceberg -> "ICE"
  | `Limit -> "LMT"
  | `Limit_if_touched -> "LIT"
  | `Limit_on_close -> "LOC"
  | `Limit_on_open -> "LOO"
  | `Market -> "MKT"
  | `Market_if_touched -> "MIT"
  | `Market_on_close -> "MOC"
  | `Market_on_open -> "MOO"
  | `Market_to_limit -> "MTL"
  | `Pegged_to_benchmark -> "PEGBENCH"
  | `Pegged_to_market -> "PEGMKT"
  | `Pegged_to_midpoint -> "PEGMID"
  | `Stop -> "STP"
  | `Stop_limit -> "STPLMT"
  | `TWAP -> "TWAP"
  | `Trailing_limit_if_touched -> "TRAILLIT"
  | `Trailing_market_if_touched -> "TRAILMIT"
  | `Trailing_stop -> "TRAIL"
  | `Trailing_stop_limit -> "TRAILLIMIT"
  | `VWAP -> "VWAP"

let t_of_tws = function
  | "AUC" -> `Auction
  | "BASKET" -> `Basket
  | "BRACKET" -> `Bracket
  | "HID" -> `Hidden
  | "ICE" -> `Iceberg
  | "LMT" -> `Limit
  | "LIT" -> `Limit_if_touched
  | "LOC" -> `Limit_on_close
  | "LOO" -> `Limit_on_open
  | "MKT" -> `Market
  | "MIT" -> `Market_if_touched
  | "MOC" -> `Market_on_close
  | "MOO" -> `Market_on_open
  | "MTL" -> `Market_to_limit
  | "PEGBENCH" -> `Pegged_to_benchmark
  | "PEGMKT" -> `Pegged_to_market
  | "PEGMID" -> `Pegged_to_midpoint
  | "STP" -> `Stop
  | "STPLMT" -> `Stop_limit
  | "TWAP" -> `TWAP
  | "TRAILLIT" -> `Trailing_limit_if_touched
  | "TRAILMIT" -> `Trailing_market_if_touched
  | "TRAIL" -> `Trailing_stop
  | "TRAILLIMIT" -> `Trailing_stop_limit
  | "VWAP" -> `VWAP
  | s -> failwithf "Order_type.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
