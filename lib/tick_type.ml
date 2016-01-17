(* File: tick_type.ml

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

module T = struct
  type t =
  [ `Auction_values (* Volume, price and imbalance *)
  | `Fundamental_ratios
  | `Historical_volatility
  | `Implied_volatility
  | `Index_future_premium
  | `Inventory
  | `Mark_price (* Used in TWS P&L computations *)
  | `Misc_stats
  | `Option_open_interest
  | `Option_volume
  | `Realtime_volume
  | `Shortable
  | `Turn_off_market_data
  ] [@@deriving sexp]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Option_volume -> "100"
  | `Option_open_interest -> "101"
  | `Historical_volatility -> "104"
  | `Implied_volatility -> "106"
  | `Index_future_premium -> "162"
  | `Misc_stats -> "165"
  | `Mark_price -> "221"
  | `Auction_values -> "225"
  | `Realtime_volume -> "233"
  | `Shortable -> "236"
  | `Inventory -> "256"
  | `Fundamental_ratios -> "258"
  | `Turn_off_market_data -> "mdoff"

let t_of_tws = function
  | "100" -> `Option_volume
  | "101" -> `Option_open_interest
  | "104" -> `Historical_volatility
  | "106" -> `Implied_volatility
  | "162" -> `Index_future_premium
  | "165" -> `Misc_stats
  | "221" -> `Mark_price
  | "225" -> `Auction_values
  | "233" -> `Realtime_volume
  | "236" -> `Shortable
  | "256" -> `Inventory
  | "258" -> `Fundamental_ratios
  | "mdoff" -> `Turn_off_market_data
  | s -> invalid_argf "Tick_type.t_of_tws: %s" s ()

let val_type = Val_type.create tws_of_t t_of_tws
