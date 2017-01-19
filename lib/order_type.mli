(* File: order_type.mli

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
include Stringable.S with type t := t
include Twsable.S with type t := t
