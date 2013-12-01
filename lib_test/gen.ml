(* File: gen.ml

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

open Rg.R

let server_time       = lazy (server_time_g ())
let market_data       = lazy (market_data_g ())
let tick_option       = lazy (tick_option_g ())
let order_states      = lazy (order_states_g ())
let execution_reports = lazy (execution_reports_g ())
let contract_details  = lazy (contract_details_g ())
let book_updates      = lazy (book_updates_g ())
let historical_data   = lazy (historical_data_g ())
let realtime_bars     = lazy (realtime_bars_g ())
