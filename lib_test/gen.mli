(* File: gen.mli

   IBX - Pure OCaml implementation of the Interactive Brokers TWS API

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

open Ibx.Std

val server_time       : Server_time.t
val market_data       : Market_data.t list
val tick_option       : Tick_option.t
val order_states      : Order_status.t list
val execution_reports : Execution_report.t list
val contract_specs    : Contract_specs.t
val book_updates      : Book_update.t list
val historical_data   : Historical_data.t
val realtime_bars     : Realtime_bar.t list
