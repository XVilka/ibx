(* File: send_tag.mli

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

type t =
| Market_data
| Cancel_market_data
| Submit_order
| Cancel_order
| Open_orders
| Portfolio_data
| Execution_reports
| Contract_data
| Market_depth
| Cancel_market_depth
| News_bulletins
| Cancel_news_bulletins
| Set_server_log_level
| Auto_open_orders
| All_open_orders
| Managed_accounts
| Financial_advisor
| Replace_financial_advisor
| Historical_data
| Exercise_options
| Scanner_subscription
| Cancel_scanner_subscription
| Scanner_parameters
| Cancel_historical_data
| Server_time
| Realtime_bars
| Cancel_realtime_bars
| Fundamental_data
| Cancel_fundamental_data
| Implied_volatility
| Option_price
| Cancel_implied_volatility
| Cancel_option_price
with sexp
include Twsable.S with type t := t

val corresponding_query_has_id : t -> bool
