open Tws_prot

type t =
  | Market_data
  | Cancel_market_data
  | Submit_order
  | Cancel_order
  | Open_orders
  | Account_data
  | Executions
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
  | History
  | Exercise_options
  | Scanner_subscription
  | Cancel_scanner_subscription
  | Scanner_parameters
  | Cancel_history
  | Server_time
  | Realtime_bars
  | Cancel_realtime_bars
  | Fundamental_data
  | Cancel_fundamental_data
  | Implied_volatility
  | Option_price
  | Cancel_implied_volatility
  | Cancel_option_price
[@@deriving sexp]
include Twsable.S with type t := t

val t_of_tws : string -> t
val corresponding_query_has_id : t -> bool
