open Core_kernel
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

let tws_of_t = function
  | Market_data -> "1"
  | Cancel_market_data -> "2"
  | Submit_order -> "3"
  | Cancel_order -> "4"
  | Open_orders -> "5"
  | Account_data -> "6"
  | Executions -> "7"
  | Contract_data -> "9"
  | Market_depth -> "10"
  | Cancel_market_depth -> "11"
  | News_bulletins -> "12"
  | Cancel_news_bulletins -> "13"
  | Set_server_log_level -> "14"
  | Auto_open_orders -> "15"
  | All_open_orders -> "16"
  | Managed_accounts -> "17"
  | Financial_advisor -> "18"
  | Replace_financial_advisor -> "19"
  | History -> "20"
  | Exercise_options -> "21"
  | Scanner_subscription -> "22"
  | Cancel_scanner_subscription -> "23"
  | Scanner_parameters -> "24"
  | Cancel_history -> "25"
  | Server_time -> "49"
  | Realtime_bars -> "50"
  | Cancel_realtime_bars -> "51"
  | Fundamental_data -> "52"
  | Cancel_fundamental_data -> "53"
  | Implied_volatility -> "54"
  | Option_price -> "55"
  | Cancel_implied_volatility -> "56"
  | Cancel_option_price -> "57"

let t_of_tws = function
  | "1" -> Market_data
  | "2" -> Cancel_market_data
  | "3" -> Submit_order
  | "4" -> Cancel_order
  | "5" -> Open_orders
  | "6" -> Account_data
  | "7" -> Executions
  | "9" -> Contract_data
  | "10" -> Market_depth
  | "11" -> Cancel_market_depth
  | "12" -> News_bulletins
  | "13" -> Cancel_news_bulletins
  | "14" -> Set_server_log_level
  | "15" -> Auto_open_orders
  | "16" -> All_open_orders
  | "17" -> Managed_accounts
  | "18" -> Financial_advisor
  | "19" -> Replace_financial_advisor
  | "20" -> History
  | "21" -> Exercise_options
  | "22" -> Scanner_subscription
  | "23" -> Cancel_scanner_subscription
  | "24" -> Scanner_parameters
  | "25" -> Cancel_history
  | "49" -> Server_time
  | "50" -> Realtime_bars
  | "51" -> Cancel_realtime_bars
  | "52" -> Fundamental_data
  | "53" -> Cancel_fundamental_data
  | "54" -> Implied_volatility
  | "55" -> Option_price
  | "56" -> Cancel_implied_volatility
  | "57" -> Cancel_option_price
  | s -> failwithf "Send_tag.of_string: %S" s ()

let corresponding_query_has_id = function
  | Market_data -> true
  | Cancel_market_data -> true
  | Submit_order -> true
  | Cancel_order -> true
  | Open_orders -> false
  | Account_data -> false
  | Executions -> true
  | Contract_data -> true
  | Market_depth -> true
  | Cancel_market_depth -> true
  | News_bulletins -> false
  | Cancel_news_bulletins -> true
  | Set_server_log_level -> false
  | Auto_open_orders -> true
  | All_open_orders -> false
  | Managed_accounts -> false
  | Financial_advisor -> false
  | Replace_financial_advisor -> false
  | History -> true
  | Exercise_options -> true
  | Scanner_subscription -> false
  | Cancel_scanner_subscription -> true
  | Scanner_parameters -> false
  | Cancel_history -> true
  | Server_time -> false
  | Realtime_bars -> true
  | Cancel_realtime_bars -> true
  | Fundamental_data -> true
  | Cancel_fundamental_data -> true
  | Implied_volatility -> true
  | Option_price -> true
  | Cancel_implied_volatility -> true
  | Cancel_option_price -> true

let val_type = Val_type.create tws_of_t t_of_tws
