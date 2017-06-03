type t =
  | Tick_price
  | Tick_size
  | Order_status
  | Tws_error
  | Open_order
  | Account_update
  | Position
  | Account_update_time
  | Next_order_id
  | Contract_data
  | Execution
  | Book_update
  | Book_update_L2
  | News_bulletins
  | Managed_accounts
  | Financial_advisor
  | History
  | Bond_contract_data
  | Scanner_parameters
  | Scanner_data
  | Tick_option
  | Tick_generic
  | Tick_string
  | Tick_efp
  | Server_time
  | Realtime_bar
  | Fundamental_data
  | Contract_data_end
  | Open_order_end
  | Account_download_end
  | Executions_end
  | Delta_neutral_validation
  | Snapshot_end
  | Commission
[@@deriving sexp]
include Twsable.S with type t := t
include Decodable.S with type t := t

val corresponding_response_has_query_id : t -> bool
