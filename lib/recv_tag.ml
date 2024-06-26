open Core
open Tws_prot

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

let tws_of_t = function
  | Tick_price -> "1"
  | Tick_size -> "2"
  | Order_status -> "3"
  | Tws_error -> "4"
  | Open_order -> "5"
  | Account_update -> "6"
  | Position -> "7"
  | Account_update_time -> "8"
  | Next_order_id -> "9"
  | Contract_data -> "10"
  | Execution -> "11"
  | Book_update -> "12"
  | Book_update_L2 -> "13"
  | News_bulletins -> "14"
  | Managed_accounts -> "15"
  | Financial_advisor -> "16"
  | History -> "17"
  | Bond_contract_data -> "18"
  | Scanner_parameters -> "19"
  | Scanner_data -> "20"
  | Tick_option -> "21"
  | Tick_generic -> "45"
  | Tick_string -> "46"
  | Tick_efp -> "47"
  | Server_time -> "49"
  | Realtime_bar -> "50"
  | Fundamental_data -> "51"
  | Contract_data_end -> "52"
  | Open_order_end -> "53"
  | Account_download_end -> "54"
  | Executions_end -> "55"
  | Delta_neutral_validation -> "56"
  | Snapshot_end -> "57"
  | Commission -> "59"

let t_of_tws = function
  | "1" -> Tick_price
  | "2" -> Tick_size
  | "3" -> Order_status
  | "4" -> Tws_error
  | "5" -> Open_order
  | "6" -> Account_update
  | "7" -> Position
  | "8" -> Account_update_time
  | "9" -> Next_order_id
  | "10" -> Contract_data
  | "11" -> Execution
  | "12" -> Book_update
  | "13" -> Book_update_L2
  | "14" -> News_bulletins
  | "15" -> Managed_accounts
  | "16" -> Financial_advisor
  | "17" -> History
  | "18" -> Bond_contract_data
  | "19" -> Scanner_parameters
  | "20" -> Scanner_data
  | "21" -> Tick_option
  | "45" -> Tick_generic
  | "46" -> Tick_string
  | "47" -> Tick_efp
  | "49" -> Server_time
  | "50" -> Realtime_bar
  | "51" -> Fundamental_data
  | "52" -> Contract_data_end
  | "53" -> Open_order_end
  | "54" -> Account_download_end
  | "55" -> Executions_end
  | "56" -> Delta_neutral_validation
  | "57" -> Snapshot_end
  | "59" -> Commission
  | s -> failwithf "Recv_tag.of_string: %S" s ()

let corresponding_response_has_query_id = function
  | Tick_price -> true
  | Tick_size -> true
  | Order_status -> true
  | Tws_error -> true
  | Open_order -> true
  | Account_update -> false
  | Position -> false
  | Account_update_time -> false
  | Next_order_id -> false
  | Contract_data -> true
  | Execution -> true
  | Book_update -> true
  | Book_update_L2 -> true
  | News_bulletins -> false
  | Managed_accounts -> false
  | Financial_advisor -> false
  | History -> true
  | Bond_contract_data -> true
  | Scanner_parameters -> false
  | Scanner_data -> false
  | Tick_option -> true
  | Tick_generic -> true
  | Tick_string -> true
  | Tick_efp -> true
  | Server_time -> false
  | Realtime_bar -> true
  | Fundamental_data -> false
  | Contract_data_end -> true
  | Open_order_end -> true
  | Account_download_end -> false
  | Executions_end -> true
  | Delta_neutral_validation -> true
  | Snapshot_end -> true
  | Commission -> false

let val_type = Val_type.create tws_of_t t_of_tws

let decoder =
  Decoder.create ~name:"Recv_tag"
    Decoder.Spec.(value (required val_type) ~name:"recv_tag")
    Fn.id
