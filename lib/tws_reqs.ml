open Tws_prot

module D = Decoder
module S = Send_tag
module R = Recv_tag

(* ==================== Connection and server ===================== *)

let req_server_time = Ib.Request.create
    ~send_header:(Ib.Header.create ~tag:S.Server_time ~version:1)
    ~recv_header:(Ib.Header.create ~tag:R.Server_time ~version:1)
    ~tws_query:Query.Server_time.encoder
    ~tws_response:Response.Server_time.decoder
;;

(* ========================= Market data ========================== *)

let req_market_data = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
    ~recv_header:[
      Ib.Header.create ~tag:R.Tick_price ~version:6
    ; Ib.Header.create ~tag:R.Tick_size ~version:6
    ; Ib.Header.create ~tag:R.Tick_option ~version:6
    ; Ib.Header.create ~tag:R.Tick_string ~version:6
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Tick_generic ~version:6
    ; Ib.Header.create ~tag:R.Snapshot_end ~version:1
    ]
    ~tws_query:Query.Market_data.encoder
    ~tws_response:[
      D.map Response.Tick_price.decoder  ~f:(fun x -> `Tick_price  x)
    ; D.map Response.Tick_size.decoder   ~f:(fun x -> `Tick_size   x)
    ; D.map Response.Tick_option.decoder ~f:(fun x -> `Tick_option x)
    ; D.map Response.Tick_string.decoder ~f:(fun x -> `Tick_string x)
    ] ()
;;

let req_option_price = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Option_price ~version:1)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_option_price ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.Tick_option ~version:6]
    ~tws_query:Query.Option_price.encoder
    ~tws_response:[
      D.map Response.Tick_option.decoder ~f:Response.Tick_option.option_price
    ] ()
;;

let req_implied_volatility = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Implied_volatility ~version:1)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_implied_volatility ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.Tick_option ~version:6]
    ~tws_query:Query.Implied_volatility.encoder
    ~tws_response:[
      D.map Response.Tick_option.decoder ~f:Response.Tick_option.implied_vol
    ] ()
;;

(* ===================== Contract details ========================= *)

let req_contract_details = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Contract_data ~version:6)
    ~recv_header:[
      Ib.Header.create ~tag:R.Contract_data ~version:8
    ; Ib.Header.create ~tag:R.Contract_data_end ~version:1
    ]
    ~tws_query:Query.Contract_details.encoder
    ~tws_response:[
      D.map Response.Contract_data.decoder ~f:(fun x -> `Contract_data x)
    ; D.const `Contract_data_end
    ] ()
;;

(* ========================== Orders ============================== *)

let req_submit_order = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Submit_order ~version:39)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_order ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.Order_status ~version:6]
    ~skip_header:[Ib.Header.create ~tag:R.Open_order ~version:30]
    ~tws_query:Query.Submit_order.encoder
    ~tws_response:[Response.Order_status.decoder]
    ()
;;

(* ================== Account and portfolio ======================= *)

let req_account_updates = Ib.Streaming_request_without_id.create
    ~send_header:(Ib.Header.create ~tag:S.Account_data ~version:2)
    ~recv_header:[
      Ib.Header.create ~tag:R.Account_update ~version:2
    ; Ib.Header.create ~tag:R.Account_download_end ~version:1
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Account_update_time ~version:1
    ; Ib.Header.create ~tag:R.Position ~version:7
    ]
    ~tws_query:Query.Account_updates.encoder
    ~tws_response:[
      D.map Response.Account_update.decoder ~f:(fun x -> `Update x)
    ; D.map Account_code.decoder ~f:(fun x -> `Update_end x)
    ] ()
;;

let req_portfolio = Ib.Streaming_request_without_id.create
    ~send_header:(Ib.Header.create ~tag:S.Account_data ~version:2)
    ~recv_header:[
      Ib.Header.create ~tag:R.Position ~version:7
    ; Ib.Header.create ~tag:R.Account_download_end ~version:1
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Account_update ~version:2
    ; Ib.Header.create ~tag:R.Account_update_time ~version:1
    ]
    ~tws_query:Query.Positions.encoder
    ~tws_response:[
      D.map Response.Position.decoder ~f:(fun x -> `Update x)
    ; D.map Account_code.decoder ~f:(fun x -> `Update_end x)
    ] ()
;;

(* ========================= Executions =========================== *)

let req_executions = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Executions ~version:3)
    ~recv_header:[
      Ib.Header.create ~tag:R.Execution ~version:9
    ; Ib.Header.create ~tag:R.Executions_end ~version:1
    ]
    ~tws_query:Query.Executions.encoder
    ~tws_response:[
      D.map Response.Execution.decoder ~f:(fun x -> `Execution x)
    ; D.const `Executions_end
    ] ()
;;

(* ======================== Market depth ========================== *)

let req_market_depth = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_depth ~version:3)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_depth ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.Book_update ~version:1]
    ~skip_header:[Ib.Header.create ~tag:R.Book_update_L2 ~version:1]
    ~tws_query:Query.Market_depth.encoder
    ~tws_response:[Response.Book_update.decoder]
    ()
;;

(* =========================== History ============================ *)

let req_history = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.History ~version:4)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_history ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.History ~version:3]
    ~tws_query:Query.History.encoder
    ~tws_response:[Response.History.decoder]
    ()
;;

(* ======================== Realtime bars ========================= *)

let req_realtime_bars = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Realtime_bars ~version:1)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_realtime_bars ~version:1)
    ~recv_header:[Ib.Header.create ~tag:R.Realtime_bar ~version:3]
    ~tws_query:Query.Realtime_bars.encoder
    ~tws_response:[Response.Realtime_bar.decoder]
    ()
;;

(* ========================= TAQ data ============================= *)

let req_taq_data = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
    ~recv_header:[
      Ib.Header.create ~tag:R.Tick_price ~version:6
    ; Ib.Header.create ~tag:R.Tick_size ~version:6
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Tick_string ~version:6
    ; Ib.Header.create ~tag:R.Tick_generic ~version:6
    ]
    ~tws_query:Query.Market_data.encoder
    ~tws_response:[
      D.map Response.Tick_price.decoder ~f:(fun x -> `Tick_price x)
    ; D.map Response.Tick_size.decoder  ~f:(fun x -> `Tick_size  x)
    ] ()
;;

(* ========================= Snapshots ============================ *)

let req_snapshot = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
    ~recv_header:[
      Ib.Header.create ~tag:R.Tick_price ~version:6
    ; Ib.Header.create ~tag:R.Snapshot_end ~version:1
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Tick_size ~version:6
    ; Ib.Header.create ~tag:R.Tick_option ~version:6
    ; Ib.Header.create ~tag:R.Tick_string ~version:6
    ; Ib.Header.create ~tag:R.Tick_generic ~version:6
    ]
    ~tws_query:Query.Market_data.encoder
    ~tws_response:[
      D.map Response.Tick_price.decoder ~f:(fun x -> `Tick_price x)
    ; D.const `Snapshot_end;
    ] ()
;;
