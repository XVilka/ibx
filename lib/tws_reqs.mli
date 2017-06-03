(** TWS requests *)

(** {1 Connection and server} *)
(*****************************************************************************)

val req_server_time :
  (Query.Server_time.t, Response.Server_time.t) Ib.Request.t

(** {1 Market data} *)
(*****************************************************************************)

val req_market_data :
  (Query.Market_data.t, [ `Tick_price  of Response.Tick_price.t
                        | `Tick_size   of Response.Tick_size.t
                        | `Tick_option of Response.Tick_option.t
                        | `Tick_string of Response.Tick_string.t
                        ]) Ib.Streaming_request.t

val req_option_price :
  (Query.Option_price.t, Price.t) Ib.Streaming_request.t

val req_implied_volatility :
  (Query.Implied_volatility.t, float) Ib.Streaming_request.t

(** {1 Contract details} *)
(*****************************************************************************)

val req_contract_details :
  (Query.Contract_details.t, [ `Contract_data of Response.Contract_data.t
                             | `Contract_data_end
                             ]) Ib.Streaming_request.t

(** {1 Orders} *)
(*****************************************************************************)

val req_submit_order :
  (Query.Submit_order.t, Response.Order_status.t) Ib.Streaming_request.t

(** {1 Account and portfolio} *)
(*****************************************************************************)

val req_account_updates :
  (Query.Account_updates.t, [ `Update of Response.Account_update.t
                            | `Update_end of Account_code.t
                            ]) Ib.Streaming_request_without_id.t

val req_portfolio :
  (Query.Positions.t, [ `Update of Response.Position.t
                      | `Update_end of Account_code.t
                      ]) Ib.Streaming_request_without_id.t

(** {1 Execution data} *)
(*****************************************************************************)

val req_executions :
  (Query.Executions.t, [ `Execution of Response.Execution.t
                       | `Executions_end
                       ]) Ib.Streaming_request.t

(** {1 Market depth} *)
(*****************************************************************************)

val req_market_depth :
  (Query.Market_depth.t, Response.Book_update.t) Ib.Streaming_request.t

(** {1 History} *)
(*****************************************************************************)

val req_history :
  (Query.History.t, Response.History.t) Ib.Streaming_request.t

(** {1 Realtime bars} *)
(*****************************************************************************)

val req_realtime_bars :
  (Query.Realtime_bars.t, Bar.t) Ib.Streaming_request.t


(** {1 TAQ data} *)
(*****************************************************************************)

val req_taq_data :
  (Query.Market_data.t, [ `Tick_price of Response.Tick_price.t
                        | `Tick_size  of Response.Tick_size.t
                        ]) Ib.Streaming_request.t

(** {1 Snapshots} *)
(*****************************************************************************)

val req_snapshot :
  (Query.Market_data.t, [ `Tick_price of Response.Tick_price.t
                        | `Snapshot_end
                        ]) Ib.Streaming_request.t
