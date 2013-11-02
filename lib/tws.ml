(* File: tws.ml

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
open Async.Std
open Std_internal
open Response

include Client

(* Connection and server *)

let server_time t =
  let q = Query.Server_time.create () in
  dispatch_request t Tws_reqs.req_server_time q

let server_time_exn t = server_time t >>| Or_error.ok_exn

(* Market data *)

module Market_data = struct
  type t =
  [ `Tick_price  of Tick_price.t
  | `Tick_size   of Tick_size.t
  | `Tick_option of Tick_option.t
  | `Tick_string of Tick_string.t
  ] with sexp

  let ( = ) t1 t2 = match t1, t2 with
    | `Tick_price  x, `Tick_price  y -> Tick_price. (=) x y
    | `Tick_size   x, `Tick_size   y -> Tick_size.  (=) x y
    | `Tick_option x, `Tick_option y -> Tick_option.(=) x y
    | `Tick_string x, `Tick_string y -> Tick_string.(=) x y
    | _ -> false

  let pp ppf t = match t with
    | `Tick_price  t -> Tick_price. pp ppf t
    | `Tick_size   t -> Tick_size.  pp ppf t
    | `Tick_option t -> Tick_option.pp ppf t
    | `Tick_string t -> Tick_string.pp ppf t
end

let market_data ?(snapshot = false) ?(tick_generics = []) t ~contract =
  let q = Query.Market_data.create ~contract ~tick_generics ~snapshot in
  dispatch_streaming_request t Tws_reqs.req_market_data q

let market_data_exn ?snapshot ?tick_generics t ~contract =
  market_data ?snapshot ?tick_generics t ~contract >>| Or_error.ok_exn

let cancel_market_data t id = cancel_streaming_request t Tws_reqs.req_market_data id

let option_price t ~contract ~volatility ~underlying_price =
  let q = Query.Option_price.create ~contract ~volatility ~underlying_price in
  dispatch_and_cancel t Tws_reqs.req_option_price q >>| function
  | Error _ as x -> x
  | Ok tick ->
    begin
      match Tick_option.option_price tick with
      | None -> Error (
        Error.create "option price not found"
          tick <:sexp_of< Response.Tick_option.t >>)
      | Some opt_price -> Ok opt_price
    end

let option_price_exn t ~contract ~volatility ~underlying_price =
  option_price t ~contract ~volatility ~underlying_price >>| Or_error.ok_exn

let implied_volatility t ~contract ~option_price ~underlying_price =
  let q = Query.Implied_volatility.create ~contract ~option_price ~underlying_price in
  dispatch_and_cancel t Tws_reqs.req_implied_volatility q >>| function
  | Error _ as x -> x
  | Ok tick ->
    begin
      match Tick_option.implied_volatility tick with
      | None -> Error (
        Error.create "implied volatility not found"
          tick <:sexp_of< Response.Tick_option.t >>)
      | Some implied_vol -> Ok implied_vol
    end

let implied_volatility_exn t ~contract ~option_price ~underlying_price =
  implied_volatility t ~contract ~option_price ~underlying_price
  >>| Or_error.ok_exn

(* Orders *)

let dedup_adjacents ~equal pipe_r =
  let last = ref None in
  Pipe.filter_map pipe_r ~f:(fun x ->
    match !last with
    | None -> last := Some x; !last
    | Some y -> if equal x y then None else begin last := Some x; !last end)

let submit_order t ~contract ~order =
  let q = Query.Submit_order.create
    ~contract
    ~order
    (* Note: [account_code t] won't return [None] since we
       can only call it in a handler of [Tws.with_client]. *)
    ~account_code:(Option.value_exn (account_code t))
  in
  dispatch_streaming_request t Tws_reqs.req_submit_order q >>| function
  | Error _ as e -> e
  | Ok (pipe_r, id) ->
    let oid = Order_id.of_int_exn (Query_id.to_int_exn id) in
    Ok (dedup_adjacents ~equal:Order_status.(=) pipe_r, oid)

let submit_order_exn t ~contract ~order =
  submit_order t ~contract ~order >>| Or_error.ok_exn

let cancel_order_status t oid =
  let id = Query_id.of_int_exn (Order_id.to_int_exn oid) in
  cancel_streaming_request t Tws_reqs.req_submit_order id

(* Account and portfolio *)

let account_updates t =
  let account_code = Option.value_exn (account_code t) in
  let create_query = Query.Account_and_portfolio_updates.create ~account_code in
  let subscribe = create_query ~subscribe:true in
  dispatch_streaming_request' t Tws_reqs.req_account_updates subscribe >>| function
  | Error _ as e -> e
  | Ok pipe_r ->
    let pipe_r = Pipe.filter_map pipe_r ~f:(function
      | `Account_update data -> Some data
      | `Account_update_end code ->
        if Account_code.(=) account_code code then begin
          cancel_streaming_request' t Tws_reqs.req_account_updates
        end;
        None)
    in
    Ok pipe_r

let account_updates_exn t = account_updates t >>| Or_error.ok_exn

(* Executions *)

let filter_executions ?time t ~contract ~order_action =
  let q = Query.Execution_reports.create
    ~contract
    ~client_id:(client_id t)
    (* Note: [account_code t] won't return [None] since we
       can only call it in a handler of [Tws.with_client]. *)
    ~account_code:(Option.value_exn (account_code t))
    ~time:(Option.value time ~default:(Time.sub (Time.now ()) Time.Span.day))
    ~order_action
  in
  dispatch_streaming_request t Tws_reqs.req_execution_reports q >>| function
  | Error _ as e -> e
  | Ok (pipe_r, id) ->
    let pipe_r = Pipe.filter_map pipe_r ~f:(function
      | `Execution_report data -> Some data
      | `Execution_report_end  ->
        cancel_streaming_request t Tws_reqs.req_execution_reports id; None)
    in
    Ok pipe_r

let filter_executions_exn ?time t ~contract ~order_action =
  filter_executions ?time t ~contract ~order_action >>| Or_error.ok_exn

(* Contract specs *)

let contract_specs t ~contract =
  let q = Query.Contract_specs.create ~contract in
  dispatch_and_cancel t Tws_reqs.req_contract_specs q

let contract_specs_exn t ~contract =
  contract_specs t ~contract >>| Or_error.ok_exn

(* Market depth *)

let market_depth ?(num_rows = 10) t ~contract =
  let q = Query.Market_depth.create ~contract ~num_rows in
  dispatch_streaming_request t Tws_reqs.req_market_depth q

let market_depth_exn ?num_rows t ~contract =
  market_depth ?num_rows t ~contract >>| Or_error.ok_exn

let cancel_market_depth t id = cancel_streaming_request t Tws_reqs.req_market_depth id

(* Historical data *)

let historical_data
    ?(bar_size = `One_hour)
    ?(duration = `Month 1)
    ?(use_rth = true)
    ?(show = `Trades)
    ?(until = Time.now ())
    t ~contract =
  let q = Query.Historical_data.create ~contract ~end_date_time:until
    ~bar_size ~duration ~use_rth ~show in
  dispatch_and_cancel t Tws_reqs.req_historical_data q

let historical_data_exn ?bar_size ?duration ?use_rth ?show ?until t ~contract =
  historical_data ?bar_size ?duration ?use_rth ?show ?until t ~contract
  >>| Or_error.ok_exn

(* Realtime bars *)

let realtime_bars
    ?(bar_size = `Five_secs)
    ?(show = `Trades)
    ?(use_rth = true)
    t ~contract =
  let q = Query.Realtime_bars.create ~contract ~bar_size ~show ~use_rth in
  dispatch_streaming_request t Tws_reqs.req_realtime_bars q

let realtime_bars_exn ?bar_size ?show ?use_rth t ~contract =
  realtime_bars ?bar_size ?show ?use_rth t ~contract >>| Or_error.ok_exn

let cancel_realtime_bars t id =
  cancel_streaming_request t Tws_reqs.req_realtime_bars id

(* Trades *)

module Trade = struct
  type t =
    { mutable time  : Time.t;
      mutable price : Price.t;
      mutable size  : int;
    } with sexp, fields

  let make_filter () =
    let t =
      {
        time  = Time.epoch;
        price = Price.zero;
        size  = 0;
      }
    in
    stage (fun tick ->
      match tick with
      | `Tick_price tick ->
        begin match Tick_price.tick_type tick with
        | Tick_price.Type.Last ->
          let new_price = Tick_price.price tick in
          let new_size  = Tick_price.size tick in
          if new_size = 0 then
            None (* We skip trades with 0 size. *)
          else begin
            t.price <- new_price;
            t.size  <- new_size;
            t.time  <- Time.now ();
            Some t
          end
        | Tick_price.Type.Bid
        | Tick_price.Type.Ask
        | Tick_price.Type.High
        | Tick_price.Type.Low
        | Tick_price.Type.Close -> None
        end
      | `Tick_size _tick -> None)

  let pp ppf t =
    Format.fprintf ppf "T|%s|%4.2f|%4d"
      (Time.to_string t.time)
      (Price.to_float t.price)
      t.size
end

let trades t ~contract =
  let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:false in
  dispatch_streaming_request t Tws_reqs.req_taq_data q >>| function
  | Error _ as x -> x
  | Ok (ticks, id) ->
    let trades = Pipe.filter_map ticks ~f:(unstage (Trade.make_filter ())) in
    Ok (trades, id)

let trades_exn t ~contract = trades t ~contract >>| Or_error.ok_exn

let cancel_trades t id =
  cancel_streaming_request t Tws_reqs.req_taq_data id

(* Quotes *)

module Quote = struct
  type t =
    { mutable time : Time.t;
      mutable ask_size : int;
      mutable bid_size : int;
      mutable ask_size_change : int;
      mutable bid_size_change : int;
      mutable ask_price : Price.t;
      mutable bid_price : Price.t;
      mutable ask_price_change : Price.t;
      mutable bid_price_change : Price.t;
      mutable change :
        [ `Unknown
        | `Ask_size_change
        | `Bid_size_change
        | `Ask_price_change
        | `Bid_price_change
        | `Ask_size_and_price_change
        | `Bid_size_and_price_change ];
    } with sexp, fields

  let make_filter () =
    let t =
      {
        time             = Time.epoch;
        ask_size         = 0;
        bid_size         = 0;
        ask_size_change  = 0;
        bid_size_change  = 0;
        ask_price        = Price.zero;
        bid_price        = Price.zero;
        ask_price_change = Price.zero;
        bid_price_change = Price.zero;
        change           = `Unknown;
      }
    in
    stage (fun tick ->
      match tick with
      | `Tick_price tick ->
        begin match Tick_price.tick_type tick with
        | Tick_price.Type.Ask ->
          let new_ask_size  = Tick_price.size  tick in
          let new_ask_price = Tick_price.price tick in
          let old_ask_size  = t.ask_size in
          let old_ask_price = t.ask_price in
          (* Compute the ask size & price change. *)
          if old_ask_size > 0 then
            t.ask_size_change <- new_ask_size - old_ask_size;
          if Price.(old_ask_price >. zero) then
            t.ask_price_change <- Price.(new_ask_price - old_ask_price);
          (* Update the ask size & price. *)
          t.ask_size  <- new_ask_size;
          t.ask_price <- new_ask_price;
          if t.ask_size_change = 0 && old_ask_size = 0 then
            (* This case occurs only when the first tick is on the ask side. *)
            t.change <- `Unknown
          else if t.ask_size_change = 0 then
            t.change <- `Ask_price_change
          else
            t.change <- `Ask_size_and_price_change;
          if t.bid_size = 0 then
            None (* We don't issue a quote without any bid information. *)
          else begin
            t.time <- Time.now ();
            Some t
          end
        | Tick_price.Type.Bid ->
          let new_bid_size  = Tick_price.size  tick in
          let new_bid_price = Tick_price.price tick in
          let old_bid_size  = t.bid_size in
          let old_bid_price = t.bid_price in
          (* Compute bid size & price change. *)
          if old_bid_size > 0 then
            t.bid_size_change <- new_bid_size - old_bid_size;
          if Price.(old_bid_price >. zero) then
            t.bid_price_change <- Price.(new_bid_price - old_bid_price);
          (* Update the bid size & price. *)
          t.bid_size  <- new_bid_size;
          t.bid_price <- new_bid_price;
          if t.bid_size_change = 0 && old_bid_size = 0 then
            (* This case occurs only when the first tick is on the bid side. *)
            t.change <- `Unknown
          else if t.bid_size_change = 0 then
            t.change <- `Bid_price_change
          else
            t.change <- `Bid_size_and_price_change;
          if t.ask_size = 0 then
            None (* We don't issue a quote without any ask information. *)
          else begin
            t.time <- Time.now ();
            Some t
          end
        | Tick_price.Type.Last
        | Tick_price.Type.High
        | Tick_price.Type.Low
        | Tick_price.Type.Close -> None
        end
      | `Tick_size tick ->
        begin match Tick_size.tick_type tick with
        | Tick_size.Type.Ask ->
          let new_ask_size = Tick_size.size tick in
          let old_ask_size = t.ask_size in
          if new_ask_size = old_ask_size then
            None
          else begin
            t.ask_size_change <- new_ask_size - old_ask_size;
            t.ask_size <- new_ask_size;
            t.change <- `Ask_size_change;
            t.time <- Time.now ();
            Some t
          end
        | Tick_size.Type.Bid ->
          let new_bid_size = Tick_size.size tick in
          let old_bid_size = t.bid_size in
          if new_bid_size = old_bid_size then
            None
          else begin
            t.bid_size_change <- new_bid_size - old_bid_size;
            t.bid_size <- new_bid_size;
            t.change <- `Bid_size_change;
            t.time <- Time.now ();
            Some t
          end
        | Tick_size.Type.Last
        | Tick_size.Type.Volume -> None
        end)

  let pp ppf t =
    Format.fprintf ppf "Q|%s|%4.2f|%4.2f|%4d|%4d"
      (Time.to_string t.time)
      (Price.to_float t.bid_price)
      (Price.to_float t.ask_price)
      t.bid_size t.ask_size
end

let quotes t ~contract =
  let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:false in
  dispatch_streaming_request t Tws_reqs.req_taq_data q >>| function
  | Error _ as x -> x
  | Ok (ticks, id) ->
    let quotes = Pipe.filter_map ticks ~f:(unstage (Quote.make_filter ())) in
    Ok (quotes, id)

let quotes_exn t ~contract = quotes t ~contract >>| Or_error.ok_exn

let cancel_quotes t id =
  cancel_streaming_request t Tws_reqs.req_taq_data id

(* TAQ data *)

module TAQ = struct
  type t =
  | Trade of Trade.t
  | Quote of Quote.t
  with sexp

  let trade x = Trade x
  let quote x = Quote x

  let pp ppf = function
    | Trade trade -> Trade.pp ppf trade
    | Quote quote -> Quote.pp ppf quote
end

let taq_data t ~contract =
  let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:false in
  dispatch_streaming_request t Tws_reqs.req_taq_data q >>| function
  | Error _ as x -> x
  | Ok (ticks, id) ->
    let filter_quote = unstage (Quote.make_filter ()) in
    let filter_trade = unstage (Trade.make_filter ()) in
    let taq_records = Pipe.filter_map ticks ~f:(fun tick ->
      match filter_quote tick with
      | Some quote -> Some (TAQ.quote quote)
      | None ->
        begin
          match filter_trade tick with
          | Some trade -> Some (TAQ.trade trade)
          | None -> None
        end)
    in
    Ok (taq_records, id)

let taq_data_exn t ~contract = taq_data t ~contract >>| Or_error.ok_exn

let cancel_taq_data t id =
  cancel_streaming_request t Tws_reqs.req_taq_data id

(* TAQ snapshots *)

module Quote_snapshot = struct
  type t =
    { symbol : Symbol.t;
      mutable ask_size : int;
      mutable bid_size : int;
      mutable ask_price : Price.t;
      mutable bid_price : Price.t;
    } with sexp, fields
end

let quote_snapshot t ~contract =
  let quote =
    { Quote_snapshot.
      symbol    = Contract.symbol contract;
      ask_size  = 0;
      bid_size  = 0;
      ask_price = Price.zero;
      bid_price = Price.zero;
    }
  in
  let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:true in
  dispatch_streaming_request t Tws_reqs.req_taq_snapshot q
  >>= function
  | Error _ as x -> return x
  | Ok (ticks, id) ->
    let module Type = Tick_price.Type in
    let num_ticks = 2 in
    Pipe.fold ticks ~init:(0, quote) ~f:(fun (counter, quote) tick ->
      let counter =
        match Tick_price.tick_type tick with
        | Type.Ask ->
          quote.Quote_snapshot.ask_size  <- Tick_price.size tick;
          quote.Quote_snapshot.ask_price <- Tick_price.price tick;
          counter + 1
        | Type.Bid ->
          quote.Quote_snapshot.bid_size  <- Tick_price.size tick;
          quote.Quote_snapshot.bid_price <- Tick_price.price tick;
          counter + 1
        | Type.Last | Type.Low | Type.High | Type.Close -> counter
      in
      if counter = num_ticks then begin
        cancel_streaming_request t Tws_reqs.req_taq_snapshot id;
        Pipe.close_read ticks
      end;
      return (counter, quote))
    >>| fun (_, quote) -> Ok quote

let quote_snapshot_exn t ~contract =
  quote_snapshot t ~contract >>| Or_error.ok_exn

module Trade_snapshot = struct
  type t =
    { symbol : Symbol.t;
      mutable last_size  : int;
      mutable last_price : Price.t;
    } with sexp, fields
end

let trade_snapshot t ~contract =
  let trade =
    { Trade_snapshot.
      symbol = Contract.symbol contract;
      last_size  = 0;
      last_price = Price.zero;
    }
  in
  let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:true in
  dispatch_streaming_request t Tws_reqs.req_taq_snapshot q
  >>= function
  | Error _ as x -> return x
  | Ok (ticks, id) ->
    let module Type = Tick_price.Type in
    let num_ticks = 1 in
    Pipe.fold ticks ~init:(0, trade) ~f:(fun (counter, trade) tick ->
      let counter =
        match Tick_price.tick_type tick with
        | Type.Last ->
          trade.Trade_snapshot.last_size  <- Tick_price.size  tick;
          trade.Trade_snapshot.last_price <- Tick_price.price tick;
          counter + 1
        | Type.Ask | Type.Bid | Type.Low | Type.High | Type.Close -> counter
      in
      if counter = num_ticks then begin
        cancel_streaming_request t Tws_reqs.req_taq_snapshot id;
        Pipe.close_read ticks
      end;
      return (counter, trade))
    >>| fun (_, trade) -> Ok trade

let trade_snapshot_exn t ~contract =
  trade_snapshot t ~contract >>| Or_error.ok_exn
