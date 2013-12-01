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

module Client_msg = struct
  module Control = struct
    type t =
    | Connecting of [ `Client_version of int ] * Host_and_port.t
    | Connected  of [ `Server_version of int ] * Time.t
    | Disconnected
    with sexp
  end

  type t =
  | Control of Control.t
  | Status  of string
  | Error   of Error.t
  with sexp
end

module Query_id = Ib.Streaming_request.Id

type t =
  { remote_host    : string;
    remote_port    : int;
    client_id      : Client_id.t;
    client_version : int;
    enable_logging : bool;
    mutable con :
      [ `Disconnected
      | `Connecting of unit -> unit
      | `Connected of Ib.Connection.t ];
    mutable server_version  : int option;
    mutable connection_time : Time.t option;
    mutable account_code    : Account_code.t option;
    messages           : Client_msg.t Tail.t;
    execution_reports  : Execution_report.t Tail.t;
    commission_reports : Commission_report.t Tail.t;
  }

let create
    ?(enable_logging = false)
    ?(client_id      = Client_id.of_int_exn 0)
    ~host
    ~port
    () =
  return
    { client_id;
      enable_logging;
      remote_host        = host;
      remote_port        = port;
      client_version     = Config.client_version;
      con                = `Disconnected;
      server_version     = None;
      connection_time    = None;
      account_code       = None;
      messages           = Tail.create ();
      execution_reports  = Tail.create ();
      commission_reports = Tail.create ();
    }

exception Not_connected_yet with sexp

exception Eof_from_client with sexp

exception Server_version_too_small of int * [ `Min of int ] with sexp

let ignore_errors f = don't_wait_for (Monitor.try_with f >>| ignore)

let connect t =
  let module C = Client_msg in
  let module E = Client_msg.Control in
  match Result.try_with (fun () -> Socket.create Socket.Type.tcp) with
  | Error exn ->
    Tail.extend t.messages (C.Error (Error.of_exn exn));
    t.con <- `Disconnected;
    return ()
  | Ok s ->
    let close_socket exn =
      t.con <- `Disconnected;
      Tail.extend t.messages (C.Error (Error.of_exn exn));
      ignore_errors (fun () -> Unix.close (Socket.fd s));
    in
    Tail.extend t.messages (C.Control (E.Connecting (
      `Client_version t.client_version,
      Host_and_port.create ~host:t.remote_host ~port:t.remote_port
    )));
    t.con <- `Connecting (fun () -> close_socket Not_connected_yet);
    Monitor.try_with ~name:"connect socket" (fun () ->
      Unix.Inet_addr.of_string_or_getbyname t.remote_host
      >>= fun inet_addr ->
      let address = Socket.Address.Inet.create inet_addr ~port:t.remote_port in
      Socket.connect s address)
    >>= function
    | Error exn ->
      close_socket (Monitor.extract_exn exn);
      return ()
    | Ok s ->
      let fd = Socket.fd s in
      Ib.Connection.create
        ~enable_logging:t.enable_logging
        ~extend_error:(fun e ->
          Tail.extend t.messages (C.Error e))
        ~extend_status:(fun s ->
          Tail.extend t.messages (C.Status s))
        ~extend_execution_report:(fun x ->
          Tail.extend t.execution_reports x)
        ~extend_commission_report:(fun x ->
          Tail.extend t.commission_reports x)
        (Reader.create fd)
        (Writer.create fd)
      >>= fun con ->
      let close_connection exn =
        t.con <- `Disconnected;
        Tail.extend t.messages (C.Error (Error.of_exn exn));
        Ib.Connection.close con
      in
      Monitor.try_with ~name:"try connect" (fun () ->
        let module H = Ib.Connection.Handshake_result in
        Ib.Connection.try_connect con
          ~client_version:t.client_version
          ~client_id:t.client_id
        >>| function
        | Error e -> Error.raise e
        | Ok handshake_result ->
          begin match handshake_result with
          | H.Eof ->
            raise Eof_from_client
          | H.Version_failure version ->
            raise (Server_version_too_small (version, `Min Config.server_version))
          | H.Server_header (`Version version, conn_time, account_code) ->
            t.con <- `Connected con;
            t.server_version  <- Some version;
            t.connection_time <- Some conn_time;
            t.account_code    <- Some account_code;
            Tail.extend t.messages (C.Control (
              E.Connected (`Server_version version, conn_time)));
          end)
      >>= function
      | Error exn -> close_connection (Monitor.extract_exn exn)
      | Ok () -> return ()

let messages t = Tail.collect t.messages
let execution_reports  t = Tail.collect t.execution_reports
let commission_reports t = Tail.collect t.commission_reports

let client_id       t = t.client_id
let server_version  t = t.server_version
let connection_time t = t.connection_time
let account_code    t = t.account_code

let is_connected t = match t.con with
  | `Disconnected
  | `Connecting _ -> false
  | `Connected _  -> true

let state t = match t.con with
  | `Disconnected -> `Disconnected
  | `Connecting _ -> `Connecting
  | `Connected  _ -> `Connected

let disconnect t =
  let module C = Client_msg in
  let module E = Client_msg.Control in
  match t.con with
  | `Disconnected -> return ()
  | `Connecting close -> return (close ())
  | `Connected con ->
    t.con <- `Disconnected;
    Tail.extend t.messages (C.Control E.Disconnected);
    Ib.Connection.close con

let with_client
    ?enable_logging
    ?client_id
    ~host
    ~port
    ~on_handler_error
    handler =
  let module C = Client_msg in
  let handle_error e =
    match on_handler_error with
    | `Ignore -> ()
    | `Raise  -> Error.raise e
    | `Call f -> f e
  in
  create ?enable_logging ?client_id ~host ~port ()
  >>= fun t ->
  Stream.iter (messages t) ~f:(fun clt_msg ->
    begin
      match clt_msg, t.enable_logging with
      | C.Control x, true ->
        Log.Global.sexp ~level:`Info x <:sexp_of< C.Control.t >>
      | C.Status x, true ->
        Log.Global.sexp ~level:`Info x <:sexp_of< string >>
      | C.Error e, true ->
        Log.Global.sexp ~level:`Error e <:sexp_of< Error.t >>;
        handle_error e
      | C.Error e, false ->
        handle_error e
      | _ -> ()
    end);
  connect t >>= fun () ->
  match state t with
  | `Connected ->
    Monitor.try_with (fun () -> handler t) >>= (function
    | Error exn ->
      disconnect t
      >>| fun () ->
      handle_error (Error.of_exn (Monitor.extract_exn exn));
    | Ok () ->
      disconnect t)
  | _ -> return ()

(* +-----------------------------------------------------------------------+
   | Helper functions                                                      |
   +-----------------------------------------------------------------------+ *)

let with_connection t ~f = match t.con with
  | `Disconnected
  | `Connecting _  -> return (Or_error.of_exn Not_connected_yet)
  | `Connected con -> f con

let with_connection_unit t ~f = match t.con with
  | `Disconnected
  | `Connecting _  -> ()
  | `Connected con -> f con

let dispatch_and_cancel req con query =
  Ib.Streaming_request.dispatch req con query
  >>= function
  | Error _ as e -> return e
  | Ok (reader, id) ->
    Pipe.read_at_most reader ~num_values:1
    >>| fun read_result ->
    Exn.protectx read_result ~f:(function
    | `Eof -> Or_error.of_exn Eof_from_client
    | `Ok result -> Ok (Queue.dequeue_exn result)
    ) ~finally:(fun _ -> Ib.Streaming_request.cancel req con id)

(* +-----------------------------------------------------------------------+
   | Connection and server                                                 |
   +-----------------------------------------------------------------------+ *)

let set_server_log_level t ~level = match t.con with
  | `Disconnected
  | `Connecting _  -> ()
  | `Connected con -> Ib.Connection.set_server_log_level con ~level

let server_time t =
  with_connection t ~f:(fun con ->
    let q = Query.Server_time.create () in
    Ib.Request.dispatch Tws_reqs.req_server_time con q
  )

let server_time_exn t = server_time t >>| Or_error.ok_exn

(* +-----------------------------------------------------------------------+
   | Market data                                                           |
   +-----------------------------------------------------------------------+ *)

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
  with_connection t ~f:(fun con ->
    let q = Query.Market_data.create ~contract ~tick_generics ~snapshot in
    Ib.Streaming_request.dispatch Tws_reqs.req_market_data con q
  )

let market_data_exn ?snapshot ?tick_generics t ~contract =
  market_data ?snapshot ?tick_generics t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_market_data t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_market_data con id
  )

let option_price t ~contract ~volatility ~underlying_price =
  with_connection t ~f:(fun con ->
    let q = Query.Option_price.create
      ~contract
      ~volatility
      ~underlying_price
    in
    dispatch_and_cancel Tws_reqs.req_option_price con q >>| function
    | Error _ as x -> x
    | Ok result ->
      match result with
      | Error tws_error -> Error (Tws_error.to_error tws_error)
      | Ok None -> Error (Error.of_string "missing option price")
      | Ok (Some opt_price) -> Ok opt_price
  )

let option_price_exn t ~contract ~volatility ~underlying_price =
  option_price t ~contract ~volatility ~underlying_price >>| Or_error.ok_exn

let implied_volatility t ~contract ~option_price ~underlying_price =
  with_connection t ~f:(fun con ->
    let q = Query.Implied_volatility.create
      ~contract
      ~option_price
      ~underlying_price
    in
    dispatch_and_cancel Tws_reqs.req_implied_volatility con q >>| function
    | Error _ as x -> x
    | Ok result ->
      match result with
      | Error tws_error -> Error (Tws_error.to_error tws_error)
      | Ok None -> Error (Error.of_string "missing implied volatility")
      | Ok (Some implied_vol) -> Ok implied_vol
  )

let implied_volatility_exn t ~contract ~option_price ~underlying_price =
  implied_volatility t ~contract ~option_price ~underlying_price
  >>| Or_error.ok_exn

(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

let dedup_adjacents ~equal pipe_r =
  let last = ref None in
  Pipe.filter_map pipe_r ~f:(fun x ->
    match !last with
    | None -> last := Some x; !last
    | Some y -> if equal x y then None else begin last := Some x; !last end)

let submit_order t ~contract ~order =
  with_connection t ~f:(fun con ->
    let q = Query.Submit_order.create
      ~contract
      ~order
    (* Note: [account_code t] won't return [None] since we
       can only call it in a handler of [Tws.with_client]. *)
      ~account_code:(Option.value_exn (account_code t))
    in
    Ib.Streaming_request.dispatch Tws_reqs.req_submit_order con q >>| function
    | Error _ as e -> e
    | Ok (pipe_r, id) ->
      let equal t1 t2 = match t1, t2 with
        | Error e1, Error e2 -> Tws_error.(=) e1 e2
        | Ok x1, Ok x2 -> Order_status.(=) x1 x2
        | _, _ -> false
      in
      let oid = Order_id.of_int_exn (Query_id.to_int_exn id) in
      Ok (dedup_adjacents ~equal pipe_r, oid)
  )

let submit_order_exn t ~contract ~order =
  submit_order t ~contract ~order >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_order_status t oid =
  with_connection_unit t ~f:(fun con ->
    let id = Query_id.of_int_exn (Order_id.to_int_exn oid) in
    Ib.Streaming_request.cancel Tws_reqs.req_submit_order con id
  )

(* +-----------------------------------------------------------------------+
   | Account and portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

let account_and_portfolio_updates t =
  with_connection t ~f:(fun con ->
    let account_code = Option.value_exn (account_code t) in
    let create_query = Query.Account_and_portfolio_updates.create ~account_code in
    let subscribe = create_query ~subscribe:true in
    Ib.Streaming_request_without_id.dispatch
      Tws_reqs.req_account_and_portfolio_updates con subscribe
    >>| function
    | Error _ as e -> e
    | Ok pipe_r ->
      let pipe_r = Pipe.filter_map pipe_r ~f:(function
        | `Account_update _
        | `Portfolio_update _ as x -> Some x
        | `Account_update_end code ->
          if Account_code.(=) account_code code then begin
            Ib.Streaming_request_without_id.cancel
              Tws_reqs.req_account_and_portfolio_updates
              con
          end;
          None)
      in
      Ok pipe_r
  )

let account_and_portfolio_updates_exn t =
  account_and_portfolio_updates t
  >>| Or_error.ok_exn

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

let filter_executions ?time t ~contract ~order_action =
  with_connection t ~f:(fun con ->
    let q = Query.Execution_reports.create
      ~contract
      ~client_id:(client_id t)
      (* Note: [account_code t] won't return [None] since we
         can only call it in a handler of [Tws.with_client]. *)
      ~account_code:(Option.value_exn (account_code t))
      ~time:(Option.value time ~default:(Time.sub (Time.now ()) Time.Span.day))
      ~order_action
    in
    Ib.Streaming_request.dispatch Tws_reqs.req_execution_reports con q
    >>| function
    | Error _ as e -> e
    | Ok (pipe_r, id) ->
      let pipe_r = Pipe.filter_map pipe_r ~f:(function
        | Error _ as x ->
          Some x
        | Ok `Execution_report data ->
          Some (Ok data)
        | Ok `Execution_report_end  ->
          Ib.Streaming_request.cancel Tws_reqs.req_execution_reports con id;
          None)
      in
      Ok pipe_r
  )

let filter_executions_exn ?time t ~contract ~order_action =
  filter_executions ?time t ~contract ~order_action >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok pipe_r -> Pipe.map pipe_r ~f:Tws_result.ok_exn

(* +-----------------------------------------------------------------------+
   | Contract details                                                      |
   +-----------------------------------------------------------------------+ *)

let contract_details t ~contract =
  with_connection t ~f:(fun con ->
    let q = Query.Contract_details.create ~contract in
    dispatch_and_cancel Tws_reqs.req_contract_details con q
  )

let contract_details_exn t ~contract =
  contract_details t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok result -> Tws_result.ok_exn result

(* +-----------------------------------------------------------------------+
   | Market depth                                                          |
   +-----------------------------------------------------------------------+ *)

let market_depth ?(num_rows = 10) t ~contract =
  with_connection t ~f:(fun con ->
    let q = Query.Market_depth.create ~contract ~num_rows in
    Ib.Streaming_request.dispatch Tws_reqs.req_market_depth con q
  )

let market_depth_exn ?num_rows t ~contract =
  market_depth ?num_rows t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_market_depth t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_market_depth con id
  )

(* +-----------------------------------------------------------------------+
   | Historical data                                                       |
   +-----------------------------------------------------------------------+ *)

let historical_data
    ?(bar_size = `One_hour)
    ?(duration = `Month 1)
    ?(use_rth = true)
    ?(show = `Trades)
    ?(until = Time.now ())
    t ~contract =
  with_connection t ~f:(fun con ->
    let q = Query.Historical_data.create ~contract ~end_date_time:until
      ~bar_size ~duration ~use_rth ~show in
    Ib.Streaming_request.dispatch Tws_reqs.req_historical_data con q
  )

let cancel_historical_data t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_historical_data con id
  )

(* +-----------------------------------------------------------------------+
   | Realtime bars                                                         |
   +-----------------------------------------------------------------------+ *)

let realtime_bars
    ?(bar_size = `Five_secs)
    ?(show = `Trades)
    ?(use_rth = true)
    t ~contract =
  with_connection t ~f:(fun con ->
    let q = Query.Realtime_bars.create ~contract ~bar_size ~show ~use_rth in
    Ib.Streaming_request.dispatch Tws_reqs.req_realtime_bars con q
  )

let realtime_bars_exn ?bar_size ?show ?use_rth t ~contract =
  realtime_bars ?bar_size ?show ?use_rth t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_realtime_bars t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_realtime_bars con id
  )

(* +-----------------------------------------------------------------------+
   | Trades                                                                |
   +-----------------------------------------------------------------------+ *)

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
  with_connection t ~f:(fun con ->
    let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:false in
    Ib.Streaming_request.dispatch Tws_reqs.req_taq_data con q >>| function
    | Error _ as x -> x
    | Ok (ticks, id) ->
      let filter_trade = unstage (Trade.make_filter ()) in
      let trades = Pipe.filter_map ticks ~f:(function
        | Error _ as x -> Some x
        | Ok tick ->
          match filter_trade tick with
          | None -> None
          | Some trade -> Some (Ok trade))
      in
      Ok (trades, id)
  )

let trades_exn t ~contract =
  trades t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_trades t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_taq_data con id
  )

(* +-----------------------------------------------------------------------+
   | Quotes                                                                |
   +-----------------------------------------------------------------------+ *)

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
  with_connection t ~f:(fun con ->
    let q = Query.Market_data.create ~contract ~tick_generics:[] ~snapshot:false in
    Ib.Streaming_request.dispatch Tws_reqs.req_taq_data con q >>| function
    | Error _ as x -> x
    | Ok (ticks, id) ->
      let filter_quote = unstage (Quote.make_filter ()) in
      let quotes = Pipe.filter_map ticks ~f:(function
        | Error _ as x -> Some x
        | Ok tick ->
          match filter_quote tick with
          | None -> None
          | Some quote -> Some (Ok quote))
      in
      Ok (quotes, id)
  )

let quotes_exn t ~contract =
  quotes t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_quotes t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_taq_data con id
  )

(* +-----------------------------------------------------------------------+
   | TAQ data                                                              |
   +-----------------------------------------------------------------------+ *)

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
  with_connection t ~f:(fun con ->
    let q = Query.Market_data.create
      ~contract
      ~tick_generics:[]
      ~snapshot:false
    in
    Ib.Streaming_request.dispatch Tws_reqs.req_taq_data con q >>| function
    | Error _ as x -> x
    | Ok (ticks, id) ->
      let filter_quote = unstage (Quote.make_filter ()) in
      let filter_trade = unstage (Trade.make_filter ()) in
      let taq_records = Pipe.filter_map ticks ~f:(function
        | Error _ as x -> Some x
        | Ok tick ->
          match filter_quote tick with
          | Some quote -> Some (Ok (TAQ.quote quote))
          | None ->
            begin
              match filter_trade tick with
              | Some trade -> Some (Ok (TAQ.trade trade))
              | None -> None
            end)
      in
      Ok (taq_records, id)
  )

let taq_data_exn t ~contract =
  taq_data t ~contract >>| function
  | Error e -> raise (Error.to_exn e)
  | Ok (pipe_r, id) -> Pipe.map pipe_r ~f:Tws_result.ok_exn, id

let cancel_taq_data t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_taq_data con id
  )

(* +-----------------------------------------------------------------------+
   | TAQ snapshots                                                         |
   +-----------------------------------------------------------------------+ *)

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
  with_connection t ~f:(fun con ->
    let q = Query.Market_data.create
      ~contract
      ~tick_generics:[]
      ~snapshot:true
    in
    Ib.Streaming_request.dispatch Tws_reqs.req_taq_snapshot con q
    >>= function
    | Error _ as x -> return x
    | Ok (ticks, id) ->
      let module Type = Tick_price.Type in
      let num_ticks = 2 in
      Monitor.try_with (fun () ->
        Pipe.fold ticks ~init:(0, quote) ~f:(fun (counter, quote) result ->
          match result with
          | Error tws_error ->
            Ib.Streaming_request.cancel Tws_reqs.req_taq_snapshot con id;
            Pipe.close_read ticks;
            raise (Tws_error.to_exn tws_error)
          | Ok tick ->
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
              Ib.Streaming_request.cancel Tws_reqs.req_taq_snapshot con id;
              Pipe.close_read ticks
            end;
            return (counter, quote)))
      >>| function
      | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)
      | Ok (_counter, quote) -> Ok quote
  )

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
  with_connection t ~f:(fun con ->
    let trade =
      { Trade_snapshot.
        symbol = Contract.symbol contract;
        last_size  = 0;
        last_price = Price.zero;
      }
    in
    let q = Query.Market_data.create
      ~contract
      ~tick_generics:[]
      ~snapshot:true
    in
    Ib.Streaming_request.dispatch Tws_reqs.req_taq_snapshot con q
    >>= function
    | Error _ as x -> return x
    | Ok (ticks, id) ->
      let module Type = Tick_price.Type in
      let num_ticks = 1 in
      Monitor.try_with (fun () ->
        Pipe.fold ticks ~init:(0, trade) ~f:(fun (counter, trade) result ->
          match result with
          | Error tws_error ->
            Ib.Streaming_request.cancel Tws_reqs.req_taq_snapshot con id;
            Pipe.close_read ticks;
            raise (Tws_error.to_exn tws_error)
          | Ok tick ->
            let counter =
              match Tick_price.tick_type tick with
              | Type.Last ->
                trade.Trade_snapshot.last_size  <- Tick_price.size  tick;
                trade.Trade_snapshot.last_price <- Tick_price.price tick;
                counter + 1
              | Type.Ask | Type.Bid | Type.Low | Type.High | Type.Close -> counter
            in
            if counter = num_ticks then begin
              Ib.Streaming_request.cancel Tws_reqs.req_taq_snapshot con id;
              Pipe.close_read ticks
            end;
            return (counter, trade)))
      >>| function
      | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)
      | Ok (_counter, trade) -> Ok trade
  )

let trade_snapshot_exn t ~contract =
  trade_snapshot t ~contract >>| Or_error.ok_exn
