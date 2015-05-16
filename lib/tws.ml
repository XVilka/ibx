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
    messages    : Client_msg.t Tail.t;
    exec_reader : Execution.t  Pipe.Reader.t;
    exec_writer : Execution.t  Pipe.Writer.t;
    comm_reader : Commission.t Pipe.Reader.t;
    comm_writer : Commission.t Pipe.Writer.t;
  }

let create
    ?(enable_logging = false)
    ?(client_id = Client_id.of_int_exn 0)
    ~host ~port () =
  let exec_reader, exec_writer = Pipe.create () in
  let comm_reader, comm_writer = Pipe.create () in
  return
    { client_id;
      enable_logging;
      remote_host     = host;
      remote_port     = port;
      client_version  = Config.client_version;
      con             = `Disconnected;
      server_version  = None;
      connection_time = None;
      account_code    = None;
      messages        = Tail.create ();
      exec_reader;
      exec_writer;
      comm_reader;
      comm_writer;
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
        ~extend_execution:(fun exec ->
          don't_wait_for (Pipe.write t.exec_writer exec))
        ~extend_commission:(fun comm ->
          don't_wait_for (Pipe.write t.comm_writer comm))
        (Reader.create fd)
        (Writer.create fd)
      >>= fun con ->
      let close_connection exn =
        t.con <- `Disconnected;
        Tail.extend t.messages (C.Error (Error.of_exn exn));
        Tail.close_exn t.messages;
        Pipe.close t.exec_writer;
        Pipe.close t.comm_writer;
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

let client_id       t = t.client_id
let server_version  t = t.server_version
let connection_time t = t.connection_time
let account_code    t = t.account_code
let messages        t = Tail.collect t.messages
let executions      t = t.exec_reader
let commissions     t = t.comm_reader

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
    Tail.close_exn t.messages;
    Pipe.close t.exec_writer;
    Pipe.close t.comm_writer;
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
  Ib.Streaming_request.(dispatch req con query
  >>= function
  | Error _ as e -> return e
  | Ok (reader, id) ->
    Pipe.read_at_most reader ~num_values:1
    >>| fun read_result ->
    Exn.protectx read_result ~f:(function
    | `Eof -> Or_error.of_exn Eof_from_client
    | `Ok result -> Ok (Queue.dequeue_exn result)
    ) ~finally:(fun _ -> cancel req con id)
  )

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
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_market_data t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_market_data con id
  )

let calc_option_price t ~contract ~volatility ~underlying_price =
  with_connection t ~f:(fun con ->
    let q = Query.Calc_option_price.create
      ~contract
      ~volatility
      ~underlying_price
    in
    dispatch_and_cancel Tws_reqs.req_calc_option_price con q >>| function
    | Error _ as x -> x
    | Ok result ->
      match result with
      | Error tws_error ->
        Error (Tws_error.to_error tws_error)
      | Ok opt_price ->
        if Price.is_nan opt_price then
          Error (Error.of_string "missing option price")
        else Ok opt_price
  )

let calc_option_price_exn t ~contract ~volatility ~underlying_price =
  calc_option_price t ~contract ~volatility ~underlying_price >>| Or_error.ok_exn

let calc_implied_volatility t ~contract ~option_price ~underlying_price =
  with_connection t ~f:(fun con ->
    let q = Query.Calc_implied_volatility.create
      ~contract
      ~option_price
      ~underlying_price
    in
    dispatch_and_cancel Tws_reqs.req_calc_implied_volatility con q >>| function
    | Error _ as x -> x
    | Ok result ->
      match result with
      | Error tws_error ->
        Error (Tws_error.to_error tws_error)
      | Ok implied_vol ->
        if Float.is_nan implied_vol then
          Error (Error.of_string "missing implied volatility")
        else Ok implied_vol
  )

let calc_implied_volatility_exn t ~contract ~option_price ~underlying_price =
  calc_implied_volatility t ~contract ~option_price ~underlying_price
  >>| Or_error.ok_exn

(* +-----------------------------------------------------------------------+
   | Orders                                                                |
   +-----------------------------------------------------------------------+ *)

let dedup_adjacents ~equal pipe =
  let last = ref None in
  Pipe.filter_map pipe ~f:(fun x ->
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
    | Ok (pipe, id) ->
      let equal t1 t2 = match t1, t2 with
        | Error e1, Error e2 -> Tws_error.(=) e1 e2
        | Ok x1, Ok x2 -> Order_status.(=) x1 x2
        | _, _ -> false
      in
      let oid = Order_id.of_int_exn (Query_id.to_int_exn id) in
      Ok (dedup_adjacents ~equal pipe, oid)
  )

let submit_order_exn t ~contract ~order =
  submit_order t ~contract ~order >>| function
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_order_status t oid =
  with_connection_unit t ~f:(fun con ->
    let id = Query_id.of_int_exn (Order_id.to_int_exn oid) in
    Ib.Streaming_request.cancel Tws_reqs.req_submit_order con id
  )

(* +-----------------------------------------------------------------------+
   | Account and portfolio                                                 |
   +-----------------------------------------------------------------------+ *)

let updates_gen t req create_query =
  with_connection t ~f:(fun con ->
    let code = Option.value_exn (account_code t) in
    let q = create_query ~subscribe:true ~account_code:code  in
    Ib.Streaming_request_without_id.(dispatch req con q >>| function
    | Error _ as e -> e
    | Ok pipe ->
      Pipe.filter_map pipe ~f:(function
      | `Update update -> Some update
      | `Update_end c  -> if Account_code.(code = c) then cancel req con; None
      ) |> Or_error.return
    )
  )

let account_updates t =
  updates_gen t
    Tws_reqs.req_account_updates
    Query.Account_updates.create

let account_updates_exn t = account_updates t >>| Or_error.ok_exn

let portfolio_positions t =
  updates_gen t
    Tws_reqs.req_portfolio_updates
    Query.Portfolio_positions.create

let portfolio_positions_exn t = portfolio_positions t >>| Or_error.ok_exn

(* +-----------------------------------------------------------------------+
   | Executions                                                            |
   +-----------------------------------------------------------------------+ *)

let filter_executions ?time t ~contract ~order_action =
  with_connection t ~f:(fun con ->
    let q = Query.Executions.create
      ~contract
      ~client_id:(client_id t)
      (* Note: [account_code t] won't return [None] since we
         can only call it in a handler of [Tws.with_client]. *)
      ~account_code:(Option.value_exn (account_code t))
      ~time:(Option.value time ~default:(Time.sub (Time.now ()) Time.Span.day))
      ~order_action
    in
    Ib.Streaming_request.(dispatch Tws_reqs.req_executions con q >>| function
    | Error _ as e -> e
    | Ok (pipe, id) ->
      Pipe.filter_map pipe ~f:(function
      | Error _ as e -> Some e
      | Ok (`Execution x)  -> Some (Ok x)
      | Ok `Executions_end -> cancel Tws_reqs.req_executions con id; None
      ) |> Or_error.return
    )
  )

let filter_executions_exn ?time t ~contract ~order_action =
  filter_executions ?time t ~contract ~order_action >>| function
  | Error e -> Error.raise e
  | Ok pipe -> Pipe.map pipe ~f:Tws_result.ok_exn

(* +-----------------------------------------------------------------------+
   | Contract details                                                      |
   +-----------------------------------------------------------------------+ *)

let contract_details t ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ?option_right ?expiry ?strike
    ~currency ~security_type symbol =
  with_connection t ~f:(fun con ->
    let q = Query.Contract_details.create
      ?contract_id ?multiplier ?listing_exchange ?local_symbol ?security_id
      ?include_expired ?exchange ?option_right ?expiry ?strike ~currency
      ~security_type symbol
    in
    Ib.Streaming_request.(dispatch Tws_reqs.req_contract_details con q >>| function
    | Error _ as e -> e
    | Ok (pipe, id) ->
      Pipe.filter_map pipe ~f:(function
      | Error _ as e -> Some e
      | Ok (`Contract_data x) -> Some (Ok x)
      | Ok `Contract_data_end -> cancel Tws_reqs.req_contract_details con id; None
      ) |> Or_error.return
    )
  )

let contract_details_exn t ?contract_id ?multiplier ?listing_exchange
    ?local_symbol ?security_id ?include_expired ?exchange ?option_right ?expiry
    ?strike ~currency ~security_type symbol =
  contract_details t
    ?contract_id ?multiplier ?listing_exchange ?local_symbol ?security_id
    ?include_expired ?exchange ?option_right ?expiry ?strike ~currency
    ~security_type symbol >>| function
    | Error e -> Error.raise e
    | Ok pipe -> Pipe.map pipe ~f:Tws_result.ok_exn

let sort_by_expiry chain =
  List.sort chain ~cmp:(fun c1 c2 ->
    let d1 = Option.value_exn (Contract.to_raw c1 |> Raw_contract.expiry) in
    let d2 = Option.value_exn (Contract.to_raw c2 |> Raw_contract.expiry) in
    Date.compare d1 d2
  )

let futures_chain t ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ~currency symbol =
  contract_details t
    ?contract_id ?multiplier ?listing_exchange ?local_symbol ?security_id
    ?include_expired ?exchange ~currency ~security_type:`Futures symbol
  >>= function
  | Error _ as e ->
    return e
  | Ok pipe ->
    try_with (fun () ->
      Pipe.to_list pipe >>| fun details ->
      let chain = List.map details ~f:(fun x ->
        Contract_data.contract (Tws_result.ok_exn x))
      in
      sort_by_expiry chain
    ) >>| fun result ->
    match result with
    | Ok _ as x -> x
    | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)

let futures_chain_exn t ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ~currency symbol =
  futures_chain t
    ?contract_id ?multiplier ?listing_exchange ?local_symbol ?security_id
    ?include_expired ?exchange ~currency symbol >>| Or_error.ok_exn

let group_by_expiry cs =
  List.group cs ~break:(fun c1 c2 ->
    let d1 = Option.value_exn (Contract.to_raw c1 |> Raw_contract.expiry) in
    let d2 = Option.value_exn (Contract.to_raw c2 |> Raw_contract.expiry) in
    Date.(d1 <> d2)
  )

let sort_by_strike cs =
  List.sort cs ~cmp:(fun c1 c2 ->
    let s1 = Option.value_exn (Contract.to_raw c1 |> Raw_contract.strike) in
    let s2 = Option.value_exn (Contract.to_raw c2 |> Raw_contract.strike) in
    Price.compare s1 s2
  )

let option_chain t ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ?expiry ?strike ~option_right
    ~currency symbol =
  contract_details t
    ?contract_id ?multiplier ?listing_exchange ?local_symbol ?security_id
    ?include_expired ?exchange ?expiry ?strike ~option_right ~currency
    ~security_type:`Option symbol
  >>= function
  | Error _ as e ->
    return e
  | Ok pipe ->
    try_with (fun () ->
      Pipe.to_list pipe >>| fun details ->
      let chain = List.map details ~f:(fun x ->
        Contract_data.contract (Tws_result.ok_exn x))
      in
      let flat_map = Fn.flip List.(>>=) in
      sort_by_expiry chain |> group_by_expiry |> flat_map sort_by_strike
    ) >>| fun result ->
    match result with
    | Ok _ as x -> x
    | Error exn -> Or_error.of_exn (Monitor.extract_exn exn)

let option_chain_exn t ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ?expiry ?strike ~option_right
    ~currency symbol =
  option_chain t
    ?contract_id ?multiplier ?listing_exchange ?local_symbol
    ?security_id ?include_expired ?exchange ?expiry ?strike ~option_right
    ~currency symbol >>| Or_error.ok_exn


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
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_market_depth t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_market_depth con id
  )

(* +-----------------------------------------------------------------------+
   | Historical data                                                       |
   +-----------------------------------------------------------------------+ *)

let historical_data
    ?(bar_size = `One_day)
    ?(bar_span = `Year 1)
    ?(use_rth = true)
    ?(show = `Trades)
    ?(until = Time.now ())
    t ~contract =
  with_connection t ~f:(fun con ->
    let q = Query.Historical_data.create ~contract ~end_date_time:until
      ~bar_size ~bar_span ~use_rth ~show in
    dispatch_and_cancel Tws_reqs.req_historical_data con q
  )

let historical_data_exn
    ?(bar_size = `One_day)
    ?(bar_span = `Year 1)
    ?(use_rth = true)
    ?(show = `Trades)
    ?(until = Time.now ())
    t ~contract =
  historical_data t ~bar_size ~bar_span ~use_rth ~show ~until ~contract
  >>| function
  | Error e -> Error.raise e
  | Ok result -> Tws_result.ok_exn result

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
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_realtime_bars t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_realtime_bars con id
  )

(* +-----------------------------------------------------------------------+
   | Trades                                                                |
   +-----------------------------------------------------------------------+ *)

module Trade = struct
  type t =
    { stamp : Time.t;
      price : Price.t;
      size  : Volume.t;
    } with sexp, fields

  let create ~price ~size =
    { stamp = Time.now (); price; size }

  let empty =
    { stamp = Time.epoch;
      price = Price.nan;
      size = Volume.zero
    }

  let pp ppf t =
    Format.fprintf ppf "T|%s|%4.2f|%4d"
      (Time.to_string t.stamp)
      (t.price :> float)
      (t.size :> int)
end

module Quote = struct
  module Change = struct
    type t =
    | Unknown
    | Ask_price of Price.t
    | Bid_price of Price.t
    | Ask_size of Volume.t
    | Bid_size of Volume.t
    | Ask_price_and_size of Price.t * Volume.t
    | Bid_price_and_size of Price.t * Volume.t
    with sexp

    let ask_size ~size_change =
      if Volume.(size_change <> zero) then
        Ask_size size_change
      else
        Unknown

    let bid_size ~size_change =
      if Volume.(size_change <> zero) then
        Bid_size size_change
      else
        Unknown

    let ask_price_and_size ~price_change ~size_change =
      if Price.is_nan price_change then
        Unknown
      else if Volume.(size_change <> zero) then
        Ask_price_and_size (price_change, size_change)
      else
        Ask_price price_change

    let bid_price_and_size ~price_change ~size_change =
      if Price.is_nan price_change then
        Unknown
      else if Volume.(size_change <> zero) then
        Bid_price_and_size (price_change, size_change)
      else
        Bid_price price_change
  end

  type t =
    { stamp : Time.t;
      ask_price : Price.t;
      bid_price : Price.t;
      ask_size : Volume.t;
      bid_size : Volume.t;
      change : Change.t;
    } with sexp, fields

  let empty =
    { stamp = Time.epoch;
      ask_price = Price.nan;
      ask_size = Volume.zero;
      bid_price = Price.nan;
      bid_size = Volume.zero;
      change = Change.Unknown;
    }

  let pp ppf t =
    Format.fprintf ppf "Q|%s|%4.2f|%4.2f|%4d|%4d"
      (Time.to_string t.stamp)
      (t.bid_price :> float)
      (t.ask_price :> float)
      (t.bid_size :> int)
      (t.ask_size :> int)

  let update_ask t ~size ~price =
    { t with
      stamp = Time.now ();
      ask_price = price;
      ask_size = size;
      change = Change.ask_price_and_size
        ~price_change:Price.(price - t.ask_price)
        ~size_change:Volume.(size - t.ask_size)
    }

  let update_bid t ~size ~price =
    { t with
      stamp = Time.now ();
      bid_size = size;
      bid_price = price;
      change = Change.bid_price_and_size
        ~price_change:Price.(price - t.bid_price)
        ~size_change:Volume.(size - t.bid_size)
    }

  let update_ask_size t ~size =
    { t with
      stamp = Time.now ();
      ask_size = size;
      change = Change.ask_size ~size_change:Volume.(size - t.ask_size);
    }

  let update_bid_size t ~size =
    { t with
      stamp = Time.now ();
      bid_size = size;
      change = Change.bid_size ~size_change:Volume.(size - t.bid_size);
    }
end

(* +-----------------------------------------------------------------------+
   | TAQ data                                                              |
   +-----------------------------------------------------------------------+ *)

module TAQ = struct
  type t =
  | Trade of Trade.t
  | Quote of Quote.t
  with sexp

  let pp ppf = function
    | Trade trade -> Trade.pp ppf trade
    | Quote quote -> Quote.pp ppf quote

  let get_snapshots tws ~contract =
    with_connection tws ~f:(fun con ->
      let q = Query.Market_data.create
        ~contract ~tick_generics:[] ~snapshot:false
      in
      Ib.Streaming_request.dispatch Tws_reqs.req_taq_data con q >>| function
      | Error _ as e -> e
      | Ok (ticks, id) ->
        let taq_data = Pipe.init (fun w ->
          Pipe.fold ticks ~init:(Trade.empty, Quote.empty)
            ~f:(fun ((trade, quote) as taq) tick ->
              match tick with
              | Error _ as e ->
                don't_wait_for (Pipe.write w e);
                return taq
              | Ok (`Tick_price tick) ->
                let module T = Tick_price.Type in
                return (match Tick_price.tick_type tick with
                | T.Ask ->
                  let price, size = Tick_price.(price tick, size tick) in
                  let quote = Quote.update_ask quote ~price ~size in
                  don't_wait_for (Pipe.write w (Ok (Quote quote)));
                  trade, quote
                | T.Bid ->
                  let price, size = Tick_price.(price tick, size tick) in
                  let quote = Quote.update_bid quote ~price ~size in
                  don't_wait_for (Pipe.write w (Ok (Quote quote)));
                  trade, quote
                | T.Last ->
                  let price, size = Tick_price.(price tick, size tick) in
                  let trade = Trade.create ~price ~size in
                  don't_wait_for (Pipe.write w (Ok (Trade trade)));
                  trade, quote
                | T.Open | T.High | T.Low | T.Close ->
                  taq
                )
              | Ok (`Tick_size tick) ->
                let module T = Tick_size.Type in
                return (match Tick_size.tick_type tick with
                | T.Ask ->
                  let size = Tick_size.size tick in
                  let quote = Quote.update_ask_size quote ~size in
                  don't_wait_for (Pipe.write w (Ok (Quote quote)));
                  trade, quote
                | T.Bid ->
                  let size = Tick_size.size tick in
                  let quote = Quote.update_bid_size quote ~size in
                  don't_wait_for (Pipe.write w (Ok (Quote quote)));
                  trade, quote
                | T.Last | T.Volume ->
                  taq
                )
            ) |> Deferred.ignore)
        in
        Ok (taq_data, id)
    )
end

let taq_data = TAQ.get_snapshots

let taq_data_exn t ~contract =
  taq_data t ~contract >>| function
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_taq_data t id =
  with_connection_unit t ~f:(fun con ->
    Ib.Streaming_request.cancel Tws_reqs.req_taq_data con id
  )

let trades t ~contract =
  taq_data t ~contract >>| function
  | Error _ as e  -> e
  | Ok (pipe, id) ->
    let trades = Pipe.filter_map pipe ~f:(function
      | Error _ as e -> Some e
      | Ok (TAQ.Trade trade) -> Some (Ok trade)
      | Ok (TAQ.Quote _) -> None
    )
    in
    Ok (trades, id)

let trades_exn t ~contract =
  trades t ~contract >>| function
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_trades = cancel_taq_data

let quotes t ~contract =
  taq_data t ~contract >>| function
  | Error _ as e  -> e
  | Ok (pipe, id) ->
    let quotes = Pipe.filter_map pipe ~f:(function
      | Error _ as e -> Some e
      | Ok (TAQ.Quote quote) -> Some (Ok quote)
      | Ok (TAQ.Trade _) -> None
    )
    in
    Ok (quotes, id)

let quotes_exn t ~contract =
  quotes t ~contract >>| function
  | Error e -> Error.raise e
  | Ok (pipe, id) -> Pipe.map pipe ~f:Tws_result.ok_exn, id

let cancel_quotes = cancel_taq_data


(* +-----------------------------------------------------------------------+
   | TAQ snapshots                                                         |
   +-----------------------------------------------------------------------+ *)

module Quote_snapshot = struct
  type t =
    { ask_size  : Volume.t;
      bid_size  : Volume.t;
      ask_price : Price.t;
      bid_price : Price.t;
    } with sexp, fields

  module Snapshot_result = struct
    type s =
    | Empty
    | With_bid of t
    | With_bid_and_ask of t
  end

  let get_snapshot tws ~contract =
    with_connection tws ~f:(fun con ->
      let q = Query.Market_data.create
        ~contract ~tick_generics:[] ~snapshot:true
      in
      Ib.Streaming_request.dispatch Tws_reqs.req_snapshot con q
      >>= function
      | Error _ as e -> return e
      | Ok (ticks, id) ->
        let module S = Snapshot_result in
        let module T = Tick_price.Type in
        let cancel = Ib.Streaming_request.cancel Tws_reqs.req_taq_data in
        try_with (fun () ->
          Pipe.fold_without_pushback ticks ~init:S.Empty ~f:(fun snapshot result ->
            match result with
            | Error tws_error ->
              cancel con id; Pipe.close_read ticks;
              Tws_error.raise tws_error
            | Ok `Snapshot_end ->
              cancel con id; Pipe.close_read ticks;
              snapshot
            | Ok (`Tick_price tick) ->
              match Tick_price.tick_type tick with
              | T.Bid ->
                begin match snapshot with
                | S.Empty ->
                  let price, size = Tick_price.(price tick, size tick) in
                  S.With_bid {
                    bid_price = Price.(if price = neg one then nan else price);
                    bid_size  = size;
                    ask_price = Price.nan;
                    ask_size  = Volume.zero;
                  }
                | S.With_bid _ | S.With_bid_and_ask _ as snapshot ->
                  snapshot
                end
              | T.Ask ->
                begin match snapshot with
                | S.With_bid snapshot ->
                  (* Received complete snapshot.  Cancel the request. *)
                  cancel con id; Pipe.close_read ticks;
                  let price, size = Tick_price.(price tick, size tick) in
                  S.With_bid_and_ask { snapshot with
                    ask_price = Price.(if price = neg one then nan else price);
                    ask_size  = size;
                  }
                | S.Empty | S.With_bid_and_ask _ as snapshot ->
                  snapshot
                end
              | T.Last | T.Low | T.High | T.Close | T.Open ->
                snapshot
          )
        ) >>| function
        | Error exn ->
          Or_error.of_exn (Monitor.extract_exn exn)
        | Ok S.Empty ->
          let name = Contract.to_string contract in
          Or_error.error_string (sprintf "No quote snapshot for %s" name)
        | Ok (S.With_bid_and_ask snapshot) ->
          Ok snapshot
        | Ok _ -> assert false
    )
end

let quote_snapshot = Quote_snapshot.get_snapshot
let quote_snapshot_exn t ~contract =
  quote_snapshot t ~contract >>| Or_error.ok_exn


module Trade_snapshot = struct
  type t =
    { size  : Volume.t;
      price : Price.t;
    } with sexp, fields

  module Snapshot_result = struct
    type s =
    | Empty
    | With_trade of t
  end

  let get_snapshot tws ~contract =
    with_connection tws ~f:(fun con ->
      let q = Query.Market_data.create
        ~contract ~tick_generics:[] ~snapshot:true
      in
      Ib.Streaming_request.dispatch Tws_reqs.req_snapshot con q
      >>= function
      | Error _ as e -> return e
      | Ok (ticks, id) ->
        let module S = Snapshot_result in
        let module T = Tick_price.Type in
        let cancel = Ib.Streaming_request.cancel Tws_reqs.req_snapshot in
        try_with (fun () ->
          Pipe.fold_without_pushback ticks ~init:S.Empty ~f:(fun snapshot result ->
            match result with
            | Error tws_error ->
              cancel con id; Pipe.close_read ticks;
              Tws_error.raise tws_error
            | Ok `Snapshot_end ->
              cancel con id; Pipe.close_read ticks;
              snapshot
            | Ok (`Tick_price tick) ->
              match Tick_price.tick_type tick with
              | T.Last ->
                begin match snapshot with
                | S.Empty ->
                  (* Received complete snapshot.  Cancel the request. *)
                  cancel con id; Pipe.close_read ticks;
                  let price, size = Tick_price.(price tick, size tick) in
                  S.With_trade { size; price }
                | S.With_trade _ as snapshot ->
                  snapshot
                end
              | T.Bid ->
                let price = Tick_price.price tick in
                if Price.(price = neg one) then begin
                  (* We won't receive a trade.  Cancel the request. *)
                  cancel con id; Pipe.close_read ticks;
                  S.Empty
                end else snapshot
              | T.Ask | T.Open | T.Low | T.High | T.Close ->
                snapshot
          )
        ) >>| function
        | Error exn ->
          Or_error.of_exn (Monitor.extract_exn exn)
        | Ok S.Empty ->
          let name = Contract.to_string contract in
          Or_error.error_string (sprintf "No trade snapshot for %s" name)
        | Ok (S.With_trade snapshot) ->
          Ok snapshot
    )
end

let trade_snapshot = Trade_snapshot.get_snapshot
let trade_snapshot_exn t ~contract =
  trade_snapshot t ~contract >>| Or_error.ok_exn


(* +-----------------------------------------------------------------------+
   | Close snapshot                                                        |
   +-----------------------------------------------------------------------+ *)

module Close_snapshot = struct
  type t =
    { price : Price.t
    } with sexp, fields

  module Snapshot_result = struct
    type s =
    | Empty
    | With_close of t
  end

  let get_snapshot tws ~contract =
    with_connection tws ~f:(fun con ->
      let q = Query.Market_data.create
        ~contract ~tick_generics:[] ~snapshot:true
      in
      Ib.Streaming_request.dispatch Tws_reqs.req_snapshot con q
      >>= function
      | Error _ as e -> return e
      | Ok (ticks, id) ->
        let module S = Snapshot_result in
        let module T = Tick_price.Type in
        let cancel = Ib.Streaming_request.cancel Tws_reqs.req_snapshot in
        try_with (fun () ->
          Pipe.fold_without_pushback ticks ~init:S.Empty ~f:(fun snapshot result ->
            match result with
            | Error tws_error ->
              cancel con id; Pipe.close_read ticks;
              Tws_error.raise tws_error
            | Ok `Snapshot_end ->
              cancel con id; Pipe.close_read ticks;
              snapshot
            | Ok (`Tick_price tick) ->
              match Tick_price.tick_type tick with
              | T.Close ->
                begin match snapshot with
                | S.Empty ->
                  (* Received complete snapshot.  Cancel the request. *)
                  cancel con id; Pipe.close_read ticks;
                  S.With_close { price = Tick_price.price tick }
                | S.With_close _ as snapshot ->
                  snapshot
                end
              | T.Bid | T.Ask | T.Open | T.Low | T.High | T.Last ->
                snapshot
          )
        ) >>| function
        | Error exn ->
          Or_error.of_exn (Monitor.extract_exn exn)
        | Ok S.Empty ->
          let name = Contract.to_string contract in
          Or_error.error_string (sprintf "No close snapshot for %s" name)
        | Ok (S.With_close snapshot) ->
          Ok snapshot
    )
end

let close_snapshot = Close_snapshot.get_snapshot
let close_snapshot_exn t ~contract =
  close_snapshot t ~contract >>| Or_error.ok_exn
