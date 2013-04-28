(* File: simulation_server.ml

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
open Ibx.Std
open Tws_prot

module Protocol = struct

  (* NOTE: The client version is used as a message tag to distinguish the
     client header from client queries, because the TWS protocol does not
     specify an extra message tag for the client header. *)
  let client_header_tag = Int.to_string Ibx.Std.Config.client_version

  module Query_id = struct
    include Unique_id.Int63 (struct end)
    let val_type = Val_type.create to_string of_string
  end

  module Client_message = struct
    module Query = struct
      type t =
        { tag     : Send_tag.t;
          version : int;
          id      : Query_id.t option;
          data    : string Queue.t;
        } with sexp
    end

    type t =
    | Client_header of int * Client_id.t
    | Client_query  of Query.t
    with sexp

    let of_tws raw_msg =
      let raw_tag = List.hd_exn raw_msg in
      let is_client_header =
        String.equal raw_tag client_header_tag
      in
      if is_client_header then
        match raw_msg with
        | client_version :: client_id :: [] ->
          Client_header (
            Int.of_string client_version,
            Client_id.of_string client_id
          )
        | _ ->
          failwiths "wrong client header format" raw_msg <:sexp_of< string list >>
      else
        let query =
          if Send_tag.corresponding_query_has_id (Send_tag.t_of_tws raw_tag) then
            match raw_msg with
            | tag :: version :: query_id :: data ->
              { Query.
                tag = Send_tag.t_of_tws tag;
                version = Int.of_string version;
                id = Some (Query_id.of_string query_id);
                data = Queue.of_list data;
              }
            | _ -> failwiths "wrong query format" raw_msg <:sexp_of< string list >>
          else
            match raw_msg with
            | tag :: version :: data ->
              { Query.
                tag = Send_tag.t_of_tws tag;
                version = Int.of_string version;
                id = None;
                data = Queue.of_list data;
              }
            | _ -> failwiths "wrong query format" raw_msg <:sexp_of< string list >>
        in
        Client_query query
  end

  module Server_message = struct
    module Response = struct
      type t =
        { tag      : Recv_tag.t;
          version  : int;
          query_id : Query_id.t option;
          data     : string;
        } with fields, sexp

      let pickler =
        Pickler.create ~name:"Response"
          Pickler.Spec.(
            wrap (
              Fields.fold
                ~init:(empty ())
                ~tag:(fields_value (required Recv_tag.val_type))
                ~version:(fields_value (required int))
                ~query_id:(fields_value (skipped_if_none Query_id.val_type))
                ~data:(fields_value tws_data))
              (fun { tag; version; query_id; data } ->
                `Args $ tag $ version $ query_id $ data))
    end

    type t =
    | Server_header of int * Time.t
    | Server_response of Response.t
    with sexp

    let to_tws = function
      | Server_header (server_version, connection_time) ->
        let pickler = Pickler.create ~name:"Server_header"
          Pickler.Spec.(
            wrap (empty () ++ value (required int) ++ value (required time))
              (fun (version, time) -> `Args $ version $ time))
        in
        Pickler.run pickler (server_version, connection_time)
      | Server_response response ->
        Pickler.run Response.pickler response
  end

  module Transport = struct
    type t =
      { reader : string Pipe.Reader.t;
        writer : Writer.t;
      }

    let create reader writer =
      let null_delim_pred = `Char '\000' in
      let read_one reader =
        Reader.read_until reader null_delim_pred ~keep_delim:false >>| function
        | `Eof
        | `Ok _ as x -> x
        | `Eof_without_delim s -> `Ok s
      in
      return {
        writer;
        reader = Reader.read_all reader read_one;
      }

    let close t = Writer.close t.writer >>| fun () -> Pipe.close_read t.reader

    let really_read reader ~len =
      Pipe.read_exactly reader ~num_values:len >>| function
      | `Eof -> `Eof
      | `Exactly result -> `Ok (Queue.to_list result)
      | `Fewer   result -> `Ok (Queue.to_list result)

    let read_version_and_query_id reader tag =
      let len = if Send_tag.corresponding_query_has_id tag then 2 else 1 in
      really_read reader ~len

    let read_body reader tag =
      let module S = Send_tag in
      let unimplemented x = Error.failwiths "unimplemented" x S.sexp_of_t in
      let read ~len = really_read reader ~len in
      let no_data = [] in
      match tag with
      | S.Market_data -> read ~len:14
      | S.Cancel_market_data -> return (`Ok no_data)
      | S.Submit_order -> read ~len:78
      | S.Cancel_order -> return (`Ok no_data)
      | S.Open_orders -> unimplemented S.Open_orders
      | S.Portfolio_data -> unimplemented S.Portfolio_data
      | S.Execution_reports -> read ~len:7
      | S.Contract_data -> read ~len:13
      | S.Market_depth -> read ~len:10
      | S.Cancel_market_depth -> return (`Ok no_data)
      | S.News_bulletins -> unimplemented S.News_bulletins
      | S.Cancel_news_bulletins -> unimplemented S.News_bulletins
      | S.Set_server_log_level -> read ~len:1
      | S.Auto_open_orders -> unimplemented S.Auto_open_orders
      | S.All_open_orders -> unimplemented S.All_open_orders
      | S.Managed_accounts -> unimplemented S.Managed_accounts
      | S.Financial_advisor -> unimplemented S.Financial_advisor
      | S.Replace_financial_advisor -> unimplemented S.Replace_financial_advisor
      | S.Historical_data -> read ~len:17
      | S.Exercise_options -> unimplemented S.Exercise_options
      | S.Scanner_subscription -> unimplemented S.Scanner_subscription
      | S.Cancel_scanner_subscription -> unimplemented S.Cancel_scanner_subscription
      | S.Scanner_parameters -> unimplemented S.Scanner_parameters
      | S.Cancel_historical_data -> return (`Ok no_data)
      | S.Server_time -> read ~len:1
      | S.Realtime_bars -> read ~len:13
      | S.Cancel_realtime_bars -> return (`Ok no_data)
      | S.Fundamental_data -> unimplemented S.Fundamental_data
      | S.Cancel_fundamental_data -> unimplemented S.Cancel_fundamental_data
      | S.Implied_volatility -> read ~len:13
      | S.Option_price -> read ~len:13
      | S.Cancel_implied_volatility -> return (`Ok no_data)
      | S.Cancel_option_price -> return (`Ok no_data)

    module Deferred_read_result : sig
      type 'a t = [ `Eof | `Ok of 'a ] Deferred.t
      include Monad.S with type 'a t := 'a t
    end = struct
      module M = struct
        type 'a t = [ `Eof | `Ok of 'a ] Deferred.t

        let bind t f = t >>= function
          | `Eof -> Deferred.return `Eof
          | `Ok x -> f x

        let return x = Deferred.return (`Ok x)
      end
      include M
      include Monad.Make (M)
    end
    let (>>=~) = Deferred_read_result.(>>=)
    let (>>|~) = Deferred_read_result.(>>|)

    let read t =
      Monitor.try_with ~name:"Transport reader" (fun () ->
        really_read t.reader ~len:1 >>=~ function
        | [] -> failwith "missing send tag"
        | raw_tag :: _ ->
          if String.equal raw_tag client_header_tag then
            really_read t.reader ~len:1 >>|~ function
            | [] -> failwith "missing client id"
            | client_id :: _ -> raw_tag :: client_id :: []
          else
            let tag = Send_tag.t_of_tws raw_tag in
            read_version_and_query_id t.reader tag
            >>=~ fun version_query_id ->
            read_body t.reader tag
            >>|~ fun body ->
            raw_tag :: version_query_id @ body
      ) >>| fun read_result ->
      match read_result with
      | Error exn -> raise exn
      | Ok `Eof -> `Eof
      | Ok (`Ok raw_msg) -> `Ok (Client_message.of_tws raw_msg)

    let write t msg = Writer.write t.writer (Server_message.to_tws msg)
    let flushed_time t = Writer.flushed_time t.writer
  end
end

module Message_generator = struct
  open Protocol

  let generate_messages clt_msg =
    let to_tws = Pickler.run in
    let module Query = Client_message.Query in
    let module Response = Server_message.Response in
    let module S = Send_tag in
    let module V = Recv_tag in
    let module E = Server_message in
    let module R = Ibx.Std.Response in
    match clt_msg with
    | Client_message.Client_header (_client_version, _client_id) ->
      [ E.Server_header (Ibx.Std.Config.server_version, Time.now ())
      ; E.Server_response {
        Response.
        tag      = V.Managed_accounts;
        version  = 1;
        query_id = None;
        data     = "DU15111\x00" }
      ; E.Server_response {
        Response.
        tag      = V.Next_order_id;
        version  = 1;
        query_id = None;
        data     = "1\x00" }
      ]

    | Client_message.Client_query query ->
      begin
        match query.Query.tag with

        (* ==================== Connection and server ==================== *)

        | S.Server_time ->
          let pickler = Only_in_test.force R.Server_time.pickler in
          [ E.Server_response {
            Response.
            tag      = V.Server_time;
            version  = 1;
            query_id = None;
            data     = to_tws pickler (Lazy.force Gen.server_time) }
          ]

        | S.Set_server_log_level -> []

        (* ======================== Market data ========================== *)

        | S.Market_data ->
          let market_data =  List.map (Lazy.force Gen.market_data) ~f:(function
            | `Tick_price x ->
              let pickler = Only_in_test.force R.Tick_price.pickler in
              E.Server_response {
                Response.
                tag      = V.Tick_price;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws pickler x;
              }
            | `Tick_size x ->
              let pickler = Only_in_test.force R.Tick_size.pickler in
              E.Server_response {
                Response.
                tag      = V.Tick_size;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws pickler x;
              }
            | `Tick_option x ->
              let pickler = Only_in_test.force R.Tick_option.pickler in
              E.Server_response {
                Response.
                tag      = V.Tick_option;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws pickler x;
              }
            | `Tick_string x ->
              let pickler = Only_in_test.force R.Tick_string.pickler in
              E.Server_response {
                Response.
                tag      = V.Tick_string;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws pickler x;
              })
          in
          let snapshot_ends = List.init 10 ~f:(fun _ ->
            E.Server_response {
              Response.
              tag      = V.Snapshot_end;
              version  = 1;
              query_id = query.Query.id;
              data     = "";
            })
          in
          List.fold snapshot_ends ~init:market_data ~f:Util.rand_insert

        | S.Cancel_market_data -> []

        | S.Option_price
        | S.Implied_volatility ->
          let pickler = Only_in_test.force R.Tick_option.pickler in
          [ E.Server_response {
            Response.
            tag      = V.Tick_option;
            version  = 6;
            query_id = query.Query.id;
            data     = to_tws pickler (Lazy.force Gen.tick_option) }
          ]

        | S.Cancel_option_price
        | S.Cancel_implied_volatility -> []

        (* =========================== Orders ============================ *)

        | S.Submit_order ->
          let pickler = Only_in_test.force R.Order_status.pickler in
          List.map (Lazy.force Gen.order_states) ~f:(fun x ->
            E.Server_response {
              Response.
              tag      = V.Order_status;
              version  = 6;
              query_id = query.Query.id;
              data     = to_tws pickler x;
            })

        | S.Cancel_order -> []

        | S.Open_orders -> []

        | S.Auto_open_orders -> []

        | S.All_open_orders -> []

        | S.Exercise_options -> []

        (* ========================== Account ============================ *)

        | S.Portfolio_data -> []

        (* ======================== Executions =========================== *)

        | S.Execution_reports ->
          List.map (Lazy.force Gen.execution_reports) ~f:(fun x ->
            let pickler = Only_in_test.force R.Execution_report.pickler in
            E.Server_response {
              Response.
              tag      = V.Execution_report;
              version  = 9;
              query_id = query.Query.id;
              data     = to_tws pickler x;
            }) @
            [ E.Server_response {
              Response.
              tag      = V.Execution_report_end;
              version  = 1;
              query_id = query.Query.id;
              data     = "";
            }]

        (* ======================= Contract specs ======================== *)

        | S.Contract_data ->
          let pickler = Only_in_test.force R.Contract_specs.pickler in
          [ E.Server_response {
            Response.
            tag      = V.Contract_data;
            version  = 8;
            query_id = query.Query.id;
            data     = to_tws pickler (Lazy.force Gen.contract_specs) }
          ; E.Server_response {
            Response.
            tag      = V.Contract_data_end;
            version  = 1;
            query_id = query.Query.id;
            data     = "" }
          ]

        (* ========================= Market depth ======================== *)

        | S.Market_depth ->
          List.map (Lazy.force Gen.book_updates) ~f:(fun x ->
            let pickler = Only_in_test.force R.Book_update.pickler in
            E.Server_response {
              Response.
              tag      = V.Book_update;
              version  = 1;
              query_id = query.Query.id;
              data     = to_tws pickler x;
            })

        | S.Cancel_market_depth -> []

        (* ======================= News bulletins ======================== *)

        | S.News_bulletins -> []

        | S.Cancel_news_bulletins -> []

        (* ===================== Financial advisors ====================== *)

        | S.Managed_accounts -> []

        | S.Financial_advisor -> []

        | S.Replace_financial_advisor -> []

        (* ======================= Market scanners ======================= *)

        | S.Scanner_subscription -> []

        | S.Cancel_scanner_subscription -> []

        | S.Scanner_parameters -> []

        (* ======================= Historical data ======================= *)

        | S.Historical_data ->
          let pickler = Only_in_test.force Historical_data.pickler in
          [ E.Server_response {
            Response.
            tag      = V.Historical_data;
            version  = 3;
            query_id = query.Query.id;
            data     = to_tws pickler (Lazy.force Gen.historical_data) }
          ]

        | S.Cancel_historical_data -> []

        (* ======================= Realtime data ========================= *)

        | S.Realtime_bars ->
          List.map (Lazy.force Gen.realtime_bars) ~f:(fun x ->
            let pickler = Only_in_test.force R.Realtime_bar.pickler in
            E.Server_response {
              Response.
              tag      = V.Realtime_bar;
              version  = 1;
              query_id = query.Query.id;
              data     = to_tws pickler x;
            })

        | S.Cancel_realtime_bars -> []

        (* ====================== Fundamental data ======================= *)

        | S.Fundamental_data -> []

        | S.Cancel_fundamental_data -> []
      end
end

module Server = Typed_tcp.Make (Protocol)

let start_on_port port =
  let server_is_ready = Ivar.create () in
  let monitor = Monitor.create ~name:"Simulation server" () in
  Stream.iter (Monitor.errors monitor) ~f:(fun exn ->
    Log.Global.error "Simulation_server: %s" (Exn.to_string (Monitor.extract_exn exn)));
  Scheduler.within ~monitor (fun () ->
    Server.create
      ~verbose:false
      ~log_disconnects:false
      ~auth:(fun _ _ -> return `Allow)
      ~port
      ()
    >>> fun server ->
    Ivar.fill server_is_ready ();
    let send_response (clt_id, clt_msg) =
      List.iter (Message_generator.generate_messages clt_msg)
        ~f:(Server.send_ignore_errors server clt_id)
    in
    Pipe.iter_without_pushback (Server.listen_ignore_errors server) ~f:send_response
    |! Deferred.don't_wait_for);
  Ivar.read server_is_ready
