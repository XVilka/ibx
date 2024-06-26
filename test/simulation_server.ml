open Core
open Async
open Ibx
open Tws_prot

module Protocol = struct

  (* NOTE: The client version is used as a message tag to distinguish the
     client header from client queries, because the TWS protocol does not
     specify an extra message tag for the client header. *)
  let client_header_tag = Int.to_string Ibx.Config.client_version
  let account_code = Account_code.of_string "DU15133";

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
        } [@@deriving sexp]
    end

    type t =
      | Client_header of int * Client_id.t
      | Client_query  of Query.t
    [@@deriving sexp]

    let client_header_of_tws raw_msg =
      match raw_msg with
      | client_version :: client_id :: [] ->
        Client_header (
          Int.of_string client_version,
          Client_id.of_string client_id
        )
      | _ ->
        failwiths "wrong client header format" raw_msg [%sexp_of: string list ]

    let client_query_of_tws ~query_has_id raw_msg =
      let query =
        if query_has_id then
          match raw_msg with
          | tag :: version :: query_id :: data ->
            { Query.
              tag = Send_tag.t_of_tws tag;
              version = Int.of_string version;
              id = Some (Query_id.of_string query_id);
              data = Queue.of_list data;
            }
          | _ ->
            failwiths "wrong query format" raw_msg [%sexp_of: string list ]
        else
          match raw_msg with
          | tag :: version :: data ->
            { Query.
              tag = Send_tag.t_of_tws tag;
              version = Int.of_string version;
              id = None;
              data = Queue.of_list data;
            }
          | _ ->
            failwiths "wrong query format" raw_msg [%sexp_of: string list ]
      in
      Client_query query

    let of_tws raw_msg =
      let raw_tag = List.hd_exn raw_msg in
      let is_client_header = String.equal raw_tag client_header_tag in
      if is_client_header then
        client_header_of_tws raw_msg
      else
        let query_has_id =
          Send_tag.corresponding_query_has_id (Send_tag.t_of_tws raw_tag)
        in
        client_query_of_tws ~query_has_id raw_msg
  end

  module Server_message = struct
    module Response = struct
      type t =
        { tag      : Recv_tag.t;
          version  : int;
          query_id : Query_id.t option;
          data     : string;
        } [@@deriving fields, sexp]

      let encoder =
        Encoder.create ~name:"Simulation_server.Server_message"
          Encoder.Spec.(
            lift (
              Fields.fold
                ~init:(empty ())
                ~tag:(fields_value (required Recv_tag.val_type))
                ~version:(fields_value (required int))
                ~query_id:(fields_value (skipped_if_none Query_id.val_type))
                ~data:(fields_value tws_data))
              (fun { tag; version; query_id; data } ->
                 `Args $ tag $ version $ query_id $ data))
    end

    module Server_header = struct
      let encoder =
        Encoder.create ~name:"Simulation_server.Server_header"
          Encoder.Spec.(value (required int) ++ value (required time))
    end

    type t =
      | Server_header of int * Time_float_unix.t
      | Server_response of Response.t
    [@@deriving sexp]

    let to_tws = function
      | Server_header (version, conn_time) ->
        Encoder.run Server_header.encoder (version, conn_time)
      | Server_response response ->
        Encoder.run Response.encoder response
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
      | S.Account_data -> read ~len:2
      | S.Executions -> read ~len:7
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
      | S.History -> read ~len:17
      | S.Exercise_options -> unimplemented S.Exercise_options
      | S.Scanner_subscription -> unimplemented S.Scanner_subscription
      | S.Cancel_scanner_subscription -> unimplemented S.Cancel_scanner_subscription
      | S.Scanner_parameters -> unimplemented S.Scanner_parameters
      | S.Cancel_history -> return (`Ok no_data)
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

        let bind t ~f = t >>= function
        | `Eof  -> Deferred.return `Eof
        | `Ok a -> f a

        let map = `Custom (fun t ~f ->
          t >>= function
          | `Eof  -> Deferred.return `Eof
          | `Ok a -> Deferred.return (`Ok (f a)))

        let return x = Deferred.return (`Ok x)
      end
      include M
      include Monad.Make (M)
    end
    let (>>=~) = Deferred_read_result.(>>=)
    let (>>|~) = Deferred_read_result.(>>|)

    let read t =
      Monitor.try_with ~name:"Simulation_server.read" (fun () ->
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

  let (^@) s t = s^"\000"^t;;

  let generate_messages clt_msg =
    let to_tws = Encoder.run in
    let module Query = Client_message.Query in
    let module Response = Server_message.Response in
    let module S = Send_tag in
    let module V = Recv_tag in
    let module E = Server_message in
    let module R = Ibx.Response in
    match clt_msg with
    | Client_message.Client_header (_client_version, _client_id) ->
      [ E.Server_header (Ibx.Config.server_version, Time_float_unix.now ())
      ; E.Server_response {
          Response.
          tag      = V.Managed_accounts;
          version  = 1;
          query_id = None;
          data     = (Account_code.to_string account_code) ^@ "" }
      ; E.Server_response {
          Response.
          tag      = V.Next_order_id;
          version  = 1;
          query_id = None;
          data     = "1" ^@ "" }
      ]

    | Client_message.Client_query query ->
      begin
        match query.Query.tag with

        (* ==================== Connection and server ==================== *)

        | S.Server_time ->
          let encoder = Only_in_test.force R.Server_time.encoder in
          [ E.Server_response {
              Response.
              tag      = V.Server_time;
              version  = 1;
              query_id = None;
              data     = to_tws encoder (Lazy.force Gen.server_time) }
          ]

        | S.Set_server_log_level -> []

        (* ======================== Market data ========================== *)

        | S.Market_data ->
          let market_data =  List.map (Lazy.force Gen.market_data) ~f:(function
            | `Tick_price x ->
              let encoder = Only_in_test.force R.Tick_price.encoder in
              E.Server_response {
                Response.
                tag      = V.Tick_price;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws encoder x;
              }
            | `Tick_size x ->
              let encoder = Only_in_test.force R.Tick_size.encoder in
              E.Server_response {
                Response.
                tag      = V.Tick_size;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws encoder x;
              }
            | `Tick_option x ->
              let encoder = Only_in_test.force R.Tick_option.encoder in
              E.Server_response {
                Response.
                tag      = V.Tick_option;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws encoder x;
              }
            | `Tick_string x ->
              let encoder = Only_in_test.force R.Tick_string.encoder in
              E.Server_response {
                Response.
                tag      = V.Tick_string;
                version  = 6;
                query_id = query.Query.id;
                data     = to_tws encoder x;
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
          let encoder = Only_in_test.force R.Tick_option.encoder in
          [ E.Server_response {
              Response.
              tag      = V.Tick_option;
              version  = 6;
              query_id = query.Query.id;
              data     = to_tws encoder (Lazy.force Gen.tick_option) }
          ]

        | S.Cancel_option_price
        | S.Cancel_implied_volatility -> []

        (* =========================== Orders ============================ *)

        | S.Submit_order ->
          let encoder = Only_in_test.force R.Order_status.encoder in
          List.map (Lazy.force Gen.order_states) ~f:(fun x ->
            E.Server_response {
              Response.
              tag      = V.Order_status;
              version  = 6;
              query_id = query.Query.id;
              data     = to_tws encoder x;
            })

        | S.Cancel_order -> []

        | S.Open_orders -> []

        | S.Auto_open_orders -> []

        | S.All_open_orders -> []

        | S.Exercise_options -> []

        (* =================== Account and Portfolio ===================== *)

        | S.Account_data ->
          List.append
            (List.map (Lazy.force Gen.account_updates) ~f:(fun x ->
               let encoder = Only_in_test.force R.Account_update.encoder in
               E.Server_response {
                 Response.
                 tag      = V.Account_update;
                 version  = 2;
                 query_id = None;
                 data     = to_tws encoder x;
               }))
            (List.map (Lazy.force Gen.positions) ~f:(fun x ->
               let encoder = Only_in_test.force R.Position.encoder in
               E.Server_response {
                 Response.
                 tag      = V.Position;
                 version  = 7;
                 query_id = None;
                 data     = to_tws encoder x;
               })) @
          [ E.Server_response {
              Response.
              tag      = V.Account_download_end;
              version  = 1;
              query_id = query.Query.id;
              data     = (Account_code.to_string account_code) ^@ "";
            }]

        (* ======================== Executions =========================== *)

        | S.Executions ->
          List.map (Lazy.force Gen.executions) ~f:(fun x ->
            let encoder = Only_in_test.force R.Execution.encoder in
            E.Server_response {
              Response.
              tag      = V.Execution;
              version  = 9;
              query_id = query.Query.id;
              data     = to_tws encoder x;
            }) @
          [ E.Server_response {
              Response.
              tag      = V.Executions_end;
              version  = 1;
              query_id = query.Query.id;
              data     = "";
            }]

        (* ====================== Contract details ======================= *)

        | S.Contract_data ->
          List.map (Lazy.force Gen.contract_details) ~f:(fun x ->
            let encoder = Only_in_test.force R.Contract_data.encoder in
            E.Server_response {
              Response.
              tag      = V.Contract_data;
              version  = 8;
              query_id = query.Query.id;
              data     = to_tws encoder x
            }) @
          [ E.Server_response {
              Response.
              tag      = V.Contract_data_end;
              version  = 1;
              query_id = query.Query.id;
              data     = ""
            }]

        (* ========================= Market depth ======================== *)

        | S.Market_depth ->
          List.map (Lazy.force Gen.book_updates) ~f:(fun x ->
            let encoder = Only_in_test.force R.Book_update.encoder in
            E.Server_response {
              Response.
              tag      = V.Book_update;
              version  = 1;
              query_id = query.Query.id;
              data     = to_tws encoder x;
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

        (* =========================== History =========================== *)

        | S.History ->
          let encoder = Only_in_test.force History.encoder in
          [ E.Server_response {
              Response.
              tag      = V.History;
              version  = 3;
              query_id = query.Query.id;
              data     = to_tws encoder (Lazy.force Gen.history) }
          ]

        | S.Cancel_history -> []

        (* ======================= Realtime data ========================= *)

        | S.Realtime_bars ->
          List.map (Lazy.force Gen.realtime_bars) ~f:(fun x ->
            let encoder = Only_in_test.force R.Realtime_bar.encoder in
            E.Server_response {
              Response.
              tag      = V.Realtime_bar;
              version  = 3;
              query_id = query.Query.id;
              data     = to_tws encoder x;
            })

        | S.Cancel_realtime_bars -> []

        (* ====================== Fundamental data ======================= *)

        | S.Fundamental_data -> []

        | S.Cancel_fundamental_data -> []
      end
end

module Server = Typed_tcp.Make (Protocol) ()

let start_on_port port =
  let server_is_ready = Ivar.create () in
  let monitor = Monitor.create ~name:"Simulation server" () in
  Stream.iter (Monitor.detach_and_get_error_stream monitor) ~f:(fun exn ->
    Log.Global.error "Simulation_server: %s" (Exn.to_string (Monitor.extract_exn exn)));
  Scheduler.within ~monitor (fun () ->
    Server.create
      ~verbose:false
      ~log_disconnects:false
      ~auth:(fun _ _ _ -> return `Allow)
      ~port
      ()
    >>> fun server ->
    Ivar.fill server_is_ready ();
    let send_response (clt_id, clt_msg) =
      List.iter (Message_generator.generate_messages clt_msg)
        ~f:(Server.send_ignore_errors server clt_id)
    in
    Pipe.iter_without_pushback (Server.listen_ignore_errors server) ~f:send_response
    |> Deferred.don't_wait_for);
  Ivar.read server_is_ready
