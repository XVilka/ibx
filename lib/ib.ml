(* File: ib.ml

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

(* Parts of this module are inspired by the Rpc module of JaneSt's Async
   library. *)

open Core.Std
open Async.Std
open Std_internal
open Tws_prot

include struct
  open Response
  module Tws_error = Tws_error
  module Next_order_id = Next_order_id
  module Execution_report = Execution_report
  module Commission_report = Commission_report
end
module Server_log_level = Query.Server_log_level

module Query_id = struct
  include Unique_id.Int63 (struct end)
  let increase t num = of_int_exn (to_int_exn t + num)
  let val_type = Val_type.create to_string of_string
end

module Ibx_error = struct
  type t =
  | Connection_closed
  | Unexpected_eof
  | Read_error of Sexp.t
  | Parse_error of Sexp.t
  | Tws_error of string
  | Unknown_response_handler of Query_id.t * Recv_tag.t
  | Version_failure of int * Recv_tag.t
  | Unpickler_mismatch of Sexp.t * Recv_tag.t list
  | Uncaught_exn of Sexp.t
  with sexp
  exception Ibx of t with sexp
  let raise t = raise (Ibx t)

  let to_error = function
    | Tws_error s -> Error.of_string s
    | e -> Error.create "IBX" e sexp_of_t
end

module Ibx_result = struct
  type 'a t = ('a, Ibx_error.t) Result.t with sexp

  let make_try_with try_with (>>|) constructor f =
    try_with f >>| function
    | Ok _ as x -> x
    | Error exn -> Error (constructor (Exn.sexp_of_t exn))

  let try_with_read f = make_try_with
    Monitor.try_with
    (>>|)
    (fun e -> Ibx_error.Read_error e)
    f

  let try_with_unpickle f = make_try_with
    Result.try_with
    (|!)
    (fun e -> Ibx_error.Parse_error e)
    f

  let or_error = function
    | Ok _ as x -> x
    | Error (Ibx_error.Tws_error s) ->
      Or_error.error_string s
    | Error e ->
      Or_error.error "IBX" e Ibx_error.sexp_of_t
end

let to_tws p x = Pickler.run p x
let of_tws u x = Ibx_result.try_with_unpickle (fun () -> Unpickler.run_exn u x)

module Client_header = struct
  type t =
    { client_version : int;
      client_id      : Client_id.t;
    } with fields, sexp

  let create = Fields.create

  let pickler =
    Pickler.create
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~client_version:(fields_value (required int))
            ~client_id:(fields_value (required Client_id.val_type)))
          (fun { client_version; client_id } ->
            `Args $ client_version $ client_id))
end

module Server_header = struct
  type t =
    { server_version  : int;
      connection_time : Time.t;
    } with fields, sexp

  let unpickler =
    Unpickler.create ~name:"Server_header"
      Unpickler.Spec.(
        Fields.fold
          ~init:(empty ())
          ~server_version:(fields_value (required int))
          ~connection_time:(fields_value (required time)))
      (fun server_version connection_time ->
        { server_version; connection_time })
end

module Query = struct
  type t =
    { tag     : Send_tag.t;
      version : int;
      id      : Query_id.t option;
      data    : string;
    } with fields, sexp

  let pickler =
    Pickler.create
      Pickler.Spec.(
        wrap (
          Fields.fold
            ~init:(empty ())
            ~tag:(fields_value (required Send_tag.val_type))
            ~version:(fields_value (required int))
            ~id:(fields_value (skipped_if_none Query_id.val_type))
            ~data:(fields_value tws_data))
          (fun { tag; version; id; data } ->
            `Args $ tag $ version $ id $ data))
end

module Response_data = struct
  type t = ([ `Cancel | `Response of string Queue.t ]) Ibx_result.t with sexp
end

module Response = struct
  type t = {
    tag      : Recv_tag.t;
    version  : int;
    query_id : Query_id.t option;
    data     : Response_data.t;
  } with fields, sexp

  let pickler = Only_in_test.of_thunk (fun () ->
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
            let tws_data = match data with
              | Error _
              | Ok `Cancel -> assert false
              | Ok (`Response data) ->
                Queue.to_list data
                |! String.concat ~sep:"\000"
                |! Fn.flip (^) "\000"
            in
            `Args $ tag $ version $ query_id $ tws_data)))
end

module type Connection = sig
  type t

  val create
    :  ?enable_logging:bool
    -> extend_error:(Error.t -> unit)
    -> extend_status:(string -> unit)
    -> extend_execution_report:(Execution_report.t -> unit)
    -> extend_commission_report:(Commission_report.t -> unit)
    -> Reader.t
    -> Writer.t
    -> t Deferred.t

  val close  : t -> unit Deferred.t
  val closed : t -> unit Deferred.t
  val is_closed : t -> bool

  val set_server_log_level
    :  t
    -> level:[
    | `System
    | `Error
    | `Warning
    | `Information
    | `Detail
    ]
    -> unit

  module Handshake_result : sig
    type t =
    | Eof
    | Version_failure of int
    | Server_header of [ `Version of int ] * Time.t * Account_code.t
    with sexp
  end

  val try_connect
    :  t
    -> client_version:int
    -> client_id:Client_id.t
    -> Handshake_result.t Or_error.t Deferred.t
end

module Response_handler = struct
  type handler =
    Response.t
    -> [ `Keep
       | `Remove
       | `Replace of handler
       | `Die of Ibx_error.t ] Deferred.t

  type t = {
    tag  : Recv_tag.t;
    run  : handler;
  }

  let create ~tag ~run = { tag; run }
end

module type Connection_internal = sig
  include Connection

  val dispatch
    :  t
    -> handlers:Response_handler.t list
    -> Query.t
    -> (unit, [ `Closed ]) Result.t

  val cancel_streaming :
    ?query:Query.t
    -> t
    -> tags:Recv_tag.t list
    -> query_id:Query_id.t
    -> (unit, [ `Closed ]) Result.t

  val next_query_id : t -> Query_id.t Deferred.t
end

module Connection : Connection_internal = struct
  type t =
    { reader           : Reader.t;
      writer           : Writer.t;
      pipe_r           : string Pipe.Reader.t;
      open_queries     : (Query_id.t * Recv_tag.t, response_handler) Hashtbl.t;
      default_query_id : Query_id.t;
      next_order_id    : Order_id.t Ivar.t;
      account_code     : Account_code.t Ivar.t;
      stop             : unit Ivar.t;
      logfun           : logfun option;
      extend_error     : Error.t -> unit;
    }
  and response_handler = Response_handler.handler
  and logfun = [ `Send of Query.t | `Recv of Response.t ] -> unit

  let init_handler ?id t ~tag ~unpickler ~action ~f =
    Hashtbl.replace t.open_queries
      ~key:(Option.value id ~default:t.default_query_id, tag)
      ~data:(fun response ->
        match response.Response.data with
        | Error err ->
          return (`Die err)
        | Ok `Cancel ->
          assert false (* Response handler is not cancelable. *)
        | Ok (`Response data) ->
          begin
            match of_tws unpickler data with
            | Error err ->
              return (`Die err)
            | Ok response ->
              f response;
              return action
          end)

  let create
      ?(enable_logging = false)
      ~extend_error
      ~extend_status
      ~extend_execution_report
      ~extend_commission_report
      reader
      writer =
    let null_delimiter_pred = `Char '\000' in
    let read_one r =
      Reader.read_until r null_delimiter_pred ~keep_delim:false >>| function
      | `Eof -> `Eof
      | `Ok _ as x -> x
        (* We pretend that everything is Ok here and handle the error later. *)
      | `Eof_without_delim s -> `Ok s
    in
    let logfun send_recv =
      let tr_null s = String.tr s ~target:'\000' ~replacement:'|' in
      match send_recv with
      | `Send query ->
        let msg = to_tws Query.pickler query in
        Log.Global.debug ">> %s" (tr_null msg)
      | `Recv response ->
        let msg = to_tws (Only_in_test.force Response.pickler) response in
        Log.Global.debug "<< %s" (tr_null msg)
    in
    let t =
      {
        reader;
        writer;
        pipe_r           = Reader.read_all reader read_one;
        open_queries     = Hashtbl.Poly.create ~size:25 ();
        default_query_id = Query_id.of_int_exn (-1);
        next_order_id    = Ivar.create ();
        account_code     = Ivar.create ();
        stop             = Ivar.create ();
        logfun           = Option.some_if enable_logging logfun;
        extend_error;
      }
    in
    init_handler t
      ~tag:Recv_tag.Tws_error
      ~unpickler:Tws_error.unpickler
      ~action:`Keep
      ~f:(fun e -> extend_status ("TWS " ^ Tws_error.to_string_hum e));
    init_handler t
      ~tag:Recv_tag.Next_order_id
      ~unpickler:Next_order_id.unpickler
      ~action:`Remove
      ~f:(Ivar.fill t.next_order_id);
    init_handler t
      ~tag:Recv_tag.Managed_accounts
      ~unpickler:Account_code.unpickler
      ~action:`Remove
      ~f:(Ivar.fill_if_empty t.account_code);
    init_handler t
      ~tag:Recv_tag.Execution_report
      ~unpickler:Execution_report.unpickler
      ~action:`Keep
      ~f:extend_execution_report;
    init_handler t
      ~tag:Recv_tag.Commission_report
      ~unpickler:Commission_report.unpickler
      ~action:`Keep
      ~f:extend_commission_report;

    return t

  let is_closed t = Ivar.is_full t.stop
  let closed t = Ivar.read t.stop

  let close t =
    if not (is_closed t) then begin
      Ivar.fill t.stop ();
      Writer.close t.writer
      >>= fun () ->
      Reader.close t.reader
    end else Deferred.unit

  let writer t = if Ivar.is_full t.stop then Error `Closed else Ok t.writer

  let send_tws writer tws_pickler msg =
    Writer.write writer (to_tws tws_pickler msg)

  let send_query ?logfun writer query =
    begin match logfun with
    | None -> ()
    | Some f -> f (`Send query)
    end;
    send_tws writer Query.pickler query

  let really_read reader ~len =
    Pipe.read_exactly reader ~num_values:len >>| function
    | `Eof -> `Eof
    | `Fewer result   -> `Ok result
    | `Exactly result -> `Ok result

  let read_tws reader unpickler ~len =
    really_read reader ~len >>| function
    | `Eof -> `Eof
    | `Ok raw_msg -> `Ok (of_tws unpickler raw_msg)

  let read_version_and_query_id reader tag =
    if Recv_tag.corresponding_response_has_query_id tag then
      let unpickler = Unpickler.create ~name:"Version_id"
        Unpickler.Spec.(value (required int) ~name:"version"
          ++ value (optional Query_id.val_type) ~name:"query_id")
        (fun version query_id -> (version, query_id))
      in
      read_tws reader unpickler ~len:2
    else
      let unpickler = Unpickler.create ~name:"Version"
        Unpickler.Spec.(value (required int) ~name:"version")
        (fun version -> (version, None))
      in
      read_tws reader unpickler ~len:1

  let read_body reader tag =
    let module R = Recv_tag in
    let unimplemented x = Error.failwiths "unimplemented" x R.sexp_of_t in
    let read ~len = really_read reader ~len in
    let no_data = Queue.create () in
    Ibx_result.try_with_read (fun () ->
      match tag with
      | R.Tick_price -> read ~len:4
      | R.Tick_size -> read ~len:2
      | R.Order_status -> read ~len:9
      | R.Tws_error -> read ~len:2
      | R.Open_order -> read ~len:93
      | R.Account_update -> read ~len:1
      | R.Portfolio_update -> unimplemented R.Portfolio_update
      | R.Account_update_time -> unimplemented R.Account_update_time
      | R.Next_order_id -> read ~len:1
      | R.Contract_data -> read ~len:26
      | R.Execution_report -> read ~len:23
      | R.Book_update -> read ~len:5
      | R.Book_update_L2 -> read ~len:6
      | R.News_bulletins -> unimplemented R.News_bulletins
      | R.Managed_accounts -> read ~len:1
      | R.Financial_advisor -> unimplemented R.Financial_advisor
      | R.Historical_data ->
        begin
          read ~len:3
          >>= function
          | `Eof -> return `Eof
          | `Ok raw_msg ->
            let array = Queue.to_array raw_msg in
            let last = array.(2) in
            let num_bars = Int.of_string last in
            let num_fields = 9 in
            read ~len:(num_bars * num_fields) >>| function
            | `Eof -> `Eof
            | `Ok bars -> Queue.transfer ~src:bars ~dst:raw_msg; `Ok raw_msg
        end
      | R.Bond_contract_data -> unimplemented R.Bond_contract_data
      | R.Scanner_parameters -> unimplemented R.Scanner_parameters
      | R.Scanner_data -> unimplemented R.Scanner_data
      | R.Tick_option -> read ~len:9
      | R.Tick_generic -> read ~len:2
      | R.Tick_string -> read ~len:2
      | R.Tick_efp -> read ~len:8
      | R.Server_time -> read ~len:1
      | R.Realtime_bar -> read ~len:8
      | R.Fundamental_data -> unimplemented R.Fundamental_data
      | R.Contract_data_end -> return (`Ok no_data)
      | R.Open_order_end -> return (`Ok no_data)
      | R.Account_download_end -> unimplemented R.Account_download_end
      | R.Execution_report_end -> return (`Ok no_data)
      | R.Delta_neutral_validation -> unimplemented R.Delta_neutral_validation
      | R.Snapshot_end -> return (`Ok no_data)
      | R.Commission_report -> read ~len:6
    )

  let read_response reader =
    read_tws reader Recv_tag.unpickler ~len:1
    >>= function
    | `Eof -> return `Eof
    | `Ok (Error _) as x -> return x
    | `Ok (Ok tag) ->
      read_version_and_query_id reader tag
      >>= function
      | `Eof -> return `Eof
      | `Ok (Error _) as x -> return x
      | `Ok (Ok (version, query_id)) ->
        read_body reader tag
        >>| function
        | Error err -> `Ok (Error err)
        | Ok `Eof -> `Eof
        | Ok (`Ok data) ->
          let response =
            { Response.
              tag;
              version;
              query_id;
              data = Ok (`Response data);
            }
          in
          `Ok (Ok response)

  module Handshake_result = struct
    type t =
    | Eof
    | Version_failure of int
    | Server_header of [ `Version of int ] * Time.t * Account_code.t
    with sexp
  end

  let set_server_log_level t ~level =
    match writer t with
    | Error `Closed -> ()
    | Ok writer ->
      let log_level = Server_log_level.create ~level in
      let query =
        { Query.
          tag     = Send_tag.Set_server_log_level;
          version = 1;
          id      = None;
          data    = to_tws Server_log_level.pickler log_level;
        }
      in
      send_query ?logfun:t.logfun writer query

  let dispatch t ~handlers query =
    match writer t with
    | Error `Closed as x -> x
    | Ok writer ->
      send_query ?logfun:t.logfun writer query;
      let id = Option.value ~default:t.default_query_id query.Query.id in
      List.iter handlers ~f:(fun h ->
        let tag = h.Response_handler.tag in
        let run = h.Response_handler.run in
        Hashtbl.replace t.open_queries ~key:(id, tag) ~data:run);
      Ok ()

  let cancel_streaming ?query t ~tags ~query_id =
    match writer t with
    | Error `Closed as x -> x
    | Ok writer ->
      Option.iter query ~f:(send_query ?logfun:t.logfun writer);
      List.iter tags ~f:(fun tag ->
        match Hashtbl.find t.open_queries (query_id, tag) with
        | None -> ()
        | Some response_handler ->
          don't_wait_for (Deferred.ignore (
            response_handler
              { Response.
                tag;
                version  = 1;
                query_id = Some query_id;
                data     = Ok `Cancel;
              }))
      );
      Ok ()

  let next_query_id t =
    let new_id = Query_id.create () in
    Ivar.read t.next_order_id
    >>| fun oid -> Query_id.increase new_id (Raw_order.Id.to_int_exn oid)

  let handle_response t response =
    let id = Option.value ~default:t.default_query_id response.Response.query_id in
    let tag = response.Response.tag in
    match Hashtbl.find t.open_queries (id, tag) with
    | None ->
      return (`Stop (Ibx_error.Unknown_response_handler (id, tag)))
    | Some f  ->
      f response >>| fun action ->
      begin
        match action with
        | `Remove ->
          Hashtbl.remove t.open_queries (id, tag);
          `Continue
        | `Keep ->
          `Continue
        | `Replace handler ->
          Hashtbl.replace t.open_queries ~key:(id, tag) ~data:handler;
          `Continue
        | `Die err ->
          `Stop err
      end

  let handle_incoming t =
    let rec loop () =
      choose
        [ choice (Ivar.read t.stop) (fun () -> `Stop);
          choice (read_response t.pipe_r) (fun x  -> `Read x);
        ] >>> function
        | `Stop -> ()
        | `Read read_result ->
          match Deferred.peek (Ivar.read t.stop) with
          | Some () -> ()
          | None ->
            match read_result with
            | `Eof -> Ibx_error.raise Ibx_error.Unexpected_eof
            | `Ok (Error err) -> Ibx_error.raise err
            | `Ok (Ok response) ->
              begin
                match t.logfun with
                | None -> ()
                | Some f -> f (`Recv response)
              end;
              handle_response t response
              >>> function
              | `Continue -> loop ()
              | `Stop err -> Ibx_error.raise err
    in
    let monitor = Monitor.create ~name:"Connection loop" () in
    Stream.iter
      (Stream.interleave (Stream.of_list (
        [ Monitor.errors (Writer.monitor t.writer)
        ; Monitor.errors monitor
        ])))
      ~f:(fun exn ->
        don't_wait_for (close t);
        let error = match Monitor.extract_exn exn with
          | Ibx_error.Ibx err -> err
          | exn -> Ibx_error.Uncaught_exn (Exn.sexp_of_t exn)
        in
        Hashtbl.iter t.open_queries
          ~f:(fun ~key:(id, tag) ~data:response_handler ->
            don't_wait_for (Deferred.ignore (
              response_handler
                { Response.
                  tag;
                  version  = 1;
                  query_id = Some id;
                  data     = Error error
                })));
        t.extend_error (Ibx_error.to_error error));
    Scheduler.within ~monitor loop

  let try_connect t ~client_version ~client_id =
    let client_header =
      Client_header.create
        ~client_version
        ~client_id
    in
    begin match writer t with
    | Error `Closed ->
      return (Error Ibx_error.Connection_closed)
    | Ok writer ->
      send_tws writer Client_header.pickler client_header;
      read_tws t.pipe_r Server_header.unpickler ~len:2
      >>= function
      | `Eof ->
        don't_wait_for (close t);
        return (Ok Handshake_result.Eof)
      | `Ok (Error err) ->
        don't_wait_for (close t);
        return (Error err)
      | `Ok (Ok header) ->
        let server_version = header.Server_header.server_version in
        if not (server_version >= Config.server_version) then
          return (Ok (Handshake_result.Version_failure server_version))
        else begin
          handle_incoming t;  (* Start handling incoming messages. *)
          Ivar.read t.account_code
          >>| fun account_code ->
          Ok (Handshake_result.Server_header (
            `Version server_version,
            header.Server_header.connection_time,
            account_code
          ))
        end
    end >>| Ibx_result.or_error
end

module Header = struct
  type 'a t = {
    tag : 'a;
    version : int;
  }

  let create ~tag ~version = { tag; version; }
  let tag t = t.tag
end

module Request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t Header.t;
      recv_header  : Recv_tag.t Header.t;
      tws_query    : 'query    Tws_prot.Pickler.t;
      tws_response : 'response Tws_prot.Unpickler.t;
    }

  let create ~send_header ~recv_header ~tws_query ~tws_response =
    { send_header;
      recv_header;
      tws_query;
      tws_response;
    }

  let dispatch t con query =
    let ivar = Ivar.create () in
    let query =
      { Query.
        tag     = t.send_header.Header.tag;
        version = t.send_header.Header.version;
        id      = None;
        data    = to_tws t.tws_query query;
      }
    in
    let handler =
      Response_handler.create
        ~tag:t.recv_header.Header.tag
        ~run:(fun response ->
          if response.Response.version = t.recv_header.Header.version then
            match response.Response.data with
            | Error err as x ->
              (* If this handler died before, the ivar is already filled. *)
              Ivar.fill_if_empty ivar x;
              return (`Die err)
            | Ok `Cancel ->
              assert false (* Non-streaming requests are not cancelable. *)
            | Ok (`Response data) ->
              begin
                match of_tws t.tws_response data with
                | Error err as x ->
                  Ivar.fill ivar x;
                  return (`Die err)
                | Ok _response as x ->
                  Ivar.fill ivar x;
                  return `Remove
              end
          else begin
            let err = Ibx_error.Version_failure (
              response.Response.version,
              t.recv_header.Header.tag
            ) in
            Ivar.fill ivar (Error err);
            return (`Die err)
          end)
    in
    begin
      match Connection.dispatch con ~handlers:[handler] query with
      | Ok () -> ()
      | Error `Closed -> Ivar.fill ivar (Error Ibx_error.Connection_closed)
    end;
    Ivar.read ivar >>| Ibx_result.or_error

  let dispatch_exn t con query = dispatch t con query >>| Or_error.ok_exn
end

module Streaming_request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t Header.t;
      canc_header  : Send_tag.t Header.t option;
      recv_header  : Recv_tag.t Header.t list;
      skip_header  : Recv_tag.t Header.t list option;
      tws_query    : 'query    Tws_prot.Pickler.t;
      tws_response : 'response Tws_prot.Unpickler.t list;
    }

  module Id = Query_id

  let create ?canc_header ?skip_header
      ~send_header ~recv_header ~tws_query ~tws_response () =
    { send_header;
      canc_header;
      recv_header;
      skip_header;
      tws_query;
      tws_response;
    }

  let dispatch t con query =
    Connection.next_query_id con
    >>= fun query_id ->
    let ivar = Ivar.create () in
    let pipe_r, pipe_w = Pipe.create () in
    let query =
      { Query.
        tag     = t.send_header.Header.tag;
        version = t.send_header.Header.version;
        id      = Some query_id;
        data    = to_tws t.tws_query query;
      }
    in
    let error_handler =
      Response_handler.create
        ~tag:Recv_tag.Tws_error
        ~run:(fun response ->
          match response.Response.data with
          | Error err ->
            Pipe.close pipe_w;
            return (`Die err)
          | Ok `Cancel ->
            Pipe.close pipe_w;
            return `Remove
          | Ok (`Response data) ->
            begin
              match of_tws Tws_error.unpickler data with
              | Error err ->
                Pipe.close pipe_w;
                return (`Die err)
              | Ok response ->
                let err = Ibx_error.Tws_error (
                  "TWS Error " ^ Tws_error.to_string_hum response
                ) in
                if Ivar.is_empty ivar then begin
                  Ivar.fill ivar (Error err);
                  return `Remove
                end else
                  return (`Die err)
            end)
    in
    let skip_handlers =
      match t.skip_header with
      | None -> []
      | Some skip_headers ->
        List.map skip_headers ~f:(fun header ->
          Response_handler.create
            ~tag:header.Header.tag
            ~run:(fun response ->
              (* Note: Skip handlers don't check the message version. *)
              match response.Response.data with
              | Error err -> return (`Die err)
              | Ok `Cancel -> return `Remove
              | Ok (`Response _) -> return `Keep))
    in
    let data_handler_result = Result.try_with (fun () ->
      List.map2_exn t.recv_header t.tws_response
        ~f:(fun header unpickler ->
          Response_handler.create
            ~tag:header.Header.tag
            ~run:(fun response ->
              if response.Response.version = header.Header.version then
                let update pipe_w response =
                  match response.Response.data with
                  | Error err ->
                    Pipe.close pipe_w;
                    return (`Die err)
                  | Ok `Cancel ->
                    Pipe.close pipe_w;
                    return `Remove
                  | Ok (`Response data) ->
                    begin
                      match of_tws unpickler data with
                      | Error err ->
                        Pipe.close pipe_w;
                        return (`Die err)
                      | Ok response ->
                        if not (Pipe.is_closed pipe_w) then begin
                          (* We guard this write call to protect us against
                             incoming messages after a cancelation, causing
                             a write call to a closed pipe. *)
                          don't_wait_for (Pipe.write pipe_w response)
                        end;
                        return `Keep
                    end
                in
                match response.Response.data with
                | Error err as x ->
                  Pipe.close pipe_w;
                  Ivar.fill_if_empty ivar x;
                  return (`Die err)
                | Ok `Cancel ->
                  Pipe.close pipe_w;
                  return `Remove
                | Ok (`Response data) ->
                  begin
                    match of_tws unpickler data with
                    | Error err as x ->
                      Pipe.close pipe_w;
                      Ivar.fill_if_empty ivar x;
                      return (`Die err)
                    | Ok response ->
                      don't_wait_for (Pipe.write pipe_w response);
                      (* We fill the ivar only in the first iteration. *)
                      Ivar.fill_if_empty ivar (Ok (pipe_r, query_id));
                      return (`Replace (update pipe_w))
                  end
              else begin
                let err = Ibx_error.Version_failure (
                  response.Response.version,
                  header.Header.tag
                ) in
                Ivar.fill_if_empty ivar (Error err);
                return (`Die err)
              end)))
    in
    begin
      match data_handler_result with
      | Error exn ->
        let tags = List.map t.recv_header ~f:Header.tag in
        don't_wait_for (Connection.close con);
        Ivar.fill ivar (Error (Ibx_error.Unpickler_mismatch (Exn.sexp_of_t exn, tags)))
      | Ok data_handlers ->
        let handlers = error_handler :: (skip_handlers @ data_handlers) in
        match Connection.dispatch con ~handlers query with
        | Ok () -> Connection.closed con >>> fun () -> Pipe.close pipe_w
        | Error `Closed -> Ivar.fill ivar (Error Ibx_error.Connection_closed)
    end;
    Ivar.read ivar >>| Ibx_result.or_error

  let dispatch_exn t con query = dispatch t con query >>| Or_error.ok_exn

  let cancel t con query_id =
    let tags = Recv_tag.Tws_error :: (List.map t.recv_header ~f:Header.tag) in
    let result =
      match t.canc_header with
      | None -> Connection.cancel_streaming con ~tags ~query_id
      | Some header ->
        let query =
          { Query.
            tag     = header.Header.tag;
            version = header.Header.version;
            id      = Some query_id;
            data    = "";
          }
        in
        Connection.cancel_streaming con ~tags ~query_id ~query
    in
    ignore (result : (unit, [ `Closed ]) Result.t)
end

module Client = struct
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

  module Query_id  = Query_id

  type t =
    { remote_host    : string;
      remote_port    : int;
      client_id      : Client_id.t;
      client_version : int;
      enable_logging : bool;
      mutable con :
        [ `Disconnected
        | `Connecting of unit -> unit
        | `Connected of Connection.t ];
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

  exception Server_version_too_small of int * [ `Min of int ] with sexp

  let ignore_errors f = don't_wait_for (Monitor.try_with f >>| ignore)

  let connect t =
    let module C = Client_msg in
    let module E = Client_msg.Control in
    match Or_error.try_with (fun () -> Socket.create Socket.Type.tcp) with
    | Error err ->
      t.con <- `Disconnected;
      return (Error err)
    | Ok s ->
      let close_socket err =
        t.con <- `Disconnected;
        Tail.extend t.messages (C.Error err);
        ignore_errors (fun () -> Unix.close (Socket.fd s));
      in
      Tail.extend t.messages (C.Control (E.Connecting (
        `Client_version t.client_version,
        Host_and_port.create ~host:t.remote_host ~port:t.remote_port
      )));
      t.con <- `Connecting (fun () ->
        close_socket (Ibx_error.to_error Ibx_error.Connection_closed));
      Monitor.try_with ~name:"connect socket" (fun () ->
        Unix.Inet_addr.of_string_or_getbyname t.remote_host
        >>= fun inet_addr ->
        let address = Socket.Address.Inet.create inet_addr ~port:t.remote_port in
        Socket.connect s address)
      >>= function
      | Error exn ->
        let err = Error.of_exn (Monitor.extract_exn exn) in
        close_socket err;
        return (Error err)
      | Ok s ->
        let fd = Socket.fd s in
        Connection.create
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
        let close_connection err =
          t.con <- `Disconnected;
          Tail.extend t.messages (C.Error err);
          Connection.close con
        in
        Monitor.try_with ~name:"try connect" (fun () ->
          let module H = Connection.Handshake_result in
          Connection.try_connect con
            ~client_version:t.client_version
            ~client_id:t.client_id
          >>| function
          | Error e -> Error.raise e
          | Ok handshake_result ->
            begin match handshake_result with
            | H.Eof ->
              Error.raise (Ibx_error.to_error Ibx_error.Unexpected_eof)
            | H.Version_failure version ->
              raise (Server_version_too_small (Config.server_version, `Min version))
            | H.Server_header (`Version version, conn_time, account_code) ->
              t.con <- `Connected con;
              t.server_version  <- Some version;
              t.connection_time <- Some conn_time;
              t.account_code    <- Some account_code;
              Tail.extend t.messages (C.Control (
                E.Connected (`Server_version version, conn_time)));
            end)
        >>= function
        | Error exn ->
          let err = Error.of_exn (Monitor.extract_exn exn) in
          close_connection err >>| fun () ->
          Error err
        | Ok () as x -> return x

  let messages t = Tail.collect t.messages
  let execution_reports  t = Tail.collect t.execution_reports
  let commission_reports t = Tail.collect t.commission_reports

  let set_server_log_level t ~level =
    match t.con with
    | `Disconnected
    | `Connecting _  -> ()
    | `Connected con -> Connection.set_server_log_level con ~level

  let disconnect t =
    let module C = Client_msg in
    let module E = Client_msg.Control in
    match t.con with
    | `Disconnected -> return ()
    | `Connecting close -> return (close ())
    | `Connected con ->
      t.con <- `Disconnected;
      Tail.extend t.messages (C.Control E.Disconnected);
      Connection.close con

  let with_client
      ?enable_logging
      ?client_id
      ~host
      ~port
      ~on_handler_error
      handler =
    let module C = Client_msg in
    Monitor.try_with (fun () ->
      create ?enable_logging ?client_id ~host ~port ()
      >>= fun t ->
      if t.enable_logging then begin
        Stream.iter (messages t) ~f:(fun clt_msg ->
          match clt_msg with
          | C.Control x ->
            Log.Global.sexp ~level:`Info  x C.Control.sexp_of_t
          | C.Status x ->
            Log.Global.sexp ~level:`Info  x String.sexp_of_t
          | C.Error e ->
            Log.Global.sexp ~level:`Error e Error.sexp_of_t;
            Error.raise e)
      end else begin
        Stream.iter (messages t) ~f:(fun clt_msg ->
          match clt_msg with
          | C.Control _
          | C.Status  _ -> ()
          | C.Error e -> Error.raise e)
      end;
      Monitor.protect (fun () ->
        connect t
        >>= function
        | Error e -> Error.raise e
        | Ok () -> handler t
      ) ~finally:(fun () -> disconnect t)
    ) >>| fun result ->
    match result with
    | Ok () -> ()
    | Error e ->
      let e = Monitor.extract_exn e in
      begin
        match on_handler_error with
        | `Ignore -> ()
        | `Raise  -> raise e
        | `Call f -> f (Error.of_exn e)
      end

  let client_id       t = t.client_id
  let server_version  t = t.server_version
  let connection_time t = t.connection_time
  let account_code    t = t.account_code

  let is_connected t =
    match t.con with
    | `Disconnected
    | `Connecting _ -> false
    | `Connected _  -> true

  let state t = match t.con with
    | `Disconnected -> `Disconnected
    | `Connecting _ -> `Connecting
    | `Connected  _ -> `Connected

  let dispatch_request t req query =
    match t.con with
    | `Disconnected
    | `Connecting _  ->
      return (Error (Ibx_error.to_error Ibx_error.Connection_closed))
    | `Connected con -> Request.dispatch req con query

  let dispatch_streaming_request t req query =
    match t.con with
    | `Disconnected
    | `Connecting _  ->
      return (Error (Ibx_error.to_error Ibx_error.Connection_closed))
    | `Connected con -> Streaming_request.dispatch req con query

  let cancel_streaming_request t req id =
    match t.con with
    | `Disconnected
    | `Connecting _  -> ()
    | `Connected con -> Streaming_request.cancel req con id

  let dispatch_and_cancel t req query =
    dispatch_streaming_request t req query
    >>= function
    | Error _ as x -> return x
    | Ok (pipe_r, id) ->
      Pipe.read_at_most pipe_r ~num_values:1
      >>| fun read_result ->
      Exn.protectx read_result ~f:(function
      | `Eof -> Error (Ibx_error.to_error Ibx_error.Unexpected_eof)
      | `Ok result -> Ok (Queue.dequeue_exn result)
      ) ~finally:(fun _ -> cancel_streaming_request t req id)
end
