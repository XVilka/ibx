(* File: client.ml

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

include struct
  open Response
  module Execution_report  = Execution_report
  module Commission_report = Commission_report
end

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

let is_connected t =
  match t.con with
  | `Disconnected
  | `Connecting _ -> false
  | `Connected _  -> true

let state t =
  match t.con with
  | `Disconnected -> `Disconnected
  | `Connecting _ -> `Connecting
  | `Connected  _ -> `Connected

let set_server_log_level t ~level =
  match t.con with
  | `Disconnected
  | `Connecting _  -> ()
  | `Connected con -> Ib.Connection.set_server_log_level con ~level

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

let dispatch_request t req query =
  match t.con with
  | `Disconnected
  | `Connecting _  -> return (Or_error.of_exn Not_connected_yet)
  | `Connected con -> Ib.Request.dispatch req con query

let dispatch_streaming_request t req query =
  match t.con with
  | `Disconnected
  | `Connecting _  -> return (Or_error.of_exn Not_connected_yet)
  | `Connected con -> Ib.Streaming_request.dispatch req con query

let cancel_streaming_request t req id =
  match t.con with
  | `Disconnected
  | `Connecting _  -> ()
  | `Connected con -> Ib.Streaming_request.cancel req con id

let dispatch_and_cancel t req query =
  dispatch_streaming_request t req query
  >>= function
  | Error _ as x -> return x
  | Ok (reader, id) ->
    Pipe.read_at_most reader ~num_values:1
    >>| fun read_result ->
    Exn.protectx read_result ~f:(function
    | `Eof -> Or_error.of_exn Eof_from_client
    | `Ok result -> Ok (Queue.dequeue_exn result)
    ) ~finally:(fun _ -> cancel_streaming_request t req id)
