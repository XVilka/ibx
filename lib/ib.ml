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

(* Parts of this module are inspired by Async's Rpc module. *)

open Core
open Async
open Tws_prot

module Tws_error = Response.Tws_error
module Execution = Response.Execution
module Commission = Response.Commission
module Server_log_level = Query.Server_log_level

module Query_id = struct
  include Unique_id.Int63 (struct end)
  let default = of_int_exn (-1)
  let increase t num = of_int_exn (to_int_exn t + num)
  let val_type = Val_type.create to_string of_string
end

module Header = struct
  type 'a t =
    { tag : 'a
    ; version : int
    } [@@deriving sexp]

  let create ~tag ~version = { tag; version }

  module R = Recv_tag
  let account_code = { tag = R.Managed_accounts ; version = 1 }
  let order_id     = { tag = R.Next_order_id    ; version = 1 }
  let tws_error    = { tag = R.Tws_error        ; version = 2 }
  let execution    = { tag = R.Execution        ; version = 9 }
  let commission   = { tag = R.Commission       ; version = 1 }
end

module Ibx_error = struct
  type t =
    | Connection_closed
    | Unexpected_eof
    | Read_error of Sexp.t
    | Parse_error of Sexp.t
    | Tws_error of string
    | Unknown_response_handler of Query_id.t * Recv_tag.t * [ `Version of int ]
    | Decoder_mismatch of Sexp.t * Recv_tag.t Header.t list
    | Uncaught_exn of Sexp.t
  [@@deriving sexp]
  exception Ibx of t [@@deriving sexp]
  let raise t = raise (Ibx t)
  let to_error t = Error.of_thunk (fun () -> Sexp.to_string_hum (sexp_of_t t))
end

module Ibx_result = struct
  type 'a t = ('a, Ibx_error.t) Result.t [@@deriving sexp]

  let make_try_with try_with (>>|) constructor f =
    try_with f >>| function
    | Ok _ as x -> x
    | Error exn -> Error (constructor (Exn.sexp_of_t exn))

  let try_with_read f =
    make_try_with
      Monitor.try_with
      (>>|)
      (fun e -> Ibx_error.Read_error e)
      f

  let try_with_decode f =
    make_try_with
      Result.try_with
      (fun x f -> x |> f)
      (fun e -> Ibx_error.Parse_error e)
      f

  let or_error = function
    | Ok _ as x -> x
    | Error err -> Error (Ibx_error.to_error err)
end

let to_tws e x = Encoder.run e x
let of_tws d x = Ibx_result.try_with_decode (fun () -> Decoder.run_exn d x)

module Client_header = struct
  type t =
    { client_version : int
    ; client_id      : Client_id.t
    } [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Client_header"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~client_version:(fields_value (required int))
            ~client_id:(fields_value (required Client_id.val_type)))
          (fun { client_version; client_id } ->
             `Args $ client_version $ client_id))
end

module Server_header = struct
  type t =
    { server_version  : int
    ; connection_time : Time.t
    } [@@deriving fields, sexp]

  let decoder =
    Decoder.create ~name:"Ib.Server_header"
      Decoder.Spec.(
        Fields.fold
          ~init:(empty ())
          ~server_version:(fields_value (required int))
          ~connection_time:(fields_value (required time)))
      (fun server_version connection_time ->
         { server_version; connection_time })
end

module Query = struct
  type t =
    { tag     : Send_tag.t
    ; version : int
    ; id      : Query_id.t option
    ; data    : string
    } [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Query"
      Encoder.Spec.(
        lift (
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
  type t = ([ `Cancel | `Response of string Queue.t ]) Ibx_result.t [@@deriving sexp]
end

module Response = struct
  type t =
    { tag      : Recv_tag.t
    ; version  : int
    ; query_id : Query_id.t option
    ; data     : Response_data.t
    } [@@deriving fields, sexp]

  let encoder =
    Encoder.create ~name:"Ib.Response"
      Encoder.Spec.(
        lift (
          Fields.fold
            ~init:(empty ())
            ~tag:(fields_value (required Recv_tag.val_type))
            ~version:(fields_value (required int))
            ~query_id:(fields_value (skipped_if_none Query_id.val_type))
            ~data:(fields_value tws_data))
          (fun { tag; version; query_id; data } ->
             let tws_data =
               match data with
               | Error _
               | Ok `Cancel -> assert false
               | Ok (`Response data) ->
                 Queue.to_list data
                 |> String.concat ~sep:"\000"
                 |> Fn.flip (^) "\000"
             in
             `Args $ tag $ version $ query_id $ tws_data))
end

module type Connection = sig
  type t

  val create
    :  ?do_logging:bool
    -> extend_error:(Error.t -> unit)
    -> extend_status:(string -> unit)
    -> extend_execution:(Execution.t -> unit)
    -> extend_commission:(Commission.t -> unit)
    -> Reader.t
    -> Writer.t
    -> t Deferred.t

  val close  : t -> unit Deferred.t
  val closed : t -> unit Deferred.t
  val is_closed : t -> bool

  val set_server_log_level
    :  t
    -> level:[ `System
             | `Error
             | `Warning
             | `Information
             | `Detail ]
    -> unit

  module Handshake_result : sig
    type t =
      | Eof
      | Version_failure of int
      | Server_header of [ `Version of int ] * Time.t * Account_code.t
    [@@deriving sexp]
  end

  val try_connect
    :  t
    -> client_version:int
    -> client_id:Client_id.t
    -> Handshake_result.t Deferred.Or_error.t
end

module Response_handler = struct
  type handler =
    Response.t
    -> [ `Keep
       | `Remove
       | `Replace of handler
       | `Die of Ibx_error.t ] Deferred.t

  type t =
    { tag     : Recv_tag.t
    ; version : int
    ; run     : handler
    }

  let create ~header ~run =
    { tag = header.Header.tag
    ; version = header.Header.version
    ; run
    }
end

module type Connection_internal = sig
  include Connection

  val dispatch
    :  t
    -> handlers:Response_handler.t list
    -> Query.t
    -> (unit, [ `Closed ]) Result.t

  val cancel_streaming
    :  ?query:Query.t
    -> t
    -> recv_header:Recv_tag.t Header.t list
    -> query_id:Query_id.t
    -> (unit, [ `Closed ]) Result.t

  val next_query_id : t -> Query_id.t Deferred.t
end

module Connection : Connection_internal = struct
  type t =
    { writer        : Writer.t
    ; reader        : string Pipe.Reader.t
    ; open_queries  : (Query_id.t * Recv_tag.t * version, response_handler) Hashtbl.t
    ; next_order_id : Order_id.t Ivar.t
    ; account_code  : Account_code.t Ivar.t
    ; stop          : unit Ivar.t
    ; logfun        : logfun option
    ; extend_error  : Error.t -> unit
    }
  and version = int
  and response_handler = Response_handler.handler
  and logfun = [ `Send of Query.t | `Recv of Response.t ] -> unit

  let init_handler t ~header ~decoder ~action ~f =
    let id = Query_id.default in
    let tag = header.Header.tag in
    let version = header.Header.version in
    Hashtbl.set t.open_queries ~key:(id, tag, version) ~data:(fun response ->
      match response.Response.data with
      | Error err ->
        return (`Die err)
      | Ok `Cancel ->
        assert false (* Response handler is not cancelable. *)
      | Ok (`Response data) ->
        begin
          match of_tws decoder data with
          | Error err ->
            return (`Die err)
          | Ok response ->
            f response;
            return action
        end)

  let create
      ?(do_logging = false)
      ~extend_error
      ~extend_status
      ~extend_execution
      ~extend_commission
      reader
      writer =
    let null_delim_pred = `Char '\000' in
    let read_one reader =
      Reader.read_until reader null_delim_pred ~keep_delim:false >>| function
      | `Eof -> `Eof
      | `Ok _ as x -> x
      | `Eof_without_delim s ->
        (* We pretend that everything is Ok here and handle the error later. *)
        `Ok s
    in
    let logfun send_recv =
      let tr_null s = String.tr s ~target:'\000' ~replacement:'|' in
      match send_recv with
      | `Send query ->
        let msg = to_tws Query.encoder query in
        Log.Global.debug ">> %s" (tr_null msg)
      | `Recv response ->
        let msg = to_tws Response.encoder response in
        Log.Global.debug "<< %s" (tr_null msg)
    in
    let t =
      { writer
      ; reader        = Reader.read_all reader read_one
      ; open_queries  = Hashtbl.Poly.create ~size:25 ()
      ; next_order_id = Ivar.create ()
      ; account_code  = Ivar.create ()
      ; stop          = Ivar.create ()
      ; logfun        = Option.some_if do_logging logfun
      ; extend_error
      }
    in
    init_handler t
      ~header:Header.account_code
      ~decoder:Account_code.decoder
      ~action:`Remove
      ~f:(Ivar.fill t.account_code);
    init_handler t
      ~header:Header.order_id
      ~decoder:Order_id.decoder
      ~action:`Remove
      ~f:(Ivar.fill t.next_order_id);
    init_handler t
      ~header:Header.tws_error
      ~decoder:Tws_error.decoder
      ~action:`Keep
      ~f:(fun e -> extend_status (Tws_error.to_string_hum e));
    init_handler t
      ~header:Header.execution
      ~decoder:Execution.decoder
      ~action:`Keep
      ~f:extend_execution;
    init_handler t
      ~header:Header.commission
      ~decoder:Commission.decoder
      ~action:`Keep
      ~f:extend_commission;
    return t

  let next_query_id t =
    let new_id = Query_id.create () in
    let%map oid = Ivar.read t.next_order_id in
    Query_id.increase new_id (Order_id.to_int_exn oid)

  let is_closed t = Ivar.is_full t.stop
  let closed t = Ivar.read t.stop

  let close t =
    if not (is_closed t) then begin
      Ivar.fill t.stop ();
      Writer.close t.writer
      >>| fun () ->
      Pipe.close_read t.reader
    end else Deferred.unit

  let send_tws writer encoder msg = Writer.write writer (to_tws encoder msg)

  let send_query ?logfun writer query =
    begin
      match logfun with
      | None -> ()
      | Some f -> f (`Send query)
    end;
    send_tws writer Query.encoder query

  let really_read reader ~len =
    Pipe.read_exactly reader ~num_values:len >>| function
    | `Eof -> `Eof
    | `Fewer   result -> `Ok result
    | `Exactly result -> `Ok result

  let read_tws reader decoder ~len =
    really_read reader ~len >>| function
    | `Eof -> Ok `Eof
    | `Ok raw_msg ->
      begin
        match of_tws decoder raw_msg with
        | Error _ as x -> x
        | Ok x -> Ok (`Ok x)
      end

  module Version_id = struct
    let decoder =
      Decoder.create ~name:"Ib.Version_id"
        Decoder.Spec.(
          value (required int) ~name:"version"
          ++ value (required Query_id.val_type) ~name:"id"
        )
        (fun version id -> (version, Some id))
  end

  module Version = struct
    let decoder =
      Decoder.create ~name:"Ib.Version"
        Decoder.Spec.(
          value (required int) ~name:"version"
        )
        (fun version -> (version, None))
  end

  let read_version_and_optional_id reader tag =
    if Recv_tag.corresponding_response_has_query_id tag
    then read_tws reader Version_id.decoder ~len:2
    else read_tws reader Version.decoder ~len:1

  let read_body reader tag =
    let module R = Recv_tag in
    let unimplemented x = Error.failwiths "unimplemented" x R.sexp_of_t in
    let empty_read = return (`Ok (Queue.create ())) in
    let read ~len = really_read reader ~len in
    Ibx_result.try_with_read (fun () ->
      match tag with
      | R.Tick_price -> read ~len:4
      | R.Tick_size -> read ~len:2
      | R.Order_status -> read ~len:9
      | R.Tws_error -> read ~len:2
      | R.Open_order -> read ~len:93
      | R.Account_update -> read ~len:4
      | R.Position -> read ~len:17
      | R.Account_update_time -> read ~len:1
      | R.Next_order_id -> read ~len:1
      | R.Contract_data -> read ~len:26
      | R.Execution -> read ~len:23
      | R.Book_update -> read ~len:5
      | R.Book_update_L2 -> read ~len:6
      | R.News_bulletins -> unimplemented R.News_bulletins
      | R.Managed_accounts -> read ~len:1
      | R.Financial_advisor -> unimplemented R.Financial_advisor
      | R.History ->
        begin
          read ~len:3 >>= function
          | `Eof -> return `Eof
          | `Ok raw_msg ->
            let array = Queue.to_array raw_msg in
            let last = array.(2) in
            let num_bars = Int.of_string last in
            let num_fields = 9 in
            read ~len:(num_bars * num_fields) >>| function
            | `Eof -> `Eof
            | `Ok bars ->
              Queue.blit_transfer ~src:bars ~dst:raw_msg ();
              `Ok raw_msg
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
      | R.Contract_data_end -> empty_read
      | R.Open_order_end -> empty_read
      | R.Account_download_end -> read ~len:1
      | R.Executions_end -> empty_read
      | R.Delta_neutral_validation -> unimplemented R.Delta_neutral_validation
      | R.Snapshot_end -> empty_read
      | R.Commission -> read ~len:6
    )

  module Deferred_read_result : sig
    type 'a t = [ `Eof | `Ok of 'a ] Ibx_result.t Deferred.t
    include Monad.S with type 'a t := 'a t
  end = struct
    module M = struct
      type 'a t = [ `Eof | `Ok of 'a ] Ibx_result.t Deferred.t

      let bind t ~f = t >>= function
      | Error _ as e -> Deferred.return e
      | Ok `Eof as x -> Deferred.return x
      | Ok (`Ok a)   -> f a

      let map = `Custom (fun t ~f ->
        t >>= function
        | Error _ as e -> Deferred.return e
        | Ok `Eof as x -> Deferred.return x
        | Ok (`Ok a)   -> Deferred.return (Ok (`Ok (f a))))

      let return x = Deferred.return (Ok (`Ok x))
    end
    include M
    include Monad.Make (M)
  end
  let (>>=~) = Deferred_read_result.(>>=)
  let (>>|~) = Deferred_read_result.(>>|)

  let read_response reader =
    read_tws reader Recv_tag.decoder ~len:1
    >>=~ fun tag ->
    read_version_and_optional_id reader tag
    >>=~ fun (version, id) ->
    read_body reader tag
    >>|~ fun data ->
    { Response.
      tag
    ; version
    ; query_id = id
    ; data = Ok (`Response data)
    }

  let writer t = if Ivar.is_full t.stop then Error `Closed else Ok t.writer

  let set_server_log_level t ~level =
    match writer t with
    | Error `Closed -> ()
    | Ok writer ->
      let log_level = Server_log_level.create ~level in
      let query =
        { Query.
          tag     = Send_tag.Set_server_log_level
        ; version = 1
        ; id      = None
        ; data    = to_tws Server_log_level.encoder log_level;
        }
      in
      send_query ?logfun:t.logfun writer query

  let dispatch t ~handlers query =
    match writer t with
    | Error `Closed as x -> x
    | Ok writer ->
      send_query ?logfun:t.logfun writer query;
      let id = Option.value query.Query.id ~default:Query_id.default in
      List.iter handlers ~f:(fun h ->
        let tag = h.Response_handler.tag in
        let version = h.Response_handler.version in
        let run = h.Response_handler.run in
        Hashtbl.set t.open_queries ~key:(id, tag, version) ~data:run
      );
      Ok ()

  let cancel_streaming ?query t ~recv_header ~query_id =
    match writer t with
    | Error `Closed as x -> x
    | Ok writer ->
      begin match query with
        | None -> ()
        | Some query -> send_query ?logfun:t.logfun writer query
      end;
      List.iter recv_header ~f:(fun header ->
        let tag = header.Header.tag in
        let version = header.Header.version in
        match Hashtbl.find t.open_queries (query_id, tag, version) with
        | None -> ()
        | Some response_handler ->
          don't_wait_for (Deferred.ignore (
            response_handler
              { Response.
                tag
              ; version
              ; query_id = Some query_id
              ;  data = Ok `Cancel
              }))
      );
      Ok ()

  let handle_response t response =
    let id = Option.value response.Response.query_id ~default:Query_id.default in
    let tag = response.Response.tag in
    let version = response.Response.version in
    let key = (id, tag, version) in
    match Hashtbl.find t.open_queries key with
    | None ->
      return (`Stop (Ibx_error.Unknown_response_handler (id, tag, `Version version)))
    | Some f  ->
      f response >>| fun action ->
      begin
        match action with
        | `Remove ->
          Hashtbl.remove t.open_queries key;
          `Continue
        | `Keep ->
          `Continue
        | `Replace handler ->
          Hashtbl.set t.open_queries ~key ~data:handler;
          `Continue
        | `Die err ->
          `Stop err
      end

  let handle_incoming t =
    let rec loop () =
      choose
        [ choice (Ivar.read t.stop) (fun () -> `Stop);
          choice (read_response t.reader) (fun x  -> `Read x);
        ] >>> function
      | `Stop -> ()
      | `Read read_result ->
        match Deferred.peek (Ivar.read t.stop) with
        | Some () -> ()
        | None ->
          match read_result with
          | Error err -> Ibx_error.raise err
          | Ok `Eof -> Ibx_error.raise Ibx_error.Unexpected_eof
          | Ok (`Ok response) ->
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
         [ Monitor.detach_and_get_error_stream (Writer.monitor t.writer)
         ; Monitor.detach_and_get_error_stream monitor
         ])))
      ~f:(fun exn ->
        don't_wait_for (close t);
        let error = match Monitor.extract_exn exn with
          | Ibx_error.Ibx err -> err
          | exn -> Ibx_error.Uncaught_exn (Exn.sexp_of_t exn)
        in
        Hashtbl.iteri t.open_queries
          ~f:(fun ~key:(id, tag, version) ~data:response_handler ->
            don't_wait_for (Deferred.ignore (
              response_handler
                { Response.
                  tag
                ; version
                ; query_id = Some id
                ; data = Error error
                })));
        t.extend_error (Ibx_error.to_error error));
    Scheduler.within ~monitor loop

  module Handshake_result = struct
    type t =
      | Eof
      | Version_failure of int
      | Server_header of [ `Version of int ] * Time.t * Account_code.t
    [@@deriving sexp]
  end

  let try_connect t ~client_version ~client_id =
    let client_header =
      { Client_header.
        client_version
      ; client_id
      }
    in
    begin match writer t with
      | Error `Closed ->
        return (Error Ibx_error.Connection_closed)
      | Ok writer ->
        send_tws writer Client_header.encoder client_header;
        read_tws t.reader Server_header.decoder ~len:2
        >>= function
        | Error err ->
          don't_wait_for (close t);
          return (Error err)
        | Ok `Eof ->
          don't_wait_for (close t);
          return (Ok Handshake_result.Eof)
        | Ok (`Ok header) ->
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

module Request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t Header.t
    ; recv_header  : Recv_tag.t Header.t
    ; tws_query    : 'query    Tws_prot.Encoder.t
    ; tws_response : 'response Tws_prot.Decoder.t
    }

  let create ~send_header ~recv_header ~tws_query ~tws_response =
    { send_header
    ; recv_header
    ; tws_query
    ; tws_response
    }

  let dispatch t con query =
    let ivar = Ivar.create () in
    let query =
      { Query.
        tag     = t.send_header.Header.tag
      ; version = t.send_header.Header.version
      ; id      = None
      ; data    = to_tws t.tws_query query
      }
    in
    let handler =
      Response_handler.create ~header:t.recv_header ~run:(fun response ->
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
          end)
    in
    begin
      match Connection.dispatch con ~handlers:[handler] query with
      | Ok () -> ()
      | Error `Closed -> Ivar.fill ivar (Error Ibx_error.Connection_closed)
    end;
    Ivar.read ivar >>| Ibx_result.or_error
end

module Streaming_request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t Header.t
    ; canc_header  : Send_tag.t Header.t option
    ; recv_header  : Recv_tag.t Header.t list
    ; skip_header  : Recv_tag.t Header.t list option
    ; tws_query    : 'query    Tws_prot.Encoder.t
    ; tws_response : 'response Tws_prot.Decoder.t list
    }

  module Id = Query_id

  let create ?canc_header ?skip_header ~send_header ~recv_header
      ~tws_query ~tws_response () =
    { send_header
    ; canc_header
    ; recv_header
    ; skip_header
    ; tws_query
    ; tws_response
    }

  let dispatch t con query =
    let%bind query_id = Connection.next_query_id con in
    let ivar = Ivar.create () in
    let pipe_r, pipe_w = Pipe.create () in
    let query =
      { Query.
        tag     = t.send_header.Header.tag
      ; version = t.send_header.Header.version
      ; id      = Some query_id
      ; data    = to_tws t.tws_query query
      }
    in
    let error_handler =
      Response_handler.create ~header:Header.tws_error ~run:(fun response ->
        match response.Response.data with
        | Error err ->
          Pipe.close pipe_w;
          return (`Die err)
        | Ok `Cancel ->
          Pipe.close pipe_w;
          return `Remove
        | Ok (`Response data) ->
          begin
            match of_tws Tws_error.decoder data with
            | Error err ->
              Pipe.close pipe_w;
              return (`Die err)
            | Ok tws_error ->
              if not (Pipe.is_closed pipe_w) then begin
                (* Pipes are closed after cancellations of streaming
                   requests and write calls must be guarded against
                   subsequent incoming messages. *)
                let e = Tws_error.to_error tws_error in
                don't_wait_for (Pipe.write pipe_w (Error e));
              end;
              Ivar.fill_if_empty ivar (Ok (pipe_r, query_id));
              return `Keep
          end)
    in
    let skip_handlers =
      match t.skip_header with
      | None -> []
      | Some skip_headers ->
        List.map skip_headers ~f:(fun header ->
          Response_handler.create ~header ~run:(fun response ->
            match response.Response.data with
            | Error err -> return (`Die err)
            | Ok `Cancel -> return `Remove
            | Ok (`Response _) -> return `Keep))
    in
    let data_handler_result = Result.try_with (fun () ->
      List.map2_exn t.recv_header t.tws_response ~f:(fun header decoder ->
        Response_handler.create ~header ~run:(fun response ->
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
                match of_tws decoder data with
                | Error err ->
                  Pipe.close pipe_w;
                  return (`Die err)
                | Ok response ->
                  if not (Pipe.is_closed pipe_w) then begin
                    (* Pipes are closed after cancellations of streaming
                       requests and write calls must be guarded against
                       subsequent incoming messages. *)
                    don't_wait_for (Pipe.write pipe_w (Ok response))
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
              match of_tws decoder data with
              | Error err as x ->
                Pipe.close pipe_w;
                Ivar.fill_if_empty ivar x;
                return (`Die err)
              | Ok response ->
                if not (Pipe.is_closed pipe_w) then begin
                  (* Guard against an early closing of the pipe. *)
                  don't_wait_for (Pipe.write pipe_w (Ok response));
                end;
                (* We fill the ivar only in the first iteration. *)
                Ivar.fill_if_empty ivar (Ok (pipe_r, query_id));
                return (`Replace (update pipe_w))
            end)))
    in
    begin
      match data_handler_result with
      | Error exn ->
        don't_wait_for (Connection.close con);
        let err = Ibx_error.Decoder_mismatch (Exn.sexp_of_t exn, t.recv_header) in
        Ivar.fill ivar (Error err)
      | Ok data_handlers ->
        let handlers = error_handler :: skip_handlers @ data_handlers in
        match Connection.dispatch con ~handlers query with
        | Ok () -> ()
        | Error `Closed -> Ivar.fill ivar (Error Ibx_error.Connection_closed)
    end;
    Ivar.read ivar >>| Ibx_result.or_error

  let cancel t con query_id =
    let recv_header = Header.tws_error :: t.recv_header in
    let result =
      match t.canc_header with
      | None -> Connection.cancel_streaming con ~recv_header ~query_id
      | Some header ->
        let query =
          { Query.
            tag     = header.Header.tag
          ; version = header.Header.version
          ; id      = Some query_id
          ; data    = ""
          }
        in
        Connection.cancel_streaming con ~recv_header ~query_id ~query
    in
    ignore (result : (unit, [ `Closed ]) Result.t)
end

module Streaming_request_without_id = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t Header.t
    ; recv_header  : Recv_tag.t Header.t list
    ; skip_header  : Recv_tag.t Header.t list option
    ; tws_query    : 'query    Tws_prot.Encoder.t
    ; tws_response : 'response Tws_prot.Decoder.t list
    }

  let create ?skip_header ~send_header ~recv_header
      ~tws_query ~tws_response () =
    { send_header
    ; recv_header
    ; skip_header
    ; tws_query
    ; tws_response
    }

  let dispatch t con query =
    let ivar = Ivar.create () in
    let pipe_r, pipe_w = Pipe.create () in
    let query =
      { Query.
        tag     = t.send_header.Header.tag
      ; version = t.send_header.Header.version
      ; id      = None
      ; data    = to_tws t.tws_query query
      }
    in
    let skip_handlers =
      match t.skip_header with
      | None -> []
      | Some skip_headers ->
        List.map skip_headers ~f:(fun header ->
          Response_handler.create ~header ~run:(fun response ->
            match response.Response.data with
            | Error err -> return (`Die err)
            | Ok `Cancel -> return `Remove
            | Ok (`Response _) -> return `Keep))
    in
    let data_handler_result = Result.try_with (fun () ->
      List.map2_exn t.recv_header t.tws_response ~f:(fun header decoder ->
        Response_handler.create ~header ~run:(fun response ->
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
                match of_tws decoder data with
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
              match of_tws decoder data with
              | Error err as x ->
                Pipe.close pipe_w;
                Ivar.fill_if_empty ivar x;
                return (`Die err)
              | Ok response ->
                don't_wait_for (Pipe.write pipe_w response);
                (* We fill the ivar only in the first iteration. *)
                Ivar.fill_if_empty ivar (Ok pipe_r);
                return (`Replace (update pipe_w))
            end)))
    in
    begin
      match data_handler_result with
      | Error exn ->
        don't_wait_for (Connection.close con);
        let err = Ibx_error.Decoder_mismatch (Exn.sexp_of_t exn, t.recv_header) in
        Ivar.fill ivar (Error err)
      | Ok data_handlers ->
        let handlers = skip_handlers @ data_handlers in
        match Connection.dispatch con ~handlers query with
        | Ok () -> ()
        | Error `Closed -> Ivar.fill ivar (Error Ibx_error.Connection_closed)
    end;
    Ivar.read ivar >>| Ibx_result.or_error

  let cancel t con =
    let result =
      Connection.cancel_streaming con
        ~recv_header:t.recv_header
        ~query_id:Query_id.default
    in
    ignore (result : (unit, [ `Closed ]) Result.t)
end
