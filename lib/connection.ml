open Core
open Async
open Tws_prot

module P = Protocol
module R = Recv_tag

module Response_handler = struct
  type handler =
    P.Response.t
    -> [ `Keep
       | `Remove
       | `Replace of handler
       | `Die of Ibx_error.t ] Deferred.t

  type t =
    { header : Recv_tag.t P.Header.t
    ; run    : handler
    }

  let create ~header ~run = { header; run }
end

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
and logfun = [ `Send of P.Query.t | `Recv of P.Response.t ] -> unit

let init_handler t ~header ~decoder ~action ~f =
  let id = Query_id.default in
  let tag = header.P.Header.tag in
  let version = header.P.Header.version in
  Hashtbl.set t.open_queries ~key:(id, tag, version) ~data:(fun response ->
    match response.P.Response.data with
    | Error err ->
      return (`Die err)
    | Ok `Cancel ->
      assert false (* Response handler is not cancelable. *)
    | Ok (`Response data) ->
      begin
        match Util.of_tws decoder data with
        | Error err ->
          return (`Die err)
        | Ok response ->
          f response;
          return action
      end)
;;

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
      let msg = Util.to_tws P.Query.encoder query in
      Log.Global.debug ">> %s" (tr_null msg)
    | `Recv response ->
      let msg = Util.to_tws P.Response.encoder response in
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
    ~header:P.Header.{ tag = R.Managed_accounts; version = 1 }
    ~decoder:Account_code.decoder
    ~action:`Remove
    ~f:(Ivar.fill t.account_code);
  init_handler t
    ~header:P.Header.{ tag = R.Next_order_id; version = 1 }
    ~decoder:Order_id.decoder
    ~action:`Remove
    ~f:(Ivar.fill t.next_order_id);
  init_handler t
    ~header:P.Header.{ tag = R.Tws_error; version = 2 }
    ~decoder:Response.Tws_error.decoder
    ~action:`Keep
    ~f:(fun e -> extend_status (Response.Tws_error.to_string_hum e));
  init_handler t
    ~header:P.Header.{ tag = R.Execution; version = 9 }
    ~decoder:Response.Execution.decoder
    ~action:`Keep
    ~f:extend_execution;
  init_handler t
    ~header:P.Header.{ tag = R.Commission; version = 1 }
    ~decoder:Response.Commission.decoder
    ~action:`Keep
    ~f:extend_commission;
  return t
;;

let next_query_id t =
  let new_id = Query_id.create () in
  let%map oid = Ivar.read t.next_order_id in
  Query_id.increase new_id (Order_id.to_int_exn oid)
;;

let is_closed t = Ivar.is_full t.stop
let closed t = Ivar.read t.stop

let close t =
  if not (is_closed t) then begin
    Ivar.fill t.stop ();
    Writer.close t.writer
    >>| fun () ->
    Pipe.close_read t.reader
  end else Deferred.unit
;;

let send_tws writer encoder msg = Writer.write writer (Util.to_tws encoder msg)

let send_query ?logfun writer query =
  begin
    match logfun with
    | None -> ()
    | Some f -> f (`Send query)
  end;
  send_tws writer P.Query.encoder query
;;

let really_read reader ~len =
  Pipe.read_exactly reader ~num_values:len >>| function
  | `Eof -> `Eof
  | `Fewer   result -> `Ok result
  | `Exactly result -> `Ok result
;;

let read_tws reader decoder ~len =
  really_read reader ~len >>| function
  | `Eof -> Ok `Eof
  | `Ok raw_msg ->
    begin
      match Util.of_tws decoder raw_msg with
      | Error _ as x -> x
      | Ok x -> Ok (`Ok x)
    end
;;

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
;;

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
;;

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
  { P.Response.
    tag
  ; version
  ; query_id = id
  ; data = Ok (`Response data)
  }
;;

let writer t = if Ivar.is_full t.stop then Error `Closed else Ok t.writer

let set_server_log_level t ~level =
  match writer t with
  | Error `Closed -> ()
  | Ok writer ->
    let level = Query.Server_log_level.create ~level in
    let query =
      { P.Query.
        tag     = Send_tag.Set_server_log_level
      ; version = 1
      ; id      = None
      ; data    = Util.to_tws Query.Server_log_level.encoder level;
      }
    in
    send_query ?logfun:t.logfun writer query
;;

let dispatch t ~handlers query =
  match writer t with
  | Error `Closed as x -> x
  | Ok writer ->
    send_query ?logfun:t.logfun writer query;
    let id = Option.value query.P.Query.id ~default:Query_id.default in
    List.iter handlers ~f:(fun h ->
      Hashtbl.set t.open_queries ~key:(
        id,
        h.Response_handler.header.P.Header.tag,
        h.Response_handler.header.P.Header.version
      ) ~data:h.Response_handler.run
    );
    Ok ()
;;

let cancel_streaming ?query t ~recv_header ~query_id =
  match writer t with
  | Error `Closed as x -> x
  | Ok writer ->
    begin match query with
    | None -> ()
    | Some query -> send_query ?logfun:t.logfun writer query
    end;
    List.iter recv_header ~f:(fun header ->
      let tag = header.P.Header.tag in
      let version = header.P.Header.version in
      match Hashtbl.find t.open_queries (query_id, tag, version) with
      | None -> ()
      | Some response_handler ->
        don't_wait_for (Deferred.ignore (
          response_handler
            { P.Response.
              tag
            ; version
            ; query_id = Some query_id
            ;  data = Ok `Cancel
            }))
    );
    Ok ()
;;

let handle_response t response =
  let id = Option.value response.P.Response.query_id ~default:Query_id.default in
  let tag = response.P.Response.tag in
  let version = response.P.Response.version in
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
;;

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
              { P.Response.
                tag
              ; version
              ; query_id = Some id
              ; data = Error error
              })));
      t.extend_error (Ibx_error.to_error error));
  Scheduler.within ~monitor loop
;;

module Handshake_result = struct
  type t =
    | Eof
    | Version_failure of int
    | Server_header of [ `Version of int ] * Time.t * Account_code.t
  [@@deriving sexp]
end

let try_connect t ~client_version ~client_id =
  let client_header =
    { P.Client_header.
      client_version
    ; client_id
    }
  in
  begin match writer t with
  | Error `Closed ->
    return (Error Ibx_error.Connection_closed)
  | Ok writer ->
    send_tws writer P.Client_header.encoder client_header;
    read_tws t.reader P.Server_header.decoder ~len:2
    >>= function
    | Error err ->
      don't_wait_for (close t);
      return (Error err)
    | Ok `Eof ->
      don't_wait_for (close t);
      return (Ok Handshake_result.Eof)
    | Ok (`Ok header) ->
      let server_version = header.P.Server_header.server_version in
      if not (server_version >= Config.server_version) then
        return (Ok (Handshake_result.Version_failure server_version))
      else begin
        handle_incoming t;  (* Start handling incoming messages. *)
        Ivar.read t.account_code
        >>| fun account_code ->
        Ok (Handshake_result.Server_header (
          `Version server_version,
          header.P.Server_header.connection_time,
          account_code
        ))
      end
  end >>| Ibx_result.or_error
;;
