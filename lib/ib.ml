open Core
open Async

module P = Protocol

module Header = struct
  let tws_error = P.Header.{ tag = Recv_tag.Tws_error; version = 2 }
end

module Request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t P.Header.t
    ; recv_header  : Recv_tag.t P.Header.t
    ; tws_query    : 'query    Tws_prot.Encoder.t
    ; tws_response : 'response Tws_prot.Decoder.t
    }

  let create ~send_header ~recv_header ~tws_query ~tws_response =
    { send_header
    ; recv_header
    ; tws_query
    ; tws_response
    }
  ;;

  let dispatch t con query =
    let ivar = Ivar.create () in
    let query =
      { P.Query.
        tag     = t.send_header.P.Header.tag
      ; version = t.send_header.P.Header.version
      ; id      = None
      ; data    = Util.to_tws t.tws_query query
      }
    in
    let handler =
      Connection.Response_handler.create ~header:t.recv_header ~run:(fun response ->
        match response.P.Response.data with
        | Error err as x ->
          (* If this handler died before, the ivar is already filled. *)
          Ivar.fill_if_empty ivar x;
          return (`Die err)
        | Ok `Cancel ->
          assert false (* Non-streaming requests are not cancelable. *)
        | Ok (`Response data) ->
          begin
            match Util.of_tws t.tws_response data with
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
  ;;
end

module Streaming_request = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t P.Header.t
    ; canc_header  : Send_tag.t P.Header.t option
    ; recv_header  : Recv_tag.t P.Header.t list
    ; skip_header  : Recv_tag.t P.Header.t list option
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
  ;;

  let dispatch t con query =
    let%bind query_id = Connection.next_query_id con in
    let ivar = Ivar.create () in
    let pipe_r, pipe_w = Pipe.create () in
    let query =
      { P.Query.
        tag     = t.send_header.P.Header.tag
      ; version = t.send_header.P.Header.version
      ; id      = Some query_id
      ; data    = Util.to_tws t.tws_query query
      }
    in
    let error_handler =
      Connection.Response_handler.create ~header:Header.tws_error ~run:(fun response ->
        match response.P.Response.data with
        | Error err ->
          Pipe.close pipe_w;
          return (`Die err)
        | Ok `Cancel ->
          Pipe.close pipe_w;
          return `Remove
        | Ok (`Response data) ->
          begin
            match Util.of_tws Response.Tws_error.decoder data with
            | Error err ->
              Pipe.close pipe_w;
              return (`Die err)
            | Ok tws_error ->
              if not (Pipe.is_closed pipe_w) then begin
                (* Pipes are closed after cancellations of streaming
                   requests and write calls must be guarded against
                   subsequent incoming messages. *)
                let e = Response.Tws_error.to_error tws_error in
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
          Connection.Response_handler.create ~header ~run:(fun response ->
            match response.P.Response.data with
            | Error err -> return (`Die err)
            | Ok `Cancel -> return `Remove
            | Ok (`Response _) -> return `Keep))
    in
    let data_handler_result = Result.try_with (fun () ->
      List.map2_exn t.recv_header t.tws_response ~f:(fun header decoder ->
        Connection.Response_handler.create ~header ~run:(fun response ->
          let update pipe_w response =
            match response.P.Response.data with
            | Error err ->
              Pipe.close pipe_w;
              return (`Die err)
            | Ok `Cancel ->
              Pipe.close pipe_w;
              return `Remove
            | Ok (`Response data) ->
              begin
                match Util.of_tws decoder data with
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
          match response.P.Response.data with
          | Error err as x ->
            Pipe.close pipe_w;
            Ivar.fill_if_empty ivar x;
            return (`Die err)
          | Ok `Cancel ->
            Pipe.close pipe_w;
            return `Remove
          | Ok (`Response data) ->
            begin
              match Util.of_tws decoder data with
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
  ;;

  let cancel t con query_id =
    let recv_header = Header.tws_error :: t.recv_header in
    let result =
      match t.canc_header with
      | None -> Connection.cancel_streaming con ~recv_header ~query_id
      | Some header ->
        let query =
          { P.Query.
            tag     = header.P.Header.tag
          ; version = header.P.Header.version
          ; id      = Some query_id
          ; data    = ""
          }
        in
        Connection.cancel_streaming con ~recv_header ~query_id ~query
    in
    ignore (result : (unit, [ `Closed ]) Result.t)
  ;;
end

module Streaming_request_without_id = struct
  type ('query, 'response) t =
    { send_header  : Send_tag.t P.Header.t
    ; recv_header  : Recv_tag.t P.Header.t list
    ; skip_header  : Recv_tag.t P.Header.t list option
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
  ;;

  let dispatch t con query =
    let ivar = Ivar.create () in
    let pipe_r, pipe_w = Pipe.create () in
    let query =
      { P.Query.
        tag     = t.send_header.P.Header.tag
      ; version = t.send_header.P.Header.version
      ; id      = None
      ; data    = Util.to_tws t.tws_query query
      }
    in
    let skip_handlers =
      match t.skip_header with
      | None -> []
      | Some skip_headers ->
        List.map skip_headers ~f:(fun header ->
          Connection.Response_handler.create ~header ~run:(fun response ->
            match response.P.Response.data with
            | Error err -> return (`Die err)
            | Ok `Cancel -> return `Remove
            | Ok (`Response _) -> return `Keep))
    in
    let data_handler_result = Result.try_with (fun () ->
      List.map2_exn t.recv_header t.tws_response ~f:(fun header decoder ->
        Connection.Response_handler.create ~header ~run:(fun response ->
          let update pipe_w response =
            match response.P.Response.data with
            | Error err ->
              Pipe.close pipe_w;
              return (`Die err)
            | Ok `Cancel ->
              Pipe.close pipe_w;
              return `Remove
            | Ok (`Response data) ->
              begin
                match Util.of_tws decoder data with
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
          match response.P.Response.data with
          | Error err as x ->
            Pipe.close pipe_w;
            Ivar.fill_if_empty ivar x;
            return (`Die err)
          | Ok `Cancel ->
            Pipe.close pipe_w;
            return `Remove
          | Ok (`Response data) ->
            begin
              match Util.of_tws decoder data with
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
  ;;

  let cancel t con =
    let result =
      Connection.cancel_streaming con
        ~recv_header:t.recv_header
        ~query_id:Query_id.default
    in
    ignore (result : (unit, [ `Closed ]) Result.t)
  ;;
end
