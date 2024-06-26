open Core
open Async
open Ibx
open Test_lib

module H = Connection.Handshake_result
module P = Protocol

let unix_pipe () =
  Unix.pipe (Info.of_string "Connection_test.unix_pipe")
  >>| fun (`Reader fd_r, `Writer fd_w) ->
  let reader = Reader.create fd_r in
  let writer = Writer.create fd_w in
  reader, writer

let with_tws_conn reader ~f =
  Unix.openfile "/dev/null" ~mode:[`Wronly] ~perm:0o0666
  >>= fun fd ->
  let writer = Writer.create fd in
  Connection.create
    ~do_logging:true
    ~extend_error:(fun e -> Log.Global.error "%s" (Error.to_string_hum e))
    ~extend_status:(fun _ -> assert false)
    ~extend_execution:(fun _ -> assert false)
    ~extend_commission:(fun _ -> assert false)
    reader
    writer
  >>= fun con -> f con

let connect con =
  Connection.try_connect con
    ~client_version:Ibx.Config.client_version
    ~client_id:(Client_id.of_int_exn 1)
  >>| fun handshake_result ->
  match handshake_result with
  | Ok (H.Server_header _) -> ()
  | _ -> assert false

let (^@) s t = s^"\000"^t;;

let send_server_header w =
  let server_version = Int.to_string Ibx.Config.server_version in
  Writer.write w (server_version ^@ "20121107 18:08:49 CET" ^@ "");
  Writer.write w ("15" ^@ "1" ^@ "DU15133" ^@ "")

module Handshake = struct

  let try_handshake con =
    Connection.try_connect con
      ~client_version:Ibx.Config.client_version
      ~client_id:(Client_id.of_int_exn 1)

  let suite = "Handshake" >::: [

    "handshake-parse-error" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          Writer.write w ("foo" ^@ "20121107 18:08:49 CET" ^@ "");
          try_handshake con
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun ()-> Writer.close w)
    );

    "handshake-unexpected-eof" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      with_tws_conn r ~f:(fun con ->
        Writer.close w
        >>= fun () ->
        try_handshake con
        >>= function
        | Error _ -> assert false
        | Ok handshake_result ->
          match handshake_result with
          | H.Eof -> Connection.closed con
          | H.Server_header _ -> assert false
          | H.Version_failure _ -> assert false
      )
    );

    "handshake-version-failure" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          let too_small_version = Int.to_string (Ibx.Config.server_version-1) in
          Writer.write w (too_small_version ^@ "20121107 18:08:49 CET" ^@ "");
          try_handshake con
          >>= function
          | Error _ -> assert false
          | Ok handshake_result->
            match handshake_result with
            | H.Eof -> assert false
            | H.Server_header _ -> assert false
            | H.Version_failure version ->
              assert_string_equal
                ~expected:too_small_version
                ~actual:(Int.to_string version);
              Connection.close con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "successful-handshake" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          let server_version = Int.to_string Ibx.Config.server_version in
          let server_time = Time_float_unix.to_string (Time_float_unix.now ()) in
          let account_code = "DU15133" in
          Writer.write w (server_version ^@ server_time ^@ "");
          Writer.write w ("15" ^@ "1" ^@ account_code ^@ "");
          try_handshake con
          >>= function
          | Error _ -> assert false
          | Ok handshake_result ->
            match handshake_result with
            | H.Eof -> assert false
            | H.Version_failure _ -> assert false
            | H.Server_header (`Version version, time, name) ->
              assert_string_equal
                ~expected:server_version
                ~actual:(Int.to_string version);
              assert_string_equal
                ~expected:server_time
                ~actual:(Time_float_unix.to_string time);
              assert_string_equal
                ~expected:account_code
                ~actual:(Account_code.to_string name);
              Connection.close con
        )
      ) ~finally:(fun () -> Writer.close w)
    );
  ]
end

module Request = struct
  let server_time con =
    Ib.Request.dispatch Tws_reqs.req_server_time con (Query.Server_time.create ())

  let suite = "Request" >::: [

    "response-parse-error" >:: (fun () ->
      Deferred.List.iter ~how:`Parallel [
        "123" ^@ "1"   ^@ "1352386125" ^@ "";
        "foo" ^@ "1"   ^@ "1352386125" ^@ "";
        "49"  ^@ "bar" ^@ "1352386125" ^@ "";
      ] ~f:(fun msg ->
        unix_pipe ()
        >>= fun (r, w) ->
        Monitor.protect (fun () ->
          with_tws_conn r ~f:(fun con ->
            send_server_header w;
            connect con
            >>= fun () ->
            Writer.write w msg;
            server_time con
            >>= function
            | Ok _ -> assert false
            | Error _ -> Connection.closed con
          )
        ) ~finally:(fun () -> Writer.close w))
    );

    "missing-response-handler" >:: (fun () ->
      let wrong_recv_tag = Recv_tag.Snapshot_end in
      let buggy_req = Ib.Request.create
          ~send_header:P.Header.{ tag = Send_tag.Server_time; version = 1 }
          ~recv_header:P.Header.{ tag = wrong_recv_tag; version = 1 }
          ~tws_query:Query.Server_time.encoder
          ~tws_response:Response.Server_time.decoder
      in
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          connect con
          >>= fun () ->
          Writer.write w ("49" ^@ "1" ^@ "1352386125" ^@ "");
          Ib.Request.dispatch buggy_req con (Query.Server_time.create ())
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "handler-version-failure" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          connect con
          >>= fun () ->
          let wrong_version = "2" in
          Writer.write w ("49" ^@ wrong_version ^@ "1352386125" ^@ "");
          server_time con
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "handler-parse-error" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          connect con
          >>= fun () ->
          Writer.write w ("49" ^@ "1" ^@ "foo" ^@ "");
          server_time con
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "handler-no-input" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      with_tws_conn r ~f:(fun con ->
        send_server_header w;
        connect con
        >>= fun () ->
        Writer.write w ("49" ^@ "1" ^@ "");
        Writer.close w
        >>= fun () ->
        server_time con
        >>= function
        | Ok _ -> assert false
        | Error _ -> Connection.closed con
      )
    );

    "successful-dispatch" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          connect con
          >>= fun () ->
          Writer.write w ("49" ^@ "1" ^@ "1352386125" ^@ "");
          server_time con
          >>= function
          | Error _ -> assert false
          | Ok _time ->
            assert (not (Connection.is_closed con));
            Connection.close con
        )
      ) ~finally:(fun () -> Writer.close w)
    );
  ]
end

module Streaming_request = struct

  let req_tick_size = Ib.Streaming_request.create
      ~send_header:P.Header.{ tag = Send_tag.Market_data; version = 9 }
      ~canc_header:P.Header.{ tag = Send_tag.Cancel_market_data; version = 1 }
      ~recv_header:[P.Header.{ tag = Recv_tag.Tick_size; version = 6 }]
      ~tws_query:Query.Market_data.encoder
      ~tws_response:[Response.Tick_size.decoder]
      ()

  let tick_size con =
    Ib.Streaming_request.dispatch req_tick_size con (Rg.Q.market_data_g ())

  let cancel_tick_size con id =
    Ib.Streaming_request.cancel req_tick_size con id

  let next_order_id = Random.int 100

  let send_next_order_id w =
    let oid = Int.to_string next_order_id in
    Writer.write w ("9" ^@ "1" ^@ oid ^@ "")

  let make_counter ~init =
    let counter = ref init in
    stage (fun () -> let res = Int.to_string !counter in incr counter; res)

  let next_query_id = unstage (make_counter ~init:next_order_id)

  let suite = "Streaming-request" >::: [

    "response-parse-error" >:: (fun () ->
      Deferred.List.iter ~how:`Parallel [
        "123" ^@ "6"   ^@ next_query_id () ^@ "0" ^@ "1" ^@ "";
        "foo" ^@ "6"   ^@ next_query_id () ^@ "0" ^@ "1" ^@ "";
        "2"   ^@ "bar" ^@ next_query_id () ^@ "0" ^@ "1" ^@ "";
      ] ~f:(fun msg ->
        unix_pipe ()
        >>= fun (r, w) ->
        Monitor.protect (fun () ->
          with_tws_conn r ~f:(fun con ->
            send_server_header w;
            send_next_order_id w;
            connect con
            >>= fun () ->
            Writer.write w msg;
            tick_size con
            >>= function
            | Ok _ -> assert false
            | Error _ -> Connection.closed con
          )
        ) ~finally:(fun () -> Writer.close w))
    );

    "handler-version-failure" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          send_next_order_id w;
          connect con
          >>= fun () ->
          let wrong_version = "1" in
          Writer.write w ("2" ^@ wrong_version ^@ next_query_id () ^@ "0" ^@ "1" ^@ "");
          tick_size con
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "handler-parse-error" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          send_next_order_id w;
          connect con
          >>= fun () ->
          Writer.write w ("2" ^@ "6" ^@ next_query_id () ^@ "foo" ^@ "bar" ^@ "");
          tick_size con
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "update-parse-error" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          send_next_order_id w;
          connect con
          >>= fun () ->
          let query_id = next_query_id () in
          Writer.write w ("2" ^@ "6" ^@ query_id ^@ "0"   ^@ "1"   ^@ "");
          Writer.write w ("2" ^@ "6" ^@ query_id ^@ "foo" ^@ "bar" ^@ "");
          tick_size con
          >>= function
          | Ok (pipe_r, _id) ->
            Connection.closed con
            >>= fun () ->
            Pipe.closed pipe_r
          | Error _ -> assert false
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "cancel-streaming" >:: (fun () ->
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          send_next_order_id w;
          connect con
          >>= fun () ->
          Writer.write w ("2" ^@ "6" ^@ next_query_id () ^@ "0" ^@ "1" ^@ "");
          tick_size con
          >>= function
          | Error _ -> assert false
          | Ok (pipe_r, id) ->
            cancel_tick_size con id;
            assert (Pipe.is_closed pipe_r);
            Connection.close con
            >>= fun () ->
            Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );

    "decoder-mismatch" >:: (fun () ->
      let module D = Tws_prot.Decoder in
      let buggy_req =
        Ib.Streaming_request.create
          ~send_header:P.Header.{ tag = Send_tag.Market_data; version = 9 }
          ~canc_header:P.Header.{ tag = Send_tag.Cancel_market_data; version = 1 }
          ~recv_header:[P.Header.{ tag = Recv_tag.Tick_size; version = 6 }]
          ~tws_query:Query.Market_data.encoder
          ~tws_response:[
            D.map Response.Tick_price.decoder ~f:(fun x -> `Tick_price x)
          ; D.map Response.Tick_size.decoder  ~f:(fun x -> `Tick_size  x)
          ] ()
      in
      unix_pipe ()
      >>= fun (r, w) ->
      Monitor.protect (fun () ->
        with_tws_conn r ~f:(fun con ->
          send_server_header w;
          send_next_order_id w;
          connect con
          >>= fun () ->
          ignore (next_query_id ());
          Ib.Streaming_request.dispatch buggy_req con (Rg.Q.market_data_g ())
          >>= function
          | Ok _ -> assert false
          | Error _ -> Connection.closed con
        )
      ) ~finally:(fun () -> Writer.close w)
    );
  ]

end
