open Core.Std
open Async.Std
open Ibx.Std

(* This example demonstrates how to customize a client. *)

module Tws_req : sig
  val req_option_data : (Query.Market_data.t, Tick_option.t) Ib.Streaming_request.t
end = struct
  module S = Send_tag
  module R = Recv_tag

  let req_option_data = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_data        ~version:9)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
    ~recv_header:[
      Ib.Header.create ~tag:R.Tick_option  ~version:6;
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Tick_price   ~version:6;
      Ib.Header.create ~tag:R.Tick_size    ~version:6;
      Ib.Header.create ~tag:R.Tick_string  ~version:6;
      Ib.Header.create ~tag:R.Tick_generic ~version:6;
    ]
    ~tws_query:Query.Market_data.pickler
    ~tws_response:[Response.Tick_option.unpickler]
    ()
end

module Option_client : sig
  type t
  include Client_intf.S with type t := t

  val option_data
    :  t
    -> option:[ `Option ] Contract.t
    -> (Tick_option.t Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

  val option_data_exn
    :  t
    -> option:[ `Option ] Contract.t
    -> (Tick_option.t Pipe.Reader.t * Query_id.t) Deferred.t

  val cancel_option_data : t -> Query_id.t -> unit
end = struct
  include Ib.Client

  let option_data t ~option =
    let q =
      Query.Market_data.create
        ~contract:option
        ~tick_generics:[]
        ~snapshot:false
    in
    dispatch_streaming_request t Tws_req.req_option_data q

  let option_data_exn t ~option =
    option_data t ~option >>| Or_error.ok_exn

  let cancel_option_data t id =
    cancel_streaming_request t Tws_req.req_option_data id
end

let print_option_data ~duration =
  Option_client.with_client
    ~on_handler_error:(`Call (fun e ->
      prerr_endline (Error.to_string_hum e);
      shutdown 1
    ))
    (fun clt ->
      (* For current contracts check: http://finance.yahoo.com/q/op?s=GOOG *)
      let goog_call =
        Contract.option
          ~id:(Contract_id.of_string "116034093")
          ~exchange:`CBOE
          ~currency:`USD
          ~option_right:`Call
          ~expiry:(Date.create_exn ~y:2013 ~m:Month.Jun ~d:21)
          ~strike:(Price.of_float 850.)
          (Symbol.of_string "GOOG")
      in
      Option_client.option_data_exn clt ~option:goog_call
      >>= fun (option_ticks, id) ->
      upon (after duration) (fun () -> Option_client.cancel_option_data clt id);
      Pipe.iter_without_pushback option_ticks ~f:(fun option_tick ->
        print_endline (Sexp.to_string_hum (Tick_option.sexp_of_t option_tick)))
    )

let command =
  Command.async_basic ~summary:"print option data"
    Command.Spec.(
      empty
      +> Common.host_arg ()
      +> Common.port_arg ()
      +> Common.duration_arg ()
    )
    (fun host port duration () ->
      print_option_data ~host ~port ~duration
    )

let () = Command.run command
