open Core.Std
open Async.Std
open Ibx.Std

module Req_intf : sig
  val req_option_data :
    (Query.Market_data.t, Tick_option.t) Ib.Streaming_request.t
end = struct
  module S = Send_tag
  module R = Recv_tag

  let req_option_data = Ib.Streaming_request.create
    ~send_header:(Ib.Header.create ~tag:S.Market_data ~version:9)
    ~canc_header:(Ib.Header.create ~tag:S.Cancel_market_data ~version:1)
    ~recv_header:[
      Ib.Header.create ~tag:R.Tick_option ~version:6;
    ]
    ~skip_header:[
      Ib.Header.create ~tag:R.Tick_price ~version:6;
      Ib.Header.create ~tag:R.Tick_size ~version:6;
      Ib.Header.create ~tag:R.Tick_string ~version:6;
      Ib.Header.create ~tag:R.Tick_generic ~version:6;
    ]
    ~tws_query:Query.Market_data.pickler
    ~tws_response:[
      Response.Tick_option.unpickler;
    ] ()
end

include Ib.Client

let option_data t ~option =
  let q =
    Query.Market_data.create
      ~contract:option
      ~tick_generics:[]
      ~snapshot:false
  in
  dispatch_streaming_request t Req_intf.req_option_data q

let option_data_exn t ~option = option_data t ~option >>| Or_error.ok_exn

let cancel_option_data t id =
  cancel_streaming_request t Req_intf.req_option_data id
