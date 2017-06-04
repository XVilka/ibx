(** Core building blocks for IBX applications, e.g. TWS clients *)

open Core
open Async
open Response

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
    [@@deriving sexp]
  end

  val try_connect
    :  t
    -> client_version:int
    -> client_id:Client_id.t
    -> Handshake_result.t Deferred.Or_error.t
end
module Connection : Connection

module Header : sig
  type 'a t
  val create : tag:'a -> version:int -> 'a t
end

module Request : sig
  type ('query, 'response) t

  val create
    :  send_header:Send_tag.t Header.t
    -> recv_header:Recv_tag.t Header.t
    -> tws_query    : 'query    Tws_prot.Encoder.t
    -> tws_response : 'response Tws_prot.Decoder.t
    -> ('query, 'response) t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Deferred.Or_error.t
end

module Streaming_request : sig
  type ('query, 'response) t

  val create
    :  ?canc_header:Send_tag.t Header.t
    -> ?skip_header:Recv_tag.t Header.t list
    -> send_header:Send_tag.t Header.t
    -> recv_header:Recv_tag.t Header.t list
    -> tws_query    : 'query    Tws_prot.Encoder.t
    -> tws_response : 'response Tws_prot.Decoder.t list
    -> unit
    -> ('query, 'response) t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> ('response Or_error.t Pipe.Reader.t * Query_id.t) Deferred.Or_error.t

  (** [cancel req con id] cancels the TWS data stream from the request
      associated with the unique identifier [id], which was returned
      as part of a call to [dispatch]. *)
  val cancel : (_, _) t -> Connection.t -> Query_id.t -> unit
end

module Streaming_request_without_id : sig
  type ('query, 'response) t

  val create
    :  ?skip_header:Recv_tag.t Header.t list
    -> send_header:Send_tag.t Header.t
    -> recv_header:Recv_tag.t Header.t list
    -> tws_query    : 'query    Tws_prot.Encoder.t
    -> tws_response : 'response Tws_prot.Decoder.t list
    -> unit
    -> ('query, 'response) t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> ('response Pipe.Reader.t) Deferred.Or_error.t

  val cancel : (_, _) t -> Connection.t -> unit
end
