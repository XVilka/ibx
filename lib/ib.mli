(* File: ib.mli

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

(** A module for building TWS clients

    Parts of this module are inspired by Async's Rpc module.
*)

open Core.Std
open Async.Std
open Response

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
    -> tws_query    : 'query    Tws_prot.Pickler.t
    -> tws_response : 'response Tws_prot.Unpickler.t
    -> ('query, 'response) t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Or_error.t Deferred.t

  val dispatch_exn
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Deferred.t
end

module Streaming_request : sig
  type ('query, 'response) t

  module Id : Unique_id

  val create
    :  ?canc_header:Send_tag.t Header.t
    -> ?skip_header:Recv_tag.t Header.t list
    -> send_header:Send_tag.t Header.t
    -> recv_header:Recv_tag.t Header.t list
    -> tws_query    : 'query    Tws_prot.Pickler.t
    -> tws_response : 'response Tws_prot.Unpickler.t list
    -> unit
    -> ('query, 'response) t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> ('response Pipe.Reader.t * Id.t) Or_error.t Deferred.t

  val dispatch_exn
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> ('response Pipe.Reader.t * Id.t) Deferred.t

  (** [cancel req con id] cancels the TWS data stream from the request
      associated with the unique identifier [id], which was returned
      as part of a call to [dispatch]. *)
  val cancel : (_, _) t -> Connection.t -> Id.t -> unit
end

module Client : sig
  type t
  include Client_intf.S with type t := t

  (** Creates a new (initially disconnected) client. *)
  val create
    :  ?enable_logging:bool
    -> ?client_id:Client_id.t
    -> host:string
    -> port:int
    -> unit
    -> t Deferred.t

  (** [connect t] initiates a connection and returns a deferred that becomes
      determined when the connection is established. *)
  val connect : t -> unit Deferred.t

  val disconnect : t -> unit Deferred.t

  val dispatch_request
    :  t
    -> ('query, 'response) Request.t
    -> 'query
    -> 'response Or_error.t Deferred.t

  val dispatch_streaming_request
    :  t
    -> ('query, 'response) Streaming_request.t
    -> 'query
    -> ('response Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

  val cancel_streaming_request
    :  t
    -> (_, _) Streaming_request.t
    -> Query_id.t
    -> unit

  val dispatch_and_cancel
    :  t
    -> ('query, 'response) Streaming_request.t
    -> 'query
    -> 'response Or_error.t Deferred.t

end
