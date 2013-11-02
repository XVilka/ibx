(* File: client.mli

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

(** A module for building TWS clients *)

open Core.Std
open Async.Std

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
  -> ('query, 'response) Ib.Request.t
  -> 'query
  -> 'response Or_error.t Deferred.t

val dispatch_streaming_request
  :  t
  -> ('query, 'response) Ib.Streaming_request.t
  -> 'query
  -> ('response Pipe.Reader.t * Query_id.t) Or_error.t Deferred.t

val cancel_streaming_request
  :  t
  -> (_, _) Ib.Streaming_request.t
  -> Query_id.t
  -> unit

val dispatch_and_cancel
  :  t
  -> ('query, 'response) Ib.Streaming_request.t
  -> 'query
  -> 'response Or_error.t Deferred.t

val dispatch_streaming_request'
  :  t
  -> ('query, 'response) Ib.Streaming_request_without_id.t
  -> 'query
  -> ('response Pipe.Reader.t) Or_error.t Deferred.t

val cancel_streaming_request'
  :  t
  -> (_, _) Ib.Streaming_request_without_id.t
  -> unit
