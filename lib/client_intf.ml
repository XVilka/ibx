(* File: client_intf.ml

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

(** A signature for customized TWS clients *)

open Core.Std
open Async.Std
open Response

module type S = sig
  module Query_id : Unique_id

  type t

  (** [with_client ~host ~port ~on_handler_error handler] connects to the
      IB connectivity software on ([host], [port]) and runs the [handler]
      until an exception is thrown or the returned Deferred is determined.

      [on_handler_error] determines what happens if the [handler] throws an
      exception.

      The standard port for TWS is 7496 and for the IB Gateway it is 4001.
  *)
  val with_client
    :  ?enable_logging:bool
    -> ?client_id:Client_id.t
    -> host:string
    -> port:int
    -> on_handler_error:[
    | `Raise
    | `Ignore
    | `Call of (Error.t -> unit)
    ]
    -> (t -> unit Deferred.t)
    -> unit Deferred.t

  val is_connected : t -> bool

  (** [state t] returns the state of the connection. *)
  val state : t -> [ `Disconnected | `Connecting | `Connected ]

  (** [set_server_log_level level] sets the log entry detail [level] of TWS
      when processing API requests. *)
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

  val execution_reports : t -> Execution_report.t Stream.t

  val commission_reports : t -> Commission_report.t Stream.t

  val client_id : t -> Client_id.t

  val server_version : t -> int option

  val connection_time : t -> Time.t option

  val account_code : t -> Account_code.t option

end
