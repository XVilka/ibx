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

  (** [with_client ~host ~on_handler_error handler] connects by default
      to the IB Gateway (running on [host] under port 4001) and runs the
      [handler] until an exception is thrown or until the returned Deferred
      is determined.

      [on_handler_error] determines what happens if the [handler] throws an
      exception.

      In order to connect to TWS, you need to supply 7496 as port number.
  *)
  val with_client
    :  ?enable_logging:bool
    -> ?client_id:Client_id.t
    -> ?port:int
    -> host:string
    -> on_handler_error:[
    | `Raise
    | `Ignore
    | `Call of (Error.t -> unit)
    ]
    -> (t -> unit Deferred.t)
    -> unit Deferred.t

  val is_connected : t -> bool

  (* [state t] returns the state of the connection. *)
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
