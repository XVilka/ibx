open Core_kernel
open Async
open Response

module type S = sig
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
