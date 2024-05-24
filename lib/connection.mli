open Core
open Async_kernel
open Protocol

type t
include Connection_intf.S with type t := t

module Response_handler : sig
  type handler =
    Response.t
    -> [ `Keep
       | `Remove
       | `Replace of handler
       | `Die of Ibx_error.t ] Deferred.t

  type t

  val create : header:Recv_tag.t Header.t -> run:handler -> t
end

val dispatch
  :  t
  -> handlers:Response_handler.t list
  -> Query.t
  -> (unit, [ `Closed ]) Result.t

val cancel_streaming
  :  ?query:Query.t
  -> t
  -> recv_header:Recv_tag.t Header.t list
  -> query_id:Query_id.t
  -> (unit, [ `Closed ]) Result.t

val next_query_id : t -> Query_id.t Deferred.t
