open Core.Std
open Async.Std
open Ibx.Std

(** This customized client offers only subscription to option data. *)

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
