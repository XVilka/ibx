open Core
open Async_kernel

type 'a t = 'a Protocol.Ibx_result.t [@@deriving sexp]

val try_with_read : (unit -> 'a Deferred.t) -> 'a t Deferred.t

val try_with_decode : (unit -> 'a) -> 'a t

val or_error : 'a t -> 'a Or_error.t
