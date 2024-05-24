open Core
open Async_kernel

type 'a t = ('a, Ibx_error.t) Result.t [@@deriving sexp]

let make_try_with try_with (>>|) constructor f =
  try_with f >>| function
  | Ok _ as x -> x
  | Error exn -> Error (constructor (Exn.sexp_of_t exn))
;;

let try_with_read f =
  make_try_with
    Monitor.try_with
    (>>|)
    (fun e -> Ibx_error.Read_error e)
    f
;;

let try_with_decode f =
  make_try_with
    Result.try_with
    (|>)
    (fun e -> Ibx_error.Parse_error e)
    f
;;

let or_error = function
  | Ok _ as x -> x
  | Error err -> Error (Ibx_error.to_error err)
;;
