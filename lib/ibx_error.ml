open Core

include Protocol.Ibx_error

exception Ibx of t [@@deriving sexp]

let raise t = raise (Ibx t)

let to_error t = Error.of_thunk (fun () -> Sexp.to_string_hum (sexp_of_t t))
