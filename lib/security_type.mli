open Core_kernel

type t =
  [ `Stock
  | `Index
  | `Futures
  | `Option
  | `Fut_opt
  | `Forex
  ] [@@deriving sexp, eq]
include Stringable.S with type t := t
val tws_of_t : [< t ] -> string
val t_of_tws : string -> [> t]
