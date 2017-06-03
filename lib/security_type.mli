open Core
open Tws_prot

type t =
  [ `Stock
  | `Index
  | `Futures
  | `Option
  | `Fut_opt
  | `Forex
  ] [@@deriving sexp]
include Stringable.S with type t := t
val tws_of_t : [< t ] -> raw_tws
val t_of_tws : raw_tws -> [> t]
