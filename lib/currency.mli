open Core

type t =
  [ `USD    (* United States dollar *)
  | `AUD    (* Australian dollar *)
  | `CAD    (* Canadian dollar *)
  | `CHF    (* Swiss franc *)
  | `CNH    (* Chinese renminbi (offshore) *)
  | `DKK    (* Danish krone *)
  | `EUR    (* European euro *)
  | `GBP    (* British pound *)
  | `HKD    (* Hong Kong dollar *)
  | `HUF    (* Hungarian forint *)
  | `ILS    (* Israeli new shekel *)
  | `JPY    (* Japanes yen *)
  | `MXN    (* Mexican peso *)
  | `NOK    (* Norwegian krone *)
  | `NZD    (* New Zealand dollar *)
  | `RUB    (* Russian ruble *)
  | `SEK    (* Swedish krona *)
  | `SGD    (* Singapore dollar *)
  | `KRW    (* South Korean won *)
  ] [@@deriving sexp, eq]
include Stringable.S with type t := t
include Twsable.S with type t := t
