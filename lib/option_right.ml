open Core
open Tws_prot

module T = struct
  type t = [ `Call | `Put ] [@@deriving sexp]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Call -> "C"
  | `Put  -> "P"

let t_of_tws = function
  | "C" -> `Call
  | "P" -> `Put
  | s -> invalid_argf "Option_right.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
