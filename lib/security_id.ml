open Core
open Tws_prot

module Type = struct
  module T = struct
    type t =
      [ `ISIN
      | `RIC
      | `CUSIP
      | `SEDOL
      ] [@@deriving sexp, eq]
  end
  include T
  include Sexpable.To_stringable (T)
  let tws_of_t = to_string
  let t_of_tws = of_string
  let val_type = Val_type.create tws_of_t t_of_tws
end

module Id = struct
  include String
  let tws_of_t = String.to_string
  let t_of_tws = String.of_string
  let val_type = Val_type.create tws_of_t t_of_tws
end

module T = struct
  type t =
    [ `ISIN  of Id.t
    | `RIC   of Id.t
    | `CUSIP of Id.t
    | `SEDOL of Id.t
    ] [@@deriving sexp, eq]
end
include T
include Sexpable.To_stringable (T)

let isin  id = (`ISIN  id)
let ric   id = (`RIC   id)
let cusip id = (`CUSIP id)
let sedol id = (`SEDOL id)

let sec_id_type = function
  | `ISIN  _ -> `ISIN
  | `RIC   _ -> `RIC
  | `CUSIP _ -> `CUSIP
  | `SEDOL _ -> `SEDOL

let sec_id = function
  | `ISIN  x -> x
  | `RIC   x -> x
  | `CUSIP x -> x
  | `SEDOL x -> x
