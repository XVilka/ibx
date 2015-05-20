(* File: security_id.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
open Tws_prot

module Type = struct
  module T = struct
    type t = [ `ISIN | `RIC | `CUSIP | `SEDOL ] with sexp
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
  ] with sexp
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
