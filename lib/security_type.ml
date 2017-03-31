(* File: security_type.ml

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

open Core
open Tws_prot

module T = struct
  type t =
    [ `Stock
    | `Index
    | `Futures
    | `Option
    | `Fut_opt
    | `Forex
    ] [@@deriving sexp]
end
include T
include Sexpable.To_stringable (T)

let tws_of_t = function
  | `Stock   -> "STK"
  | `Index   -> "IND"
  | `Futures -> "FUT"
  | `Option  -> "OPT"
  | `Fut_opt -> "FOP"
  | `Forex   -> "CASH"

let t_of_tws = function
  | "STK"  -> `Stock
  | "IND"  -> `Index
  | "FUT"  -> `Futures
  | "OPT"  -> `Option
  | "FOP"  -> `Fut_opt
  | "CASH" -> `Forex
  | s -> invalid_argf "Security_type.t_of_tws: %S" s ()

let val_type = Val_type.create tws_of_t t_of_tws
