(* File: currency.mli

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
] with sexp
include Stringable.S with type t := t

val val_type : t Val_type.t
