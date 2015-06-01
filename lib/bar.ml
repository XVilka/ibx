(* File: bar.ml

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

include Raw_bar

let to_raw = Fn.id
let of_raw = Fn.id

let ( = ) t1 t2 = Raw_bar.(=) (to_raw t1) (to_raw t2)

let pp ppf t =
  Format.fprintf ppf
    "Bar<%s> op=%.2f hi=%.2f lo=%.2f cl=%.2f vo=%d wap=%.2f count=%d"
    (t.stamp |> Time.to_sec_string ~zone:Time.Zone.local)
    (t.op :> float) (t.hi :> float) (t.lo :> float) (t.cl :> float)
    (t.vo :> int) (t.wap :> float) (t.count)
