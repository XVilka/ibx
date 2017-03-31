(* File: order_id.ml

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

include Unique_id.Int63 (struct end)
let tws_of_t = to_string
let t_of_tws = of_string
let val_type = Val_type.create tws_of_t t_of_tws
let unpickler =
  Unpickler.create ~name:"Order_Id"
    Unpickler.Spec.(value (required val_type) ~name:"order_id")
    Fn.id
