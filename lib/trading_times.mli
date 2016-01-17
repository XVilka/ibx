(* File: trading_times.mli

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

type t [@@deriving sexp]
include Twsable.S with type t := t

val create : date:Date.t -> hours:Time.Ofday.t list -> t

val date : t -> Date.t
val closed : t -> bool

val start : t -> zone:Time.Zone.t -> Time.t option
val stop  : t -> zone:Time.Zone.t -> Time.t option

val start_exn : t -> zone:Time.Zone.t -> Time.t
val stop_exn  : t -> zone:Time.Zone.t -> Time.t
