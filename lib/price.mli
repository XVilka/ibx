(* File: price.mli

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

type t = private float [@@deriving sexp]
include Floatable.S with type t := t
include Identifiable.S with type t := t
include Robustly_comparable.S with type t := t
include Twsable.S with type t := t

val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : t -> t -> t
val (/) : t -> t -> t

val zero : t
val one : t
val nan : t

val add : t -> t -> t
val sub : t -> t -> t
val neg : t -> t
val scale : t -> t -> t
val abs : t -> t
val is_nan : t -> bool
