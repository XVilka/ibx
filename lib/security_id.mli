(* File: security_id.mli

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

module Type : sig
  type t = [ `ISIN | `RIC | `CUSIP | `SEDOL ] [@@deriving sexp]
  include Twsable.S with type t := t
end

module Id : sig
  type t = private string [@@deriving sexp]
  include Identifiable.S with type t := t
  include Twsable.S with type t := t
end

type t =
[ `ISIN  of Id.t
| `RIC   of Id.t
| `CUSIP of Id.t
| `SEDOL of Id.t
] [@@deriving sexp]
include Stringable.S with type t := t

val isin  : Id.t -> t
val ric   : Id.t -> t
val cusip : Id.t -> t
val sedol : Id.t -> t

val sec_id_type : t -> Type.t
val sec_id      : t -> Id.t
