(* File: response_intf.ml

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

module type S = sig
  type t
  include Sexpable.S with type t := t
  include Unpickable.S with type t:= t
  val pickler : t Pickler.t Only_in_test.t
  val ( = ) : t -> t -> bool
end

module Wrapper = struct
  module type S = sig
    type t
    include Sexpable.S with type t := t
    val ( = ) : t -> t -> bool
  end
end
