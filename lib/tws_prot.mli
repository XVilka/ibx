(* File: tws_prot.mli

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

(** A small pickler/unpickler library for the TWS protocol *)

open Core

(** Type of a raw TWS messages. *)
type raw_tws = string [@@deriving sexp]

module Val_type : sig
  type 'a t
  val create : ('a -> raw_tws) -> (raw_tws -> 'a) -> 'a t
end

module Pickler : sig

  module Spec : sig

    type 'a t

    val empty : unit -> [ `Args ] t

    (** We may want to use a ['a] pickler to serialize ['b] values. *)
    val lift : 'a t -> ('b -> 'a) -> 'b t

    val (++) : 'a t -> 'b t -> ('a * 'b) t

    val ($) : 'a -> 'b -> 'a * 'b

    val unit   : unit        Val_type.t
    val string : string      Val_type.t
    val int    : int         Val_type.t
    val int64  : int64       Val_type.t
    val float  : float       Val_type.t
    val bool   : bool        Val_type.t
    val bools  : bool        Val_type.t
    val stamp  : Time.t      Val_type.t
    val time   : Time.t      Val_type.t
    val date   : Date.t      Val_type.t
    val zone   : Time.Zone.t Val_type.t

    type 'a value

    val sequence : ?sep:char -> 'a Val_type.t -> 'a list value
    val required : 'a Val_type.t -> 'a value
    val optional : ?default_on_none:raw_tws -> 'a Val_type.t -> 'a option value

    val skipped_if_none : 'a Val_type.t -> 'a option value
    val skipped : _ value
    val tws_data : raw_tws value

    val value : 'a value -> 'a t

    val fields_value : 'a value -> 'accum t -> (_, _) Field.t -> ('accum * 'a) t

  end

  type 'a t

  val create : ?buf_size:int -> ?name:string -> 'a Spec.t -> 'a t

  val run : 'a t -> 'a -> raw_tws

end

module Unpickler : sig

  module Spec : sig

    type ('conv_in, 'conv_out) t

    val (++) : ('c1, 'c2) t -> ('c2, 'c3) t -> ('c1, 'c3) t

    val step  : ('c1 -> 'c2) -> ('c1, 'c2) t
    val empty : unit -> ('c, 'c) t

    val unit   : unit        Val_type.t
    val string : string      Val_type.t
    val int    : int         Val_type.t
    val int64  : int64       Val_type.t
    val float  : float       Val_type.t
    val bool   : bool        Val_type.t
    val bools  : bool        Val_type.t
    val stamp  : Time.t      Val_type.t
    val time   : Time.t      Val_type.t
    val date   : Date.t      Val_type.t
    val zone   : Time.Zone.t Val_type.t

    type 'a value

    val sequence : ?sep:char -> 'a Val_type.t -> 'a list value
    val required : 'a Val_type.t -> 'a value
    val optional : ?none_on_default:raw_tws -> 'a Val_type.t -> 'a option value
    val optional_with_default : default:'a -> 'a Val_type.t -> 'a value

    val value : 'a value -> name:string -> ('a -> 'c, 'c) t
    val capture_remaining_message : (raw_tws Queue.t -> 'c, 'c) t

    val fields_value
      :  'a value
      -> ('c1, 'a -> 'c2) t
      -> (_, _) Fieldslib.Field.t
      -> ('c1, 'c2) t
  end

  type 'a t

  val create : ?name:string -> ('conv, 'a) Spec.t -> 'conv -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val const : 'a -> 'a t

  val run : 'a t -> raw_tws Queue.t -> 'a Or_error.t

  val run_exn : 'a t -> raw_tws Queue.t -> 'a

end
