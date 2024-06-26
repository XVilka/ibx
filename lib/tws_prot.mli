(** A small encoder/decoder library for the TWS protocol *)

open Core

module Val_type : sig
  type 'a t
  val create : ('a -> string) -> (string -> 'a) -> 'a t
end

module Encoder : sig

  module Spec : sig

    type 'a t

    val empty : unit -> [ `Args ] t

    (** We may want to use a ['a] encoder to serialize ['b] values. *)
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
    val stamp  : Time_float_unix.t      Val_type.t
    val time   : Time_float_unix.t      Val_type.t
    val date   : Date.t      Val_type.t
    val zone   : Time_float_unix.Zone.t Val_type.t

    type 'a value

    val sequence : ?sep:char -> 'a Val_type.t -> 'a list value
    val required : 'a Val_type.t -> 'a value
    val optional : ?default_on_none:string -> 'a Val_type.t -> 'a option value

    val skipped_if_none : 'a Val_type.t -> 'a option value
    val skipped : _ value
    val tws_data : string value

    val value : 'a value -> 'a t

    val fields_value : 'a value -> 'accum t -> (_, _) Field.t -> ('accum * 'a) t

  end

  type 'a t

  val create : ?buf_size:int -> ?name:string -> 'a Spec.t -> 'a t

  val run : 'a t -> 'a -> string

end

module Decoder : sig

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
    val stamp  : Time_float_unix.t      Val_type.t
    val time   : Time_float_unix.t      Val_type.t
    val date   : Date.t      Val_type.t
    val zone   : Time_float_unix.Zone.t Val_type.t

    type 'a value

    val sequence : ?sep:char -> 'a Val_type.t -> 'a list value
    val required : 'a Val_type.t -> 'a value
    val optional : ?none_on_default:string -> 'a Val_type.t -> 'a option value
    val optional_with_default : default:'a -> 'a Val_type.t -> 'a value

    val value : 'a value -> name:string -> ('a -> 'c, 'c) t
    val capture_remaining_message : (string Queue.t -> 'c, 'c) t

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

  val run : 'a t -> string Queue.t -> 'a Or_error.t

  val run_exn : 'a t -> string Queue.t -> 'a

end
