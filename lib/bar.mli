open Core

(* A bar contained in [History] and [Realtime_bars] responses. *)
type t = private
  { stamp : Time_float_unix.t
    (* Timestamp of the bar. *)
  ; op : Price.t
    (* The opening price of the bar. *)
  ; hi : Price.t
    (* The highest price during the time covered by the bar. *)
  ; lo : Price.t
    (* The lowest price during the time covered by the bar. *)
  ; cl : Price.t
    (* The closing price of the bar. *)
  ; vo : Volume.t
    (* The volume during the time covered by the bar. *)
  ; wap : Price.t
    (* The weigthed average price during the time covered by the bar. *)
  ; has_gaps : bool
    (* Determines whether or not there are gaps in the data. *)
  ; n_trades : int
    (* The number of trades during the time covered by the bar.  Only set when
       the [tick_type] was [Trades]. *)
  } [@@deriving sexp, fields]

include Raw_bar_intf.S
  with type raw := Raw_bar.t
  with type t := t

(** Creates a new bar from the given arguments. *)
val create
  :  stamp:Time_float_unix.t
  -> op:Price.t
  -> hi:Price.t
  -> lo:Price.t
  -> cl:Price.t
  -> vo:Volume.t
  -> wap:Price.t
  -> has_gaps:bool
  -> n_trades:int
  -> t

(** Checks two bars for equality. *)
val ( = ) : t -> t -> bool

(** [combine t ~bar] combines [t] and [bar] into a new bar whose size is the
    sum of the sizes of these bars, e.g. two 1 min bars become a 2 min bar. *)
val combine : t -> bar:t -> t

(** Pretty printer for bars. *)
val pp : Format.formatter -> t -> unit

module Duration : sig
  (** The duration of a historical bar request. *)
  type t =
    [ `Sec   of int
    | `Day   of int
    | `Week  of int
    | `Month of int
    | `Year  of int
    ] [@@deriving sexp, eq]
  include Stringable.S with type t := t
  include Twsable.S with type t := t
end

module Size : sig
  (** Specifies the size of historical bars. *)
  type t =
    [ `One_sec     | `Five_sec   | `Fifteen_sec | `Thirty_sec
    | `One_min     | `Two_min    | `Three_min   | `Five_min
    | `Fifteen_min | `Thirty_min | `One_hour    | `One_day
    ] [@@deriving sexp, eq]
  include Stringable.S with type t := t
  include Twsable.S with type t := t

  (** [to_span t] converts the bar size specification into a time span. *)
  val to_span : t -> Time_float_unix.Span.t
end
