open Core_kernel

module Type : sig
  type t =
    [ `ISIN
    | `RIC
    | `CUSIP
    | `SEDOL
    ] [@@deriving sexp]
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
