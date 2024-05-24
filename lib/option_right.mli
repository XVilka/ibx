open Core

type t = [ `Call | `Put ] [@@deriving sexp, eq]
include Stringable.S with type t := t
include Twsable.S with type t := t
