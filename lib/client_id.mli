open Core_kernel

type t
include Unique_id.Id with type t := t
include Twsable.S with type t := t
