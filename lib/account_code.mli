open Core_kernel

type t = private string
include Identifiable.S with type t := t
include Twsable.S with type t := t
include Decodable.S with type t := t
