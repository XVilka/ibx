open Core
open Tws_prot

module type S = sig
  type t
  include Sexpable.S with type t := t
  include Decodable.S with type t:= t
  val encoder : t Encoder.t Only_in_test.t
  val ( = ) : t -> t -> bool
end

module Wrapper = struct
  module type S = sig
    type t
    include Sexpable.S with type t := t
    val ( = ) : t -> t -> bool
  end
end
