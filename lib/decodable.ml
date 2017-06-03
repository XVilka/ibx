open Tws_prot

module type S = sig
  type t
  val decoder : t Decoder.t
end
