open Tws_prot

module type S = sig
  type t
  val tws_of_t : t -> raw_tws
  val t_of_tws : raw_tws -> t
  val val_type : t Val_type.t
end
