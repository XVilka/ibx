open Core

type t = private int
include Int_intf.S with type t := t
include Twsable.S with type t := t
