(* File: exchange.ml

   IBX - OCaml implementation of the Interactive Brokers TWS API

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Core.Std
open Tws_prot

type t =
[ `SMART
| `ARCAEDGE
| `BYX
| `BATS
| `BTRADE
| `BOX
| `CFE
| `CBSX
| `ECBOT
| `CBOE
| `CBOE2
| `CHX
| `GLOBEX
| `DRCTEDGE
| `EDGEA
| `ELX
| `ICEUS
| `ISLAND
| `ISE
| `LAVA
| `NASDAQ
| `NFX
| `NASDAQOM
| `BEX
| `PSX
| `NSX
| `NYBOT
| `NYMEX
| `NYSE
| `AMEX
| `ARCA
| `LIFFE
| `ONE
| `PSE
| `PHLX
| `PINK
| `ALPHA
| `CDE
| `OMEGA
| `PURE
| `SELECT
| `TSE
| `VENTURE
| `MEXDER
| `MEXI
| `VSE
| `BATEEN
| `CHIXEN
| `SBVM
| `TRQXEN
| `MATIF
| `MONEP
| `SBF
| `BATEDE
| `CHIXDE
| `DTB
| `FWB
| `SWB
| `TRADEGATE
| `TRQXDE
| `IBIS
| `BVME
| `IDEM
| `IBCFD
| `FTA
| `AEB
| `BM
| `MEFF
| `OMS
| `SFB
| `BATECH
| `CHIXCH
| `SOFFEX
| `SWX
| `TRQXCH
| `VIRTX
| `BATEUK
| `CHIXUK
| `LSE
| `ASX
| `SNFE
| `HKFE
| `HKMEX
| `SEHK
| `IBSX
| `CHIXJ
| `TSEJ
| `SGX
| `KSE
| `IDEAL
| `IDEALPRO
| `MIBSX
] with sexp

let tws_of_t = function
  | `SMART -> "SMART"
  | `ARCAEDGE -> "ARCAEDGE"
  | `BYX -> "BYX"
  | `BATS -> "BATS"
  | `BTRADE -> "BTRADE"
  | `BOX -> "BOX"
  | `CFE -> "CFE"
  | `CBSX -> "CBSX"
  | `ECBOT -> "ECBOT"
  | `CBOE -> "CBOE"
  | `CBOE2 -> "CBOE2"
  | `CHX -> "CHX"
  | `GLOBEX -> "GLOBEX"
  | `DRCTEDGE -> "DRCTEDGE"
  | `EDGEA -> "EDGEA"
  | `ELX -> "ELX"
  | `ICEUS -> "ICEUS"
  | `ISLAND -> "ISLAND"
  | `ISE -> "ISE"
  | `LAVA -> "LAVA"
  | `NASDAQ -> "NASDAQ"
  | `NFX -> "NFX"
  | `NASDAQOM -> "NASDAQOM"
  | `BEX -> "BEX"
  | `PSX -> "PSX"
  | `NSX -> "NSX"
  | `NYBOT -> "NYBOT"
  | `NYMEX -> "NYMEX"
  | `NYSE -> "NYSE"
  | `AMEX -> "AMEX"
  | `ARCA -> "ARCA"
  | `LIFFE -> "LIFFE"
  | `ONE -> "ONE"
  | `PSE -> "PSE"
  | `PHLX -> "PHLX"
  | `PINK -> "PINK"
  | `ALPHA -> "ALPHA"
  | `CDE -> "CDE"
  | `OMEGA -> "OMEGA"
  | `PURE -> "PURE"
  | `SELECT -> "SELECT"
  | `TSE -> "TSE"
  | `VENTURE -> "VENTURE"
  | `MEXDER -> "MEXDER"
  | `MEXI -> "MEXI"
  | `VSE -> "VSE"
  | `BATEEN -> "BATEEN"
  | `CHIXEN -> "CHIXEN"
  | `SBVM -> "SBVM"
  | `TRQXEN -> "TRQXEN"
  | `MATIF -> "MATIF"
  | `MONEP -> "MONEP"
  | `SBF -> "SBF"
  | `BATEDE -> "BATEDE"
  | `CHIXDE -> "CHIXDE"
  | `DTB -> "DTB"
  | `FWB -> "FWB"
  | `SWB -> "SWB"
  | `TRADEGATE -> "TRADEGATE"
  | `TRQXDE -> "TRQXDE"
  | `IBIS -> "IBIS"
  | `BVME -> "BVME"
  | `IDEM -> "IDEM"
  | `IBCFD -> "IBCFD"
  | `FTA -> "FTA"
  | `AEB -> "AEB"
  | `BM -> "BM"
  | `MEFF -> "MEFF"
  | `OMS -> "OMS"
  | `SFB -> "SFB"
  | `BATECH -> "BATECH"
  | `CHIXCH -> "CHIXCH"
  | `SOFFEX -> "SOFFEX"
  | `SWX -> "SWX"
  | `TRQXCH -> "TRQXCH"
  | `VIRTX -> "VIRTX"
  | `BATEUK -> "BATEUK"
  | `CHIXUK -> "CHIXUK"
  | `LSE -> "LSE"
  | `ASX -> "ASX"
  | `SNFE -> "SNFE"
  | `HKFE -> "HKFE"
  | `HKMEX -> "HKMEX"
  | `SEHK -> "SEHK"
  | `IBSX -> "IBSX"
  | `CHIXJ -> "CHIXJ"
  | `TSEJ -> "TSEJ"
  | `SGX -> "SGX"
  | `KSE -> "KSE"
  | `IDEAL -> "IDEAL"
  | `IDEALPRO -> "IDEALPRO"
  | `MIBSX -> "MIBSX"

let t_of_tws = function
  | "SMART" -> `SMART
  | "ARCAEDGE" -> `ARCAEDGE
  | "BYX" -> `BYX
  | "BATS" -> `BATS
  | "BTRADE" -> `BTRADE
  | "BOX" -> `BOX
  | "CFE" -> `CFE
  | "CBSX" -> `CBSX
  | "ECBOT" -> `ECBOT
  | "CBOE" -> `CBOE
  | "CBOE2" -> `CBOE2
  | "CHX" -> `CHX
  | "GLOBEX" -> `GLOBEX
  | "DRCTEDGE" -> `DRCTEDGE
  | "EDGEA" -> `EDGEA
  | "ELX" -> `ELX
  | "ICEUS" -> `ICEUS
  | "ISLAND" -> `ISLAND
  | "ISE" -> `ISE
  | "LAVA" -> `LAVA
  | "NASDAQ" -> `NASDAQ
  | "NFX" -> `NFX
  | "NASDAQOM" -> `NASDAQOM
  | "BEX" -> `BEX
  | "PSX" -> `PSX
  | "NSX" -> `NSX
  | "NYBOT" -> `NYBOT
  | "NYMEX" -> `NYMEX
  | "NYSE" -> `NYSE
  | "AMEX" -> `AMEX
  | "ARCA" -> `ARCA
  | "LIFFE" -> `LIFFE
  | "ONE" -> `ONE
  | "PSE" -> `PSE
  | "PHLX" -> `PHLX
  | "PINK" -> `PINK
  | "ALPHA" -> `ALPHA
  | "CDE" -> `CDE
  | "OMEGA" -> `OMEGA
  | "PURE" -> `PURE
  | "SELECT" -> `SELECT
  | "TSE" -> `TSE
  | "VENTURE" -> `VENTURE
  | "MEXDER" -> `MEXDER
  | "MEXI" -> `MEXI
  | "VSE" -> `VSE
  | "BATEEN" -> `BATEEN
  | "CHIXEN" -> `CHIXEN
  | "SBVM" -> `SBVM
  | "TRQXEN" -> `TRQXEN
  | "MATIF" -> `MATIF
  | "MONEP" -> `MONEP
  | "SBF" -> `SBF
  | "BATEDE" -> `BATEDE
  | "CHIXDE" -> `CHIXDE
  | "DTB" -> `DTB
  | "FWB" -> `FWB
  | "SWB" -> `SWB
  | "TRADEGATE" -> `TRADEGATE
  | "TRQXDE" -> `TRQXDE
  | "IBIS" -> `IBIS
  | "BVME" -> `BVME
  | "IDEM" -> `IDEM
  | "IBCFD" -> `IBCFD
  | "FTA" -> `FTA
  | "AEB" -> `AEB
  | "BM" -> `BM
  | "MEFF" -> `MEFF
  | "OMS" -> `OMS
  | "SFB" -> `SFB
  | "BATECH" -> `BATECH
  | "CHIXCH" -> `CHIXCH
  | "SOFFEX" -> `SOFFEX
  | "SWX" -> `SWX
  | "TRQXCH" -> `TRQXCH
  | "VIRTX" -> `VIRTX
  | "BATEUK" -> `BATEUK
  | "CHIXUK" -> `CHIXUK
  | "LSE" -> `LSE
  | "ASX" -> `ASX
  | "SNFE" -> `SNFE
  | "HKFE" -> `HKFE
  | "HKMEX" -> `HKMEX
  | "SEHK" -> `SEHK
  | "IBSX" -> `IBSX
  | "CHIXJ" -> `CHIXJ
  | "TSEJ" -> `TSEJ
  | "SGX" -> `SGX
  | "KSE" -> `KSE
  | "IDEAL" -> `IDEAL
  | "IDEALPRO" -> `IDEALPRO
  | "MIBSX" -> `MIBSX
  | s -> failwithf "Exchange.to_string: %S" s ()

let to_string = tws_of_t
let of_string = t_of_tws

let val_type = Val_type.create tws_of_t t_of_tws
