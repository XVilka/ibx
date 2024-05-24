open Core
open Tws_prot

type t =
  [ `AEB
  | `ALPHA
  | `AMEX
  | `ARCA
  | `ARCAEDGE
  | `ASX
  | `BATECH
  | `BATEDE
  | `BATEEN
  | `BATEUK
  | `BATS
  | `BEX
  | `BM
  | `BOX
  | `BTRADE
  | `BVME
  | `BYX
  | `CBOE
  | `CBOE2
  | `CBSX
  | `CDE
  | `CFE
  | `CHIXCH
  | `CHIXDE
  | `CHIXEN
  | `CHIXJ
  | `CHIXUK
  | `CHX
  | `CME
  | `CSFBALGO
  | `DRCTEDGE
  | `DTB
  | `ECBOT
  | `EDGX
  | `EDGEA
  | `ELX
  | `FTA
  | `FWB
  | `GEMINI
  | `GLOBEX
  | `HKFE
  | `HKMEX
  | `IBCFD
  | `IBIS
  | `IBSX
  | `ICEUS
  | `IDEAL
  | `IDEALPRO
  | `IDEM
  | `IEX
  | `ISE
  | `ISLAND
  | `JEFFALGO
  | `KSE
  | `LAVA
  | `LIFFE
  | `LSE
  | `MATIF
  | `MEFF
  | `MERCURY
  | `MEXDER
  | `MEXI
  | `MIAX
  | `MIBSX
  | `MONEP
  | `NASDAQ
  | `NASDAQBX
  | `NASDAQOM
  | `NFX
  | `NSX
  | `NYBOT
  | `NYMEX
  | `NYSE
  | `OMEGA
  | `OMS
  | `ONE
  | `PEARL
  | `PHLX
  | `PINK
  | `PSE
  | `PSX
  | `PURE
  | `SBF
  | `SBVM
  | `SEHK
  | `SELECT
  | `SFB
  | `SGX
  | `SMART
  | `SNFE
  | `SOFFEX
  | `SWB
  | `SWX
  | `TGATE
  | `TPLUS2
  | `TRADEGATE
  | `TRQXCH
  | `TRQXDE
  | `TRQXEN
  | `TSE
  | `TSEJ
  | `VENTURE
  | `VIRTX
  | `VSE
  | `VWAP
  ] [@@deriving sexp, eq]

let tws_of_t = function
  | `AEB -> "AEB"
  | `ALPHA -> "ALPHA"
  | `AMEX -> "AMEX"
  | `ARCA -> "ARCA"
  | `ARCAEDGE -> "ARCAEDGE"
  | `ASX -> "ASX"
  | `BATECH -> "BATECH"
  | `BATEDE -> "BATEDE"
  | `BATEEN -> "BATEEN"
  | `BATEUK -> "BATEUK"
  | `BATS -> "BATS"
  | `BEX -> "BEX"
  | `BM -> "BM"
  | `BOX -> "BOX"
  | `BTRADE -> "BTRADE"
  | `BVME -> "BVME"
  | `BYX -> "BYX"
  | `CBOE -> "CBOE"
  | `CBOE2 -> "CBOE2"
  | `CBSX -> "CBSX"
  | `CDE -> "CDE"
  | `CFE -> "CFE"
  | `CHIXCH -> "CHIXCH"
  | `CHIXDE -> "CHIXDE"
  | `CHIXEN -> "CHIXEN"
  | `CHIXJ -> "CHIXJ"
  | `CHIXUK -> "CHIXUK"
  | `CHX -> "CHX"
  | `CME -> "CME"
  | `CSFBALGO -> "CSFBALGO"
  | `DRCTEDGE -> "DRCTEDGE"
  | `DTB -> "DTB"
  | `ECBOT -> "ECBOT"
  | `EDGEA -> "EDGEA"
  | `EDGX -> "EDGX"
  | `ELX -> "ELX"
  | `FTA -> "FTA"
  | `FWB -> "FWB"
  | `GEMINI -> "GEMINI"
  | `GLOBEX -> "GLOBEX"
  | `HKFE -> "HKFE"
  | `HKMEX -> "HKMEX"
  | `IBCFD -> "IBCFD"
  | `IBIS -> "IBIS"
  | `IBSX -> "IBSX"
  | `ICEUS -> "ICEUS"
  | `IDEAL -> "IDEAL"
  | `IDEALPRO -> "IDEALPRO"
  | `IDEM -> "IDEM"
  | `IEX -> "IEX"
  | `ISE -> "ISE"
  | `ISLAND -> "ISLAND"
  | `JEFFALGO -> "JEFFALGO"
  | `KSE -> "KSE"
  | `LAVA -> "LAVA"
  | `LIFFE -> "LIFFE"
  | `LSE -> "LSE"
  | `MATIF -> "MATIF"
  | `MEFF -> "MEFF"
  | `MERCURY -> "MERCURY"
  | `MEXDER -> "MEXDER"
  | `MEXI -> "MEXI"
  | `MIAX -> "MIAX"
  | `MIBSX -> "MIBSX"
  | `MONEP -> "MONEP"
  | `NASDAQ -> "NASDAQ"
  | `NASDAQBX -> "NASDAQBX"
  | `NASDAQOM -> "NASDAQOM"
  | `NFX -> "NFX"
  | `NSX -> "NSX"
  | `NYBOT -> "NYBOT"
  | `NYMEX -> "NYMEX"
  | `NYSE -> "NYSE"
  | `OMEGA -> "OMEGA"
  | `OMS -> "OMS"
  | `ONE -> "ONE"
  | `PEARL -> "PEARL"
  | `PHLX -> "PHLX"
  | `PINK -> "PINK"
  | `PSE -> "PSE"
  | `PSX -> "PSX"
  | `PURE -> "PURE"
  | `SBF -> "SBF"
  | `SBVM -> "SBVM"
  | `SEHK -> "SEHK"
  | `SELECT -> "SELECT"
  | `SFB -> "SFB"
  | `SGX -> "SGX"
  | `SMART -> "SMART"
  | `SNFE -> "SNFE"
  | `SOFFEX -> "SOFFEX"
  | `SWB -> "SWB"
  | `SWX -> "SWX"
  | `TGATE -> "TGATE"
  | `TPLUS2 -> "TPLUS2"
  | `TRADEGATE -> "TRADEGATE"
  | `TRQXCH -> "TRQXCH"
  | `TRQXDE -> "TRQXDE"
  | `TRQXEN -> "TRQXEN"
  | `TSE -> "TSE"
  | `TSEJ -> "TSEJ"
  | `VENTURE -> "VENTURE"
  | `VIRTX -> "VIRTX"
  | `VSE -> "VSE"
  | `VWAP -> "VWAP"

let t_of_tws = function
  | "AEB" -> `AEB
  | "ALPHA" -> `ALPHA
  | "AMEX" -> `AMEX
  | "ARCA" -> `ARCA
  | "ARCAEDGE" -> `ARCAEDGE
  | "ASX" -> `ASX
  | "BATECH" -> `BATECH
  | "BATEDE" -> `BATEDE
  | "BATEEN" -> `BATEEN
  | "BATEUK" -> `BATEUK
  | "BATS" -> `BATS
  | "BEX" -> `BEX
  | "BM" -> `BM
  | "BOX" -> `BOX
  | "BTRADE" -> `BTRADE
  | "BVME" -> `BVME
  | "BYX" -> `BYX
  | "CBOE" -> `CBOE
  | "CBOE2" -> `CBOE2
  | "CBSX" -> `CBSX
  | "CDE" -> `CDE
  | "CFE" -> `CFE
  | "CHIXCH" -> `CHIXCH
  | "CHIXDE" -> `CHIXDE
  | "CHIXEN" -> `CHIXEN
  | "CHIXJ" -> `CHIXJ
  | "CHIXUK" -> `CHIXUK
  | "CHX" -> `CHX
  | "CME" -> `CME
  | "CSFBALGO" -> `CSFBALGO
  | "DRCTEDGE" -> `DRCTEDGE
  | "DTB" -> `DTB
  | "ECBOT" -> `ECBOT
  | "EDGEA" -> `EDGEA
  | "EDGX" -> `EDGX
  | "ELX" -> `ELX
  | "FTA" -> `FTA
  | "FWB" -> `FWB
  | "GEMINI" -> `GEMINI
  | "GLOBEX" -> `GLOBEX
  | "HKFE" -> `HKFE
  | "HKMEX" -> `HKMEX
  | "IBCFD" -> `IBCFD
  | "IBIS" -> `IBIS
  | "IBSX" -> `IBSX
  | "ICEUS" -> `ICEUS
  | "IDEAL" -> `IDEAL
  | "IDEALPRO" -> `IDEALPRO
  | "IDEM" -> `IDEM
  | "IEX" -> `IEX
  | "ISE" -> `ISE
  | "ISLAND" -> `ISLAND
  | "JEFFALGO" -> `JEFFALGO
  | "KSE" -> `KSE
  | "LAVA" -> `LAVA
  | "LIFFE" -> `LIFFE
  | "LSE" -> `LSE
  | "MATIF" -> `MATIF
  | "MEFF" -> `MEFF
  | "MERCURY" -> `MERCURY
  | "MEXDER" -> `MEXDER
  | "MEXI" -> `MEXI
  | "MIAX" -> `MIAX
  | "MIBSX" -> `MIBSX
  | "MONEP" -> `MONEP
  | "NASDAQ" -> `NASDAQ
  | "NASDAQBX" -> `NASDAQBX
  | "NASDAQOM" -> `NASDAQOM
  | "NFX" -> `NFX
  | "NSX" -> `NSX
  | "NYBOT" -> `NYBOT
  | "NYMEX" -> `NYMEX
  | "NYSE" -> `NYSE
  | "OMEGA" -> `OMEGA
  | "OMS" -> `OMS
  | "ONE" -> `ONE
  | "PEARL" -> `PEARL
  | "PHLX" -> `PHLX
  | "PINK" -> `PINK
  | "PSE" -> `PSE
  | "PSX" -> `PSX
  | "PURE" -> `PURE
  | "SBF" -> `SBF
  | "SBVM" -> `SBVM
  | "SEHK" -> `SEHK
  | "SELECT" -> `SELECT
  | "SFB" -> `SFB
  | "SGX" -> `SGX
  | "SMART" -> `SMART
  | "SNFE" -> `SNFE
  | "SOFFEX" -> `SOFFEX
  | "SWB" -> `SWB
  | "SWX" -> `SWX
  | "TGATE" -> `TGATE
  | "TPLUS2" -> `TPLUS2
  | "TRADEGATE" -> `TRADEGATE
  | "TRQXCH" -> `TRQXCH
  | "TRQXDE" -> `TRQXDE
  | "TRQXEN" -> `TRQXEN
  | "TSE" -> `TSE
  | "TSEJ" -> `TSEJ
  | "VENTURE" -> `VENTURE
  | "VIRTX" -> `VIRTX
  | "VSE" -> `VSE
  | "VWAP" -> `VWAP
  | s -> failwithf "Exchange.to_string: %S" s ()

let to_string = tws_of_t
let of_string = t_of_tws

let val_type = Val_type.create tws_of_t t_of_tws
