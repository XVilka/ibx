IBX - OCaml implementation of the Interactive Brokers TWS API
==================================================================

---------------------------------------------------------------------------

IBX is a pure OCaml implementation of the
[Interactive Brokers](http://www.interactivebrokers.com/en/main.php)
Trader Workstation API (TWS API) built on top of Jane Street's Core
and Async library.

__DISCLAIMER:__ This software is not approved by Interactive Brokers or any
of its affiliates. It comes with absolutely no warranty and the use of
this software for actual trading is at your own risk.

Installation
------------

From [OPAM](http://opam.ocamlpro.com)

    $ opam install ibx

From Source

    $ make
    $ make install

Usage
-----

### Prerequisites

Before you start, please install the Interactive Brokers Trader Workstation
[TWS](http://www.interactivebrokers.com/en/p.php?f=tws) or its low-resource alternative
[IB Gateway](http://www.interactivebrokers.com/en/p.php?f=programInterface&ib_entity=llc)
and make sure that the software allows for incoming API connections. How this is
done for TWS can be watched [here](http://www.youtube.com/watch?v=53tmypRq5wI).
For more detailed information please refer to the Interactive Brokers
[API Reference Guide](http://www.interactivebrokers.com/en/software/api/api.htm).

### Examples

To get started please refer to the `examples`-directory of this distribution.


Contact Information
-------------------

In case of bugs, feature requests and similar, please contact:

  * Oliver Gu <gu.oliver@yahoo.com>
