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

### Documentation

The _still incomplete_ API-documentation of this distribution can be built with `make doc`.
It can also be found [online](http://ogu.bitbucket.org/ibx/api/).

### Prerequisites

Before you start, please install the Interactive Brokers Trader Workstation
[TWS](http://www.interactivebrokers.com/en/p.php?f=tws) or its low-resource alternative
[IB Gateway](http://www.interactivebrokers.com/en/p.php?f=programInterface&ib_entity=llc)
and make sure that the software allows for incoming API connections. How this is
done for TWS can be watched [here](http://www.youtube.com/watch?v=53tmypRq5wI).
For more detailed information please refer to the Interactive Brokers
[API Reference Guide](http://www.interactivebrokers.com/en/software/api/api.htm).

### Examples

This simple example will connect to the IB Gateway running on localhost
and retrieve the last price for a given stock symbol:

    ::: ocaml
    open Core.Std
    open Async.Std
    open Ibx.Std

    let host = "localhost"
    let port = 4001

    let print_last_price symbol =
      Tws.with_client ~host ~port
        ~on_handler_error:(`Call (fun e ->
          eprintf "[Error] Failed to retrieve last price for %s:\n" symbol;
          prerr_endline (Error.to_string_hum e);
          shutdown 1
        ))
        (fun tws ->
          let stock = Contract.stock ~currency:`USD (Symbol.of_string symbol) in
          Tws.trade_snapshot_exn tws ~contract:stock
          >>= fun snapshot ->
          let price = Price.to_float (Trade_snapshot.last_price snapshot) in
          printf "[Info] Last price for %s was %4.2f USD\n" symbol price;
          return ()
        )

    let command =
      Command.async_basic ~summary:"Retrieve last stock price"
        Command.Spec.(
          empty
          +> anon ("STOCK-SYMBOL" %: string)
        )
        (fun symbol () -> print_last_price symbol)

    let () = Command.run command

You can simply build it by running

    $ ocamlbuild -use-ocamlfind -tag thread -pkgs ibx,threads last_price.native

and then use it to retrieve the last price of Apple Inc. as follows:

    $ ./last_price.native AAPL

For more complex examples please refer to the `examples`-directory of this
distribution. You can build the examples by typing

    $ ./configure --enable-examples
    $ make

in the top-level directory of this distribution.

Contact Information
-------------------

In case of bugs, feature requests and similar, please contact:

  * Oliver Gu <gu.oliver@yahoo.com>
