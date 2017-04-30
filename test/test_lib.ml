(* File: test_lib.ml

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

open Core
open Async
open Ibx

module Console = Textutils.Console.Ansi

type test = {
  test_name : string;
  run : unit -> unit Deferred.t;
}

type suite = {
  suite_name : string;
  tests : test list;
}

let create_test test_name test_fun = { test_name; run = test_fun }
let create_suite suite_name tests = { suite_name; tests }

let (>::) name test_fun = create_test name test_fun
let (>:::) name tests = create_suite name tests

exception Assertion_failure of string
let raise_assertion_failure msg = raise (Assertion_failure msg)

let message (type t) s expected actual =
  let module S = (val s : Sexpable.S with type t = t) in
  sprintf "\nexpected: %s\nreceived: %s"
    (Sexp.to_string (S.sexp_of_t expected))
    (Sexp.to_string (S.sexp_of_t actual))

let assert_string_equal ~expected ~actual =
  if not (String.compare expected actual = 0) then
    raise_assertion_failure (
      message (module String : Sexpable.S with type t = String.t) expected actual
    )

let assert_error_equal ~expected ~actual =
  if not (expected = actual) then
    raise_assertion_failure (
      message (module Error : Sexpable.S with type t = Error.t) expected actual
    )

let assert_query_equal (type s) q ~expected ~actual =
  let module Q = (val q : Query_intf.S with type t = s) in
  if not Q.(expected = actual) then
    raise_assertion_failure (
      message (module Q : Sexpable.S with type t = Q.t) expected actual
    )

let assert_response_equal (type s) r ~expected ~actual =
  let module R = (val r : Response_intf.S with type t = s) in
  if not R.(expected = actual) then
    raise_assertion_failure (
      message (module R : Sexpable.S with type t = R.t) expected actual
    )

let assert_wrapper_equal (type s) w ~expected ~actual =
  let module W = (val w : Response_intf.Wrapper.S with type t = s) in
  if not W.(expected = actual) then
    raise_assertion_failure (
      message (module W : Sexpable.S with type t = W.t) expected actual
    )

let run name suites =
  let num_tests = List.fold suites ~init:0 ~f:(fun sum suite ->
    sum + List.length suite.tests)
  in
  printf "Running %d tests for library %s.\n%!" num_tests name;
  let rec loop_suites suites counter failures =
    match suites with
    | [] ->
      if failures = 0 then begin
        Console.printf [`Bright; `Green] "Done. All tests succeeded.\n";
        return 0
      end else begin
        Console.printf [`Bright; `Red] "Done. %d of %d tests failed.\n" failures num_tests;
        return 1
      end
    | suite :: remaining_suites ->
      loop_tests suite.tests suite.suite_name remaining_suites counter failures
  and loop_tests tests suite_name suites counter failures =
    match tests with
    | [] ->
      loop_suites suites counter failures
    | test :: remaining_tests ->
      let message = sprintf "[%02d/%02d] Running test %s from suite %s"
          counter num_tests test.test_name suite_name
      in
      Log.Global.info "%s" message;
      print_endline message;
      Monitor.try_with (fun () -> test.run ())
      >>= function
      | Ok () ->
        loop_tests remaining_tests suite_name suites (counter + 1) failures
      | Error exn ->
        let err_msg = match Monitor.extract_exn exn with
          | Assertion_failure msg -> msg
          | exn -> Exn.to_string exn
        in
        Console.printf [`Bright; `Red]
          "Test %s from suite %s failed: %s.\n" test.test_name suite_name err_msg;
        loop_tests remaining_tests suite_name suites (counter + 1) (failures + 1)
  in
  loop_suites suites 1 0;
