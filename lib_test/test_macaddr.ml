(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open OUnit
open Macaddr

(* TODO: check that only Parse_error exceptions are raised *)

let test_string_rt () =
  let addr = "ca:fe:ba:be:ee:ee" in
  assert_equal ~msg:addr (to_string (of_string_exn addr)) addr

let test_string_rt_bad () =
  let addrs = [
    "ca:fe:ba:be:ee:e";
    "ca:fe:ba:be:ee:eee";
    "ca:fe:ba:be:eeee";
    "ca:fe:ba:be:ee::ee";
    "ca:fe:ba:be:e:eee";
  ] in
  List.iter (fun addr -> assert_equal ~msg:addr (of_string addr) None) addrs

let test_bytes_rt () =
  let addr = "\254\099\003\128\000\000" in
  assert_equal ~msg:(String.escaped addr) (to_bytes (of_bytes_exn addr)) addr

let test_bytes_rt_bad () =
  let addrs = [
    "\254\099\003\128\000";
    "\254\099\003\128\000\000\233";
  ] in
  List.iter (fun addr ->
    assert_equal ~msg:(String.escaped addr) (of_bytes addr) None) addrs

let suite = "Test" >::: [
  "string_rt"            >:: test_string_rt;
  "string_rt_bad"        >:: test_string_rt_bad;
  "bytes_rt"             >:: test_bytes_rt;
  "bytes_rt_bad"         >:: test_bytes_rt_bad;
]
;;
run_test_tt_main suite
