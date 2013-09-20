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
open Ipaddr

(* TODO: check that only Parse_error exceptions are raised *)

let test_string_rt () =
  let addr = "192.168.0.1" in
  assert_equal ~msg:addr V4.(to_string (of_string_exn addr)) addr

let test_string_rt_bad () =
  let addrs = [
    "192.168.0";
    "192.168.0.1.1";
    "192.268.2.1";
    "192. 168.1.1";
    "192..0.1";
    "192,168.0.1";
  ] in
  List.iter (fun addr -> assert_equal ~msg:addr (V4.of_string addr) None) addrs

let test_bytes_rt () =
  let addr = "\254\099\003\128" in
  assert_equal ~msg:(String.escaped addr) V4.(to_bytes (of_bytes_exn addr)) addr

let test_bytes_rt_bad () =
  let addrs = [
    "\254\099\003";
    "\254\099\003\128\001";
  ] in
  List.iter (fun addr ->
    assert_equal ~msg:(String.escaped addr) V4.(of_bytes addr) None) addrs

let test_int32_rt () =
  let addr = 0x0_F0_AB_00_01_l in
  assert_equal ~msg:(Printf.sprintf "%08lX" addr)
    V4.(to_int32 (of_int32 addr)) addr

let test_prefix_string_rt () =
  let subnets = [
    "192.168.0.0/24";
    "0.0.0.0/0";
  ] in
  List.iter (fun subnet ->
    assert_equal ~msg:subnet
      V4.Prefix.(to_string (of_string_exn subnet)) subnet) subnets

let test_prefix_string_rt_bad () =
  let subnets = [
    "192.168.0/24", None;
    "192.168.0./24", None;
    "192.168.0.1/24", V4.Prefix.of_string "192.168.0.0/24";
    "192.168.0.0/33", None;
    "192.168.0.0/30/1", None;
    "192.168.0.0/0", V4.Prefix.of_string "0.0.0.0/0";
    "192.168.0.0/-1", None;
  ] in
  List.iter (fun (subnet,result) ->
    let r = V4.Prefix.(of_string subnet) in
    let s = match r with None -> "None" | Some p -> V4.Prefix.to_string p in
    assert_equal ~msg:(subnet ^ " <> " ^ s) r result
  ) subnets

let test_prefix_broadcast () =
  let pairs = [
    "192.168.0.0/16",   "192.168.255.255";
    "192.168.0.0/24",   "192.168.0.255";
    "192.168.1.1/24",   "192.168.1.255";
    "192.168.0.128/29", "192.168.0.135";
    "0.0.0.0/0",        "255.255.255.255";
  ] in
  List.iter (fun (subnet,bcast) ->
    let r = V4.(to_string (Prefix.(broadcast (of_string_exn subnet)))) in
    assert_equal ~msg:(subnet ^ " <> " ^ r) r bcast
  ) pairs

let test_prefix_bits () =
  let pairs = V4.Prefix.([
    global, 0;
    loopback, 8;
    link, 16;
    relative, 8;
    multicast, 4;
    private_10, 8;
    private_172, 12;
    private_192, 16;
  ]) in
  List.iter (fun (subnet,bits) ->
    let msg = (V4.Prefix.to_string subnet) ^ " <> " ^ (string_of_int bits) in
    assert_equal ~msg (V4.Prefix.bits subnet) bits
  ) pairs

let test_is_private () =
  let pairs = [
    "192.168.0.1",    true;
    "10.3.21.155",    true;
    "172.16.0.0",     true;
    "172.31.255.255", true;
    "172.15.255.255", false;
    "172.32.0.0",     false;
  ] in
  List.iter (fun (addr,p) ->
    assert_equal ~msg:addr V4.(is_private (of_string_exn addr)) p) pairs

let test_map () =
  let module M = Map.Make(V4) in
  let m = M.add (V4.of_string_exn "1.0.0.1") "min" M.empty in
  let m = M.add (V4.of_string_exn "254.254.254.254") "the greatest host" m in
  let m = M.add (V4.of_string_exn "1.0.0.1") "the least host" m in
  assert_equal ~msg:"size" (M.cardinal m) 2;
  let (min_key, min_val) = M.min_binding m in
  assert_equal ~msg:("min is '" ^ min_val ^"'") (min_key, min_val)
    (V4.of_string_exn "1.0.0.1", "the least host");
  assert_equal ~msg:"max" (M.max_binding m)
    (V4.of_string_exn "254.254.254.254", "the greatest host")

let test_prefix_map () =
  let module M = Map.Make(V4.Prefix) in
  let m = M.add (V4.Prefix.of_string_exn "0.0.0.0/0") "everyone" M.empty in
  let m = M.add (V4.Prefix.of_string_exn "192.0.0.0/1") "weirdos" m in
  let m = M.add (V4.Prefix.of_string_exn "128.0.0.0/1") "high-bitters" m in
  let m = M.add (V4.Prefix.of_string_exn "254.0.0.0/8") "top-end" m in
  let m = M.add (V4.Prefix.of_string_exn "0.0.0.0/0") "iana" m in
  assert_equal ~msg:"size" (M.cardinal m) 3;
  assert_equal ~msg:"min" (M.min_binding m)
    (V4.Prefix.of_string_exn "0.0.0.0/0", "iana");
  assert_equal ~msg:"max" (M.max_binding m)
    (V4.Prefix.of_string_exn "254.0.0.0/8", "top-end");
  assert_equal ~msg:"third"
    (M.find (V4.Prefix.of_string_exn "128.0.0.0/1") m) "high-bitters"

let test_special_addr () =
  assert_equal ~msg:"broadcast" V4.broadcast V4.Prefix.(broadcast global);
  assert_equal ~msg:"any"       V4.any       V4.Prefix.(network global);
  assert_equal ~msg:"localhost" true V4.(Prefix.(mem localhost loopback))

let suite = "Test" >::: [
  "string_rt"            >:: test_string_rt;
  "string_rt_bad"        >:: test_string_rt_bad;
  "bytes_rt"             >:: test_bytes_rt;
  "bytes_rt_bad"         >:: test_bytes_rt_bad;
  "int32_rt"             >:: test_int32_rt;
  "prefix_string_rt"     >:: test_prefix_string_rt;
  "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
  "prefix_broadcast"     >:: test_prefix_broadcast;
  "prefix_bits"          >:: test_prefix_bits;
  "is_private"           >:: test_is_private;
  "map"                  >:: test_map;
  "prefix_map"           >:: test_prefix_map;
  "special_addr"         >:: test_special_addr;
]
;;
run_test_tt_main suite
