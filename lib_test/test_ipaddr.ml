(*
 * Copyright (c) 2013-2014 David Sheets <sheets@alum.mit.edu>
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

(*
  check generic address fns
  check generic map support
 *)

let error s msg = s, Parse_error (msg,s)
let need_more s = error s "not enough data"
let too_much s  = error s "too much data"
let bad_char i s =
  error s (Printf.sprintf "invalid character '%c' at %d" s.[i] i)

let assert_raises ~msg exn test_fn =
  assert_raises ~msg exn (fun () ->
    try test_fn ()
    with rtexn -> begin
      (if exn <> rtexn then (
        let bt = Printexc.get_raw_backtrace () in
        Printf.eprintf "Stacktrace for '%s':\n%s"
          msg (Printexc.raw_backtrace_to_string bt);
       ));
      raise rtexn
    end)

module Test_v4 = struct
  let test_string_rt () =
    let addrs = [
      "192.168.0.1", "192.168.0.1";
    ] in
    List.iter (fun (addr,rt) ->
      let os = V4.of_string_exn addr in
      let ts = V4.to_string os in
      assert_equal ~msg:addr ts rt
    ) addrs

  let test_string_rt_bad () =
    let addrs = [
      need_more "192.168.0";
      bad_char 11 "192.168.0.1.1";
      error "192.268.2.1" "second octet out of bounds";
      bad_char 4 "192. 168.1.1";
      bad_char 4 "192..0.1";
      bad_char 3 "192,168.0.1";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:addr exn (fun () -> V4.of_string_exn addr)
    ) addrs

  let test_string_raw_rt () =
    let addrs = [
      ("IP: 192.168.0.1!!!",4),   ("192.168.0.1",15);
      ("IP: 192.168.0.1.1!!!",4), ("192.168.0.1",15);
    ] in
    List.iter (fun ((addr,off),result) ->
      let c = ref off in
      let os = V4.of_string_raw addr c in
      let ts = V4.to_string os in
      assert_equal ~msg:addr (ts,!c) result
    ) addrs

  let test_string_raw_rt_bad () =
    let addrs = [
      (let s = "IP: 192.168.0!!!" in
       (s,4), (Parse_error ("invalid character '!' at 13",s), 13));
    ] in
    List.iter (fun ((addr,off),(exn,cursor)) ->
      let c = ref off in
      assert_raises ~msg:addr exn (fun () -> V4.of_string_raw addr c);
      assert_equal ~msg:(Printf.sprintf "%s cursor <> %d (%d)" addr cursor !c)
        !c cursor
    ) addrs

  let test_bytes_rt () =
    let addr = "\254\099\003\128" in
    assert_equal ~msg:(String.escaped addr)
      V4.(to_bytes (of_bytes_exn addr)) addr

  let test_bytes_rt_bad () =
    let addrs = [
      need_more "\254\099\003";
      too_much "\254\099\003\128\001";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> V4.of_bytes_exn addr)
    ) addrs

  let test_int32_rt () =
    let addr = 0x0_F0_AB_00_01_l in
    assert_equal ~msg:(Printf.sprintf "%08lX" addr)
      V4.(to_int32 (of_int32 addr)) addr

  let test_prefix_string_rt () =
    let subnets = [
      "192.168.0.0/24", "192.168.0.0/24";
      "0.0.0.0/0",      "0.0.0.0/0";
      "192.168.0.1/24", "192.168.0.0/24";
      "192.168.0.0/0",  "0.0.0.0/0";
    ] in
    List.iter (fun (subnet,rt) ->
      assert_equal ~msg:subnet
        V4.Prefix.(to_string (of_string_exn subnet)) rt
    ) subnets

  let test_prefix_string_rt_bad () =
    let subnets = [
      bad_char 9 "192.168.0/24";
      bad_char 10 "192.168.0./24";
      error "192.168.0.0/33" "invalid prefix size";
      bad_char 14 "192.168.0.0/30/1";
      bad_char 12 "192.168.0.0/-1";
    ] in
    List.iter (fun (subnet,exn) ->
      assert_raises ~msg:subnet exn (fun () -> V4.Prefix.of_string_exn subnet)
    ) subnets

  let test_network_address_rt () =
    let netaddrs = [
      "192.168.0.1/24", "192.168.0.0/24", "192.168.0.1";
    ] in
    List.iter (fun (netaddr,net,addr) ->
      let netv4 = V4.Prefix.of_string_exn net in
      let addrv4 = V4.of_string_exn addr in
      let prefix,v4 = V4.Prefix.of_address_string_exn netaddr in
      assert_equal ~msg:(net^" <> "^(V4.Prefix.to_string prefix)) netv4 prefix;
      assert_equal ~msg:(addr^" <> "^(V4.to_string v4)) addrv4 v4;
      let addrstr = V4.Prefix.to_address_string prefix v4 in
      assert_equal ~msg:(netaddr^" <> "^addrstr) netaddr addrstr;
    ) netaddrs

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

  let test_scope () =
    let ip = V4.of_string_exn in
    (*let is subnet addr = V4.Prefix.(mem addr subnet) in*)
    let is_scope scop addr = scop = V4.scope addr in
    let ships = V4.([
      unspecified,         "global",    is_global,          false;
      unspecified,         "multicast", is_multicast,       false;
      unspecified,         "point",     is_scope Point,     true;
      localhost,           "global",    is_global,          false;
      localhost,           "multicast", is_multicast,       false;
      localhost,           "interface", is_scope Interface, true;
      broadcast,           "global",    is_global,          false;
      broadcast,           "multicast", is_multicast,       false;
      broadcast,           "admin",     is_scope Admin,     true;
      nodes,               "global",    is_global,          false;
      nodes,               "multicast", is_multicast,       true;
      nodes,               "interface", is_scope Link,      true;
      routers,             "global",    is_global,          false;
      routers,             "multicast", is_multicast,       true;
      routers,             "link",      is_scope Link,      true;
      ip "192.168.0.1",    "private",   is_private,         true;
      ip "10.3.21.155",    "private",   is_private,         true;
      ip "172.16.0.0",     "private",   is_private,         true;
      ip "172.31.255.255", "private",   is_private,         true;
      ip "172.15.255.255", "private",   is_private,         false;
      ip "172.32.0.0",     "private",   is_private,         false;
    ]) in
    List.iter (fun (addr,lbl,pred,is_mem) ->
      let mems = if is_mem then "" else " not" in
      let msg = (V4.to_string addr)^" is"^mems^" in "^lbl in
      assert_equal ~msg (pred addr) is_mem
    ) ships

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

  let suite = "Test V4" >::: [
    "string_rt"            >:: test_string_rt;
    "string_rt_bad"        >:: test_string_rt_bad;
    "string_raw_rt"        >:: test_string_raw_rt;
    "string_raw_rt_bad"    >:: test_string_raw_rt_bad;
    "bytes_rt"             >:: test_bytes_rt;
    "bytes_rt_bad"         >:: test_bytes_rt_bad;
    "int32_rt"             >:: test_int32_rt;
    "prefix_string_rt"     >:: test_prefix_string_rt;
    "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
    "network_address_rt"   >:: test_network_address_rt;
    "prefix_broadcast"     >:: test_prefix_broadcast;
    "prefix_bits"          >:: test_prefix_bits;
    "scope"                >:: test_scope;
    "map"                  >:: test_map;
    "prefix_map"           >:: test_prefix_map;
    "special_addr"         >:: test_special_addr;
  ]
end


module Test_v6 = struct
  let test_string_rt () =
    let addrs = [
      "2001:db8::ff00:42:8329","2001:db8::ff00:42:8329";
      "::ffff:192.168.1.1",    "::ffff:192.168.1.1";
      "::",                    "::";
      "[::]",                  "::";
      "1:1:1:1::1:1:1",        "1:1:1:1:0:1:1:1";
      "0:0:0:1:1:0:0:0",       "::1:1:0:0:0";
      "0:0:0:1:1::",           "::1:1:0:0:0";
      "::1:0:0:0:0",           "0:0:0:1::";
      "FE80::",                "fe80::";
      "::192.168.0.1",         "::c0a8:1";
    ] in
    List.iter (fun (addr,rt) ->
      let os = V6.of_string_exn addr in
      let ts = V6.to_string os in
      assert_equal ~msg:(addr^" <> "^rt^" ("^ts^")") ts rt
    ) addrs

  let test_string_rt_bad () =
    let addrs = [
      need_more "[";
      need_more "[]"; (* ? *)
      need_more ":";
      need_more "[::";
      bad_char 4 "::1:g:f";
      bad_char 3 "::1::";
      bad_char 4 "1::2::3";
      need_more "1:2:3:4:5:6:7";
      bad_char 15 "1:2:3:4:5:6:7:8:9";
      bad_char 15 "1:2:3:4:5:6:7:8::";
      error "12345::12:2" "component 0 out of bounds";
      bad_char 1 ":1";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:addr exn (fun () -> V6.of_string_exn addr)
    ) addrs

  let test_string_raw_rt () =
    let addrs = [
      ("IP: 2001:db8::ff00:42:8329!",4), ("2001:db8::ff00:42:8329",26);
      ("IP: ::ffff:192.168.1.1 ",4),     ("::ffff:192.168.1.1",22);
      ("IP: :::",4),                     ("::",6);
      ("IP: [::]:",4),                   ("::",8);
      ("IP: 1:1:1:1::1:1:1:1",4),        ("1:1:1:1:0:1:1:1",18);
      ("IP: ::1:1:0:0:0::g",4),          ("::1:1:0:0:0",15);
    ] in
    List.iter (fun ((addr,off),(result,cursor)) ->
      let c = ref off in
      let os = V6.of_string_raw addr c in
      let ts = V6.to_string os in
      let msg = Printf.sprintf "%s at %d: %s at %d <> %s at %d"
        addr off result cursor ts !c
      in assert_equal ~msg (ts,!c) (result,cursor)
    ) addrs

  let test_string_raw_rt_bad () =
    let error (s,c) msg c' = (s,c), (Parse_error (msg,s),c') in
    let need_more loc = error loc "not enough data" in
    let bad_char i (s,c) =
      error (s,c) (Printf.sprintf "invalid character '%c' at %d" s.[i] i) i
    in
    let addrs = [
      need_more   ("IP: [] ",4) 5;
      bad_char 5  ("IP: : ",4);
      bad_char 7  ("IP: [:: ",4);
      bad_char 17 ("IP: 1:2:3:4:5:6:7 ",4);
      error       ("IP: 12345::12:2 ",4) "component 0 out of bounds" 15;
      bad_char 5  ("IP: :1 ",4);
      need_more   ("IP: ::1:1:0:0:0:",4) 16;
      bad_char 8  ("IP: ::1:g:f ",4);
    ] in
    List.iter (fun ((addr,off),(exn,cursor)) ->
      let c = ref off in
      assert_raises ~msg:addr exn (fun () -> V6.of_string_raw addr c);
      assert_equal ~msg:(Printf.sprintf "%s cursor <> %d (%d)" addr cursor !c)
        !c cursor
    ) addrs

  let test_bytes_rt () =
    let addr =
      "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\000\001"
    in
    let v6 = V6.of_bytes_exn addr in
    assert_equal ~msg:(String.escaped addr) V6.(to_bytes v6) addr

  let test_bytes_rt_bad () =
    let addrs = [
      need_more "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\001";
      too_much
        "\000\000\000\000\000\000\000\000\000\000\255\255\192\168\000\000\001";
    ] in
    List.iter (fun (addr,exn) ->
      assert_raises ~msg:(String.escaped addr) exn
        (fun () -> V6.of_bytes_exn addr)
    ) addrs

  let test_int32_rt () =
    let (a,b,c,d) as addr =
      0x2001_0665_l, 0x0000_0000_l, 0xff00_00ff_l, 0xfe00_0001_l
    in
    assert_equal ~msg:(Printf.sprintf "%08lx %08lx %08lx %08lx" a b c d)
      V6.(to_int32 (of_int32 addr)) addr

  let test_prefix_string_rt () =
    let subnets = [
      "2000::/3",              "2000::/3";
      "c012::/2",              "c000::/2";
      "ffff:ffff:ffff::ffff/0",    "::/0";
      "::/0",                      "::/0";
      "::/128",                  "::/128";
      "::1/128",                "::1/128";
      "::/64",                    "::/64";
      "[::]/64",                  "::/64";
    ] in
    List.iter (fun (subnet,rt) ->
      assert_equal ~msg:subnet
        V6.Prefix.(to_string (of_string_exn subnet)) rt
    ) subnets

  let test_prefix_string_rt_bad () =
    let subnets = [
      need_more "/24";
      need_more "::";
      error "::/130" "invalid prefix size";
      bad_char 5 "::/30/1";
      bad_char 7 "2000::/-1";
      bad_char 5 "1::3:/4";
    ] in
    List.iter (fun (subnet,exn) ->
      assert_raises ~msg:subnet exn (fun () -> V6.Prefix.of_string_exn subnet)
    ) subnets

  let test_network_address_rt () =
    let netaddrs = [
      "::1/24", "::/24", "::1";
    ] in
    List.iter (fun (netaddr,net,addr) ->
      let netv4 = V6.Prefix.of_string_exn net in
      let addrv4 = V6.of_string_exn addr in
      let prefix,v4 = V6.Prefix.of_address_string_exn netaddr in
      assert_equal ~msg:(net^" <> "^(V6.Prefix.to_string prefix)) netv4 prefix;
      assert_equal ~msg:(addr^" <> "^(V6.to_string v4)) addrv4 v4;
      let addrstr = V6.Prefix.to_address_string prefix v4 in
      assert_equal ~msg:(netaddr^" <> "^addrstr) netaddr addrstr;
    ) netaddrs

  let test_prefix_bits () =
    let pairs = V6.Prefix.([
      global_unicast_001, 3;
      link,              10;
      unique_local,       7;
      multicast,          8;
      ipv4_mapped,       96;
      noneui64_interface, 3;
    ]) in
    List.iter (fun (subnet,bits) ->
      let msg = (V6.Prefix.to_string subnet) ^ " <> bits "
        ^ (string_of_int bits) in
      assert_equal ~msg (V6.Prefix.bits subnet) bits
    ) pairs

  let test_scope () =
    let localhost_v4 = V6.of_string_exn "::ffff:127.0.0.1" in
    let is subnet addr = V6.Prefix.(mem addr subnet) in
    let is_scope scop addr = scop = V6.scope addr in
    let ships = V6.([
      unspecified,     "global",    is_global,                    false;
      unspecified,     "multicast", is_multicast,                 false;
      unspecified,     "point",     is_scope Point,                true;
      localhost,       "global",    is_global,                    false;
      localhost,       "multicast", is_multicast,                 false;
      localhost,       "interface", is_scope Interface,            true;
      interface_nodes, "global",    is_global,                    false;
      interface_nodes, "multicast", is_multicast,                  true;
      interface_nodes, "interface", is_scope Interface,            true;
      link_nodes,      "global",    is_global,                    false;
      link_nodes,      "multicast", is_multicast,                  true;
      link_nodes,      "link",      is_scope Link,                 true;
      link_routers,    "global",    is_global,                    false;
      link_routers,    "multicast", is_multicast,                  true;
      link_routers,    "link",      is_scope Link,                 true;
      localhost_v4,    "global",    is_global,                    false;
      localhost_v4,    "multicast", is_multicast,                 false;
      localhost_v4,    "ipv4",      is Prefix.ipv4_mapped,         true;
      localhost_v4,    "noneui64",  is Prefix.noneui64_interface,  true;
      localhost_v4,    "global_001",is Prefix.global_unicast_001, false;
      localhost_v4,    "interface", is_scope Interface,            true;
    ]) in
    List.iter (fun (addr,lbl,pred,is_mem) ->
      let mems = if is_mem then "" else " not" in
      let msg = (V6.to_string addr)^" is"^mems^" in "^lbl in
      assert_equal ~msg (pred addr) is_mem
    ) ships

  let test_map () =
    let module M = Map.Make(V6) in
    let maxs = "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" in
    let m = M.add (V6.of_string_exn "::0:0") "min" M.empty in
    let m = M.add (V6.of_string_exn maxs) "the greatest host" m in
    let m = M.add (V6.of_string_exn "::") "the least host" m in
    assert_equal ~msg:"size" (M.cardinal m) 2;
    let (min_key, min_val) = M.min_binding m in
    assert_equal ~msg:("min is '" ^ min_val ^"'") (min_key, min_val)
      (V6.of_string_exn "::0:0:0", "the least host");
    assert_equal ~msg:"max" (M.max_binding m)
      (V6.of_string_exn maxs, "the greatest host")

  let test_prefix_map () =
    let module M = Map.Make(V6.Prefix) in
    let m = M.add (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0")
      "everyone" M.empty in
    let m = M.add (V6.Prefix.of_string_exn "::ffff:192.0.0.0/1")
      "weirdos" m in
    let m = M.add (V6.Prefix.of_string_exn "::ffff:128.0.0.0/1")
      "high-bitters" m in
    let m = M.add (V6.Prefix.of_string_exn "::ffff:254.0.0.0/8")
      "top-end" m in
    let m = M.add (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0")
      "iana" m in
    assert_equal ~msg:"size" (M.cardinal m) 3;
    assert_equal ~msg:"min" (M.min_binding m)
      (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0", "iana");
    assert_equal ~msg:"max" (M.max_binding m)
      (V6.Prefix.of_string_exn "::ffff:254.0.0.0/8", "top-end");
    assert_equal ~msg:"third"
      (M.find (V6.Prefix.of_string_exn "::ffff:128.0.0.0/1") m) "high-bitters"

  let suite = "Test V6" >::: [
    "string_rt"            >:: test_string_rt;
    "string_rt_bad"        >:: test_string_rt_bad;
    "string_raw_rt"        >:: test_string_raw_rt;
    "string_raw_rt_bad"    >:: test_string_raw_rt_bad;
    "bytes_rt"             >:: test_bytes_rt;
    "bytes_rt_bad"         >:: test_bytes_rt_bad;
    "int32_rt"             >:: test_int32_rt;
    "prefix_string_rt"     >:: test_prefix_string_rt;
    "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
    "network_address_rt"   >:: test_network_address_rt;
    "prefix_bits"          >:: test_prefix_bits;
    "scope"                >:: test_scope;
    "map"                  >:: test_map;
    "prefix_map"           >:: test_prefix_map;
  ]
end

;;
let _results = run_test_tt_main Test_v4.suite in
let _results = run_test_tt_main Test_v6.suite in
()
