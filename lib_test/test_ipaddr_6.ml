open OUnit
open Ipaddr

(*
  check offset parse errors
  check errors

  check network_address
  check of/to_address_string
  check ipv4 scope
  check generic address fns
  check generic map support
  check multicast scopes

  authors?

  consolidate tests
 *)

let error s msg = s, Parse_error (msg,s)
let need_more s = error s "not enough data"
let too_much s  = error s "too much data"
let bad_char s c i = error s (Printf.sprintf "invalid character '%c' at %d" c i)

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

let test_string_rt () =
  let addrs = [
    "2001:db8::ff00:42:8329","2001:db8::ff00:42:8329";
    "::ffff:192.168.1.1",        "::ffff:192.168.1.1";
    "::",                                        "::";
    "[::]",                                      "::";
    "1:1:1:1::1:1:1",               "1:1:1:1:0:1:1:1";
    "0:0:0:1:1:0:0:0",                  "::1:1:0:0:0";
    "0:0:0:1:1::",                      "::1:1:0:0:0";
    "::1:0:0:0:0",                        "0:0:0:1::";
    "FE80::",                                "fe80::";
  ] in
  List.iter (fun (addr,rt) ->
    let os = V6.of_string_exn addr in
    let ts = V6.to_string os in
    assert_equal ~msg:addr ts rt
  ) addrs

let test_string_rt_bad () =
  let addrs = [
    need_more "[";
    need_more "[]";
    need_more ":";
    need_more "[::";
    bad_char "::1:g:f" 'g' 4;
    bad_char "::1::" ':' 3;
    need_more "1:2:3:4:5:6:7";
    error "12345::12:2" "component 0 out of bounds";
    bad_char ":1" '1' 1;
  ] in
  List.iter (fun (addr,exn) ->
    assert_raises ~msg:addr exn (fun () -> V6.of_string_exn addr)
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
    bad_char "::/30/1" '/' 5;
    bad_char "2000::/-1" '-' 7;
  ] in
  List.iter (fun (subnet,exn) ->
    assert_raises ~msg:subnet exn (fun () -> V6.Prefix.of_string_exn subnet)
  ) subnets

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
    let msg = (V6.Prefix.to_string subnet) ^ " <> bits " ^ (string_of_int bits) in
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
  let m = M.add (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0") "everyone" M.empty in
  let m = M.add (V6.Prefix.of_string_exn "::ffff:192.0.0.0/1") "weirdos" m in
  let m = M.add (V6.Prefix.of_string_exn "::ffff:128.0.0.0/1") "high-bitters" m in
  let m = M.add (V6.Prefix.of_string_exn "::ffff:254.0.0.0/8") "top-end" m in
  let m = M.add (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0") "iana" m in
  assert_equal ~msg:"size" (M.cardinal m) 3;
  assert_equal ~msg:"min" (M.min_binding m)
    (V6.Prefix.of_string_exn "::ffff:0.0.0.0/0", "iana");
  assert_equal ~msg:"max" (M.max_binding m)
    (V6.Prefix.of_string_exn "::ffff:254.0.0.0/8", "top-end");
  assert_equal ~msg:"third"
    (M.find (V6.Prefix.of_string_exn "::ffff:128.0.0.0/1") m) "high-bitters"

let suite_v6 = "Test" >::: [
  "string_rt"            >:: test_string_rt;
  "string_rt_bad"        >:: test_string_rt_bad;
  "bytes_rt"             >:: test_bytes_rt;
  "bytes_rt_bad"         >:: test_bytes_rt_bad;
  "int32_rt"             >:: test_int32_rt;
  "prefix_string_rt"     >:: test_prefix_string_rt;
  "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
  "prefix_bits"          >:: test_prefix_bits;
  "scope"                >:: test_scope;
  "map"                  >:: test_map;
  "prefix_map"           >:: test_prefix_map;
]
;;
run_test_tt_main suite_v6
