open OUnit
open Ipaddr

let test_string_rt () =
  let addrs = [
    "2001:db8::ff00:42:8329";
    "::ffff:192.168.1.1"
  ] in
  List.iter (fun addr ->
      let os = V6.of_string_exn addr in
      let ts = V6.to_string os in
      assert_equal ~msg:addr ts addr) addrs
let test_string_rt_bad () =
  let addrs = [
    "::1::";
    "1:2:3:4:5:6:7";
    "12345::12:2"
  ] in
  List.iter (fun addr -> assert_equal ~msg:addr V6.(of_string addr) None) addrs


let test_prefix_string_rt () =
  let subnets = [
    "2000::/3";
    "::/0";
  ] in
  List.iter (fun subnet ->
    assert_equal ~msg:subnet
      V6.Prefix.(to_string (of_string_exn subnet)) subnet) subnets

let test_prefix_string_rt_bad () =
  let subnets = [
    "/24", None;
    "::", None;
    "c012::/2", V6.Prefix.of_string "c000::/2";
    "::/130", None;
    "::/30/1", None;
    "ffff:ffff:ffff::ffff/0", V6.Prefix.of_string "::/0";
    "2000::/-1", None;
  ] in
  List.iter (fun (subnet,result) ->
    let r = V6.Prefix.(of_string subnet) in
    let s = match r with None -> "None" | Some p -> V6.Prefix.to_string p in
    let result_str = match result with None -> "None" | Some p -> V6.Prefix.to_string p in
    assert_equal ~msg:(s ^ " <> " ^ result_str) r result
  ) subnets

let test_prefix_bits () =
  let pairs = V6.Prefix.([
    global_unicast, 3;
    unique_local, 7;
    link_local, 10;
    multicast, 8;
    ipv4_mapped, 96;
  ]) in
  List.iter (fun (subnet,bits) ->
    let msg = (V6.Prefix.to_string subnet) ^ " <> bits " ^ (string_of_int bits) in
    assert_equal ~msg (V6.Prefix.bits subnet) bits
  ) pairs

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
  (* "bytes_rt"             >:: test_bytes_rt; *)
  (* "bytes_rt_bad"         >:: test_bytes_rt_bad; *)
  (* "int32_rt"             >:: test_int32_rt; *)
  "prefix_string_rt"     >:: test_prefix_string_rt;
  "prefix_string_rt_bad" >:: test_prefix_string_rt_bad;
  (* "prefix_broadcast"     >:: test_prefix_broadcast; *)
  "prefix_bits"          >:: test_prefix_bits;
  "map"                  >:: test_map;
  "prefix_map"           >:: test_prefix_map;
  (* "special_addr"         >:: test_special_addr; *)
]
;;
run_test_tt_main suite_v6
