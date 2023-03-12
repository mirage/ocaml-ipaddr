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
module B128 = Ipaddr_internal.B128

(* copied from test_ipaddr.ml *)
let assert_raises ~msg exn test_fn =
  assert_raises ~msg exn (fun () ->
      try test_fn ()
      with rtexn ->
        if exn <> rtexn then (
          Printf.eprintf "Stacktrace for '%s':\n%!" msg;
          Printexc.print_backtrace stderr);
        raise rtexn)

let assert_equal = assert_equal ~printer:Ipaddr_internal.B128.to_string

let test_addition () =
  (* simple addition *)
  let d1 = B128.zero () in
  let d2 = B128.of_string_exn "00000000000000000000000000000001" in
  assert_equal ~msg:"adding one to zero is one" d2 (B128.add_exn d1 d2);

  (* addition carry *)
  let d1 = B128.of_string_exn "000000000000000000ff000000000000" in
  let d2 = B128.of_string_exn "00000000000000000001000000000000" in
  let d3 = B128.of_string_exn "00000000000000000100000000000000" in
  assert_equal ~msg:"test addition carry over" d3 (B128.add_exn d1 d2);

  (* adding one to max_int overflows *)
  let d1 = B128.max_int () in
  let d2 = B128.of_string_exn "00000000000000000000000000000001" in
  assert_raises ~msg:"adding one to max_int overflows" B128.Overflow (fun () ->
      B128.add_exn d1 d2)

let test_subtraction () =
  (* simple subtraction *)
  let d1 = B128.of_string_exn "00000000000000000000000000000001" in
  let d2 = B128.of_string_exn "00000000000000000000000000000001" in
  let d3 = B128.zero () in
  assert_equal ~msg:"subtracting one from one is zero" d3 (B128.sub_exn d1 d2);

  (* subtract carry *)
  let d1 = B128.of_string_exn "00000000000000000000000000000300" in
  let d2 = B128.of_string_exn "0000000000000000000000000000002a" in
  let d3 = B128.of_string_exn "000000000000000000000000000002d6" in
  assert_equal ~msg:"test subtraction carry over" d3 (B128.sub_exn d1 d2);

  (* subtracting one from zero overflows *)
  let d1 = B128.zero () in
  let d2 = B128.of_string_exn "00000000000000000000000000000001" in
  assert_raises ~msg:"subtracting one from min_int overflows" B128.Overflow
    (fun () -> B128.sub_exn d1 d2)

let test_of_to_string () =
  let s = "ff000000000000004200000000000001" in
  OUnit.assert_equal ~msg:"input of of_string is equal to output of to_string" s
    (B128.of_string_exn s |> B128.to_string)

let test_lognot () =
  let d1 = B128.of_string_exn "00000000000000000000000000000001" in
  let d2 = B128.of_string_exn "fffffffffffffffffffffffffffffffe" in
  assert_equal ~msg:"lognot inverts bits" d2 (B128.lognot d1)

let test_shift_left () =
  (* bit shift count, input, expected output *)
  let test_shifts =
    [
      (1, "f0000000000000000000000000000000", "e0000000000000000000000000000000");
      (1, "0000000000000000000000000000000f", "0000000000000000000000000000001e");
      (1, "00000000000000000000000000000001", "00000000000000000000000000000002");
      (2, "f0000000000000000000000000000000", "c0000000000000000000000000000000");
      (2, "0000000000000000000000000000ffff", "0000000000000000000000000003fffc");
      (8, "00000000000000000000000000000100", "00000000000000000000000000010000");
      (9, "f0000000000000000000000000000000", "00000000000000000000000000000000");
      ( 64,
        "00000000000000000000000000000001",
        "00000000000000010000000000000000" );
      ( 127,
        "00000000000000000000000000000001",
        "80000000000000000000000000000000" );
      ( 128,
        "00000000000000000000000000000001",
        "00000000000000000000000000000000" );
    ]
  in
  List.iter
    (fun (bits, input_value, expected_output) ->
      assert_equal
        ~msg:(Printf.sprintf "shift left by %i" bits)
        (B128.of_string_exn expected_output)
        (B128.shift_left (B128.of_string_exn input_value) bits))
    test_shifts

let test_shift_right () =
  (* (bit shift count, input, expected output) *)
  let test_shifts =
    [
      (1, "f0000000000000000000000000000000", "78000000000000000000000000000000");
      (2, "f0000000000000000000000000000000", "3c000000000000000000000000000000");
      (2, "0000000000000000000000000000ffff", "00000000000000000000000000003fff");
      (2, "000000000000000000000000000ffff0", "0000000000000000000000000003fffc");
      (8, "00000000000000000000000000000100", "00000000000000000000000000000001");
      (9, "f0000000000000000000000000000000", "00780000000000000000000000000000");
      ( 32,
        "000000000000000000000000ffffffff",
        "00000000000000000000000000000000" );
      ( 32,
        "0000000000000000aaaabbbbffffffff",
        "000000000000000000000000aaaabbbb" );
      ( 40,
        "0000000000000000aaaabbbbffffffff",
        "00000000000000000000000000aaaabb" );
      ( 64,
        "01000000000000000000000000000000",
        "00000000000000000100000000000000" );
      ( 120,
        "aaaabbbbccccdddd0000000000000000",
        "000000000000000000000000000000aa" );
      ( 127,
        "80000000000000000000000000000000",
        "00000000000000000000000000000001" );
      ( 128,
        "ffff0000000000000000000000000000",
        "00000000000000000000000000000000" );
    ]
  in
  List.iter
    (fun (bits, input_value, expected_output) ->
      assert_equal
        ~msg:(Printf.sprintf "shift right by %i" bits)
        (B128.of_string_exn expected_output)
        (B128.shift_right (B128.of_string_exn input_value) bits))
    test_shifts

let test_byte_module () =
  let assert_equal = OUnit2.assert_equal ~printer:(Printf.sprintf "0x%x") in
  assert_equal ~msg:"get 3 lsb" 0x00 (B128.Byte.get_lsbits 3 0x00);
  assert_equal ~msg:"get 4 lsb" 0x0f (B128.Byte.get_lsbits 4 0xff);
  assert_equal ~msg:"get 5 lsb" 0x10 (B128.Byte.get_lsbits 5 0x10);
  assert_equal ~msg:"get 8 lsb" 0xff (B128.Byte.get_lsbits 8 0xff);

  assert_equal ~msg:"get 3 msb" 0x0 (B128.Byte.get_msbits 3 0x00);
  assert_equal ~msg:"get 4 msb" 0xf (B128.Byte.get_msbits 4 0xff);
  assert_equal ~msg:"get 5 msb" 0x2 (B128.Byte.get_msbits 5 0x10);
  assert_equal ~msg:"get 8 msb" 0xff (B128.Byte.get_msbits 8 0xff);

  assert_equal ~msg:"set 3 msb" 0x20 (B128.Byte.set_msbits 3 0x1 0x00);
  assert_equal ~msg:"set 4 msb" 0xa0 (B128.Byte.set_msbits 4 0xa 0x00);
  assert_equal ~msg:"set 5 msb" 0x98 (B128.Byte.set_msbits 5 0x13 0x00);
  assert_equal ~msg:"set 8 msb" 0xff (B128.Byte.set_msbits 8 0xff 0x00)

let suite =
  "Test B128 module"
  >::: [
         "addition" >:: test_addition;
         "subtraction" >:: test_subtraction;
         "of_to_string" >:: test_of_to_string;
         "lognot" >:: test_lognot;
         "shift_left" >:: test_shift_left;
         "shift_right" >:: test_shift_right;
         "byte_module" >:: test_byte_module;
       ]
;;

let _results = run_test_tt_main suite in
()
