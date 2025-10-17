(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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
 *)

exception Parse_error of string * string

let need_more x = Parse_error ("not enough data", x)

let try_with_result fn a =
  try Ok (fn a) with Parse_error (msg, _) -> Error (`Msg ("Macaddr: " ^ msg))

type t = string (* length 6 only *)

let compare = String.compare

(* Raw MAC address off the wire (network endian) *)
let of_octets_exn x =
  if String.length x <> 6 then raise (Parse_error ("MAC is exactly 6 bytes", x))
  else x

let of_octets x = try_with_result of_octets_exn x

exception Invalid_hex_digit of char

let hex_digit c =
  match Char.uppercase_ascii c with
  | '0' .. '9' as c -> Char.code c - 48
  | 'A' .. 'F' as c -> Char.code c - 55
  | c -> raise_notrace (Invalid_hex_digit c)

let hex_byte x i =
  (hex_digit (String.get x i) lsl 4) + hex_digit (String.get x (succ i))

(* Read a MAC address colon-separated string *)
let of_string_exn x =
  if String.length x < (2 * 6) + 5 then raise (need_more x);
  if String.length x <> (2 * 6) + 5 then
    raise (Parse_error ("macaddr string is too long", x));
  let m = Bytes.create 6 in
  try
    for i = 0 to 5 do
      Bytes.set_uint8 m i (hex_byte x (3 * i))
    done;
    let sep = x.[2] in
    (match sep with
    | ':' | '-' -> ()
    | _ ->
        raise
          (Parse_error (Printf.sprintf "Invalid macaddr separator: %C" sep, x)));
    for i = 1 to 4 do
      if x.[(3 * i) + 2] <> sep then
        raise
          (Parse_error
             (Printf.sprintf "Invalid macaddr separator: %C" x.[(3 * i) + 2], x))
    done;
    Bytes.unsafe_to_string m
  with Invalid_hex_digit c ->
    raise (Parse_error (Printf.sprintf "Invalid macaddr hex digit: %C" c, x))

let of_string x = try_with_result of_string_exn x
let chri x i = Char.code x.[i]

let to_string ?(sep = ':') x =
  Printf.sprintf "%02x%c%02x%c%02x%c%02x%c%02x%c%02x" (chri x 0) sep (chri x 1)
    sep (chri x 2) sep (chri x 3) sep (chri x 4) sep (chri x 5)

let to_octets x = x
let pp ppf i = Format.fprintf ppf "%s" (to_string i)
let broadcast = String.make 6 '\255'

let make_local bytegenf =
  let x = Bytes.create 6 in
  (* set locally administered and unicast bits *)
  Bytes.set x 0 (Char.chr (((bytegenf 0 lor 2) lsr 1) lsl 1));
  for i = 1 to 5 do
    Bytes.set x i (Char.chr (bytegenf i))
  done;
  Bytes.unsafe_to_string x

let get_oui x = (chri x 0 lsl 16) lor (chri x 1 lsl 8) lor chri x 2
let is_local x = (chri x 0 lsr 1) land 1 = 1
let is_unicast x = chri x 0 land 1 = 0
