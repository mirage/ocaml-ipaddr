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

exception Parse_error of string * string

let need_more x = Parse_error ("not enough data", x)

module V4 = struct
  type t = int32

  let (&&&) x y = Int32.logand x y
  let (|||) x y = Int32.logor x y
  let (<|<) x y = Int32.shift_left x y
  let (>|>) x y = Int32.shift_right_logical x y
  let (>!)  x y = (x >|> y) &&& 0xFF_l
  let (<!)  x y = (x &&& 0xFF_l) <|< y

  let compare a b = (* ignore the sign *)
    let c = Int32.compare (a >|> 1) (b >|> 1) in
    if c = 0 then Int32.compare (a &&& 1l) (b &&& 1l) else c

  let make a b c d =
    ((a <! 24) ||| (b <! 16)) ||| ((c <! 8) ||| (d <! 0))

  let of_string_exn s =
    try Scanf.sscanf s "%ld.%ld.%ld.%ld%!"
          (fun a b c d ->
            if a = (a <! 0)
              && b = (b <! 0)
              && c = (c <! 0)
              && d = (d <! 0)
            then make a b c d
            else raise (Parse_error ("octet out of bounds", s)))
    with
    | Scanf.Scan_failure msg -> raise (Parse_error (msg, s))
    | End_of_file -> raise (need_more s)

  let of_string s = try Some (of_string_exn s) with _ -> None

  let of_bytes_exn bs =
    if String.length bs > 4 then raise (Parse_error ("too much data", bs));
    try
      make
        (Int32.of_int (Char.code bs.[0]))
        (Int32.of_int (Char.code bs.[1]))
        (Int32.of_int (Char.code bs.[2]))
        (Int32.of_int (Char.code bs.[3]))
    with
    | Invalid_argument "index out of bounds" -> raise (need_more bs)

  let of_bytes bs = try Some (of_bytes_exn bs) with _ -> None

  let of_int32 i = i

  let to_string i =
    Printf.sprintf "%ld.%ld.%ld.%ld" (i >! 24) (i >! 16) (i >! 8) (i >! 0)

  let to_bytes i =
    let b = String.create 4 in
    b.[0] <- Char.chr (Int32.to_int (i >! 24));
    b.[1] <- Char.chr (Int32.to_int (i >! 16));
    b.[2] <- Char.chr (Int32.to_int (i >!  8));
    b.[3] <- Char.chr (Int32.to_int (i >!  0));
    b

  let to_int32 i = i

  let blank = 0l

  let broadcast = make 255l 255l 255l 255l

  let localhost = make 127l 0l 0l 1l

  module Prefix = struct
    type addr = t
    type t = addr * int

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Pervasives.compare sz sz' else c

    let ip = make

    let mask sz = if sz = 0 then 0_l else 0x0_FF_FF_FF_FF_l <|< (32 - sz)

    let make sz pre = (pre &&& (mask sz),sz)

    let of_string_exn s =
      try Scanf.sscanf s "%ld.%ld.%ld.%ld/%d%!"
            (fun a b c d p ->
              if p > 32 || p < 0
              then raise (Parse_error ("invalid prefix size", s));
              make p (ip a b c d))
      with
      | Scanf.Scan_failure msg -> raise (Parse_error (msg, s))
      | End_of_file -> raise (need_more s)

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string (pre,sz) = Printf.sprintf "%s/%d" (to_string pre) sz

    let mem ip (pre,sz) = let host = 32 - sz in (ip >|> host) = (pre >|> host)

    let loopback  = make  8 (ip 127_l   0_l 0_l 0_l)
    let link      = make 16 (ip 169_l 254_l 0_l 0_l)
    let relative  = make  8 (ip   0_l   0_l 0_l 0_l)
    let multicast = make  4 (ip 224_l   0_l 0_l 0_l)

    let private_10  = make 8  (ip 10_l    0_l 0_l 0_l)
    let private_172 = make 12 (ip 172_l  16_l 0_l 0_l)
    let private_192 = make 16 (ip 192_l 168_l 0_l 0_l)

    let private_blocks = [
      loopback ; link ; private_10 ; private_172 ; private_192
    ]

    let broadcast (pre,sz) = pre ||| (0x0_FF_FF_FF_FF_l >|> sz)
  end

  let is_private i = List.exists (Prefix.mem i) Prefix.private_blocks
end
(*
module V6 = struct
  type t = int64 * int64

  let compare (h,l) (h',l') =
    let c = Int64.compare h h' in
    if c = 0 then Int64.compare l l' else c

  let (++++) x y = Int64.add x y
  let (&&&&) x y = Int64.logand x y
  let (<||<) x y = Int64.shift_left x y
  let (>||>) x y = Int64.shift_right_logical x y

  let make abcd efgh ijkl mnop

  let of_string_exn

  let of_string

  let of_bytes_exn bs = (* from cstruct *)
    let hihi = V4.of_bytes_exn (String.sub bs 0 4) in
    let hilo = V4.of_bytes_exn (String.sub bs 4 4) in
    let lohi = V4.of_bytes_exn (String.sub bs 8 4) in
    let lolo = V4.of_bytes_exn (String.sub bs 12 4) in
    ((Int64.of_int32 hihi) <||< 48) ++++ (Int64.of_int32 hilo),
    ((Int64.of_int32 lohi) <||< 48) ++++ (Int64.of_int32 lolo)

  let of_bytes

  (* TODO: http://tools.ietf.org/html/rfc5952 *)
  let to_string (hi, lo) = (* from cstruct *)
    sprintf "%Lx:%Lx:%Lx:%Lx:%Lx:%Lx:%Lx:%Lx"
      ((hi >||> 48) &&&& 0xffff_L) ((hi >||> 32) &&&& 0xffff_L)
      ((hi >||> 16) &&&& 0xffff_L) ( hi          &&&& 0xffff_L)
      ((lo >||> 48) &&&& 0xffff_L) ((lo >||> 32) &&&& 0xffff_L)
      ((lo >||> 16) &&&& 0xffff_L) ( lo          &&&& 0xffff_L)

  module Prefix = struct

  end
end
*)
(*type t = [ `ipv4 of V4.t | `ipv6 of V6.t ]

let compare a b = match a,b with
  | `ipv4 a, `ipv4 b -> V4.compare a b
  | `ipv6 a, `ipv6 b -> V6.compare a b

let to_string = function
  | `ipv4 x -> V4.to_string x
  | `ipv6 x -> V6.to_string x

let of_string_exn s =
  try
    if s.[0] = '[' then V6.of_string_exn s
    else
  with
  | End_of_file | raise need_more
  | e -> raise e
*)
