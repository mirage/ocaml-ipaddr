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

exception Parse_error of string * string

type bytes = string

let need_more x = Parse_error ("not enough data", x)
let too_much x = Parse_error ("too much data", x)

let char_0 = Pervasives.int_of_char '0'
let char_a = Pervasives.int_of_char 'a'
let char_A = Pervasives.int_of_char 'A'


let int_of_char c = match c with
  | '0'..'9' -> Pervasives.int_of_char c - char_0
  | 'a'..'f' -> 10 + Pervasives.int_of_char c - char_a
  | 'A'..'F' -> 10 + Pervasives.int_of_char c - char_A
  | _ -> -1

let bad_char i s =
  let msg = Printf.sprintf "invalid character '%c' at %d" s.[i] i
  in Parse_error (msg, s)

let is_number base n = n >=0 && n < base

let parse_int base s i =
  let len = String.length s in
  let rec dec prev =
    let j = !i in
    if j >= len then prev
    else let c = s.[j] in
         let k = int_of_char c in
         if is_number base k
         then (incr i; dec (prev*base + k))
         else prev
  in
  let i = !i in
  if i < len
  then if is_number base (int_of_char s.[i])
    then dec 0
    else raise (bad_char i s)
  else raise (need_more s)

let parse_decimal_int s i = parse_int 10 s i
let parse_hexa_int s i = parse_int 16 s i
let expect_char s i c =
  if !i < String.length s
  then if s.[!i] <> c then raise (bad_char !i s) else incr i
  else raise (need_more s)
let expect_end s i =
  if String.length s <= !i
  then ()
  else raise (bad_char !i s)

module V4 = struct

  let (&&&) x y = Int32.logand x y
  let (|||) x y = Int32.logor x y
  let (<|<) x y = Int32.shift_left x y
  let (>|>) x y = Int32.shift_right_logical x y
  let (>!)  x y = (x >|> y) &&& 0xFF_l
  let (<!)  x y = (x &&& 0xFF_l) <|< y

  type t = int32

  let compare a b = (* ignore the sign *)
    let c = Int32.compare (a >|> 1) (b >|> 1) in
    if c = 0 then Int32.compare (a &&& 1l) (b &&& 1l) else c

  let make a b c d =
    ((a <! 24) ||| (b <! 16)) ||| ((c <! 8) ||| (d <! 0))

  (* parsing *)

  let parse_dotted_quad s i =
    let a = parse_decimal_int s i in
    expect_char s i '.';
    let b = parse_decimal_int s i in
    expect_char s i '.';
    let c = parse_decimal_int s i in
    expect_char s i '.';
    let d = parse_decimal_int s i in
    let valid a = a land 0xff <> a in
    if valid a
    then raise (Parse_error ("first octet out of bounds", s))
    else if valid b
    then raise (Parse_error ("second octet out of bounds", s))
    else if valid c
    then raise (Parse_error ("third octet out of bounds", s))
    else if valid d
    then raise (Parse_error ("fourth octet out of bounds", s))
    else (a, b, c, d)

  (* string convertion *)

  let of_string_raw s offset =
    let a,b,c,d = parse_dotted_quad s offset in
    make (Int32.of_int a) (Int32.of_int b) (Int32.of_int c) (Int32.of_int d)

  let of_string_exn s =
    let o = ref 0 in
    let x = of_string_raw s o in
    expect_end s o;
    x
  let of_string s = try Some (of_string_exn s) with _ -> None

  let to_buffer b i =
    Printf.bprintf b "%ld.%ld.%ld.%ld" (i >! 24) (i >! 16) (i >! 8) (i >! 0)

  let to_string i =
    let b = Buffer.create 15 in
    to_buffer b i;
    Buffer.contents b

  (* Byte convertion *)

  let of_bytes_raw bs o =
    make
      (Int32.of_int (Char.code bs.[0 + o]))
      (Int32.of_int (Char.code bs.[1 + o]))
      (Int32.of_int (Char.code bs.[2 + o]))
      (Int32.of_int (Char.code bs.[3 + o]))

  let of_bytes_exn bs =
    let len = String.length bs in
    if len > 4 then raise (too_much bs);
    if len < 4 then raise (need_more bs);
    of_bytes_raw bs 0

  let of_bytes bs = try Some (of_bytes_exn bs) with _ -> None

  let to_bytes_raw i b o =
    b.[0 + o] <- Char.chr (Int32.to_int (i >! 24));
    b.[1 + o] <- Char.chr (Int32.to_int (i >! 16));
    b.[2 + o] <- Char.chr (Int32.to_int (i >!  8));
    b.[3 + o] <- Char.chr (Int32.to_int (i >!  0))

  let to_bytes i =
    let b = String.create 4 in
    to_bytes_raw i b 0;
    b

  (* Int32*)
  let of_int32 i = i
  let to_int32 i = i

  (* Int16 *)
  let of_int16 (a,b) =
    Int32.(
      logor (shift_left a 16) b)
  let to_int16 a =
    Int32.(
      shift_right_logical a 16,
      logand a 0xffffl)

  (* constant *)

  let any = make 0_l 0_l 0_l 0_l

  let broadcast = make 255_l 255_l 255_l 255_l

  let localhost = make 127_l 0_l 0_l 1_l

  module Prefix = struct
    type addr = t
    type t = addr * int

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Pervasives.compare sz sz' else c

    let ip = make

    let mask sz =
      if sz <= 0 then 0_l
      else if sz >= 32 then 0x0_FF_FF_FF_FF_l
      else 0x0_FF_FF_FF_FF_l <|< (32 - sz)

    let make sz pre = (pre &&& (mask sz),sz)

    (* string convertion *)

    let of_string_exn s =
      let i = ref 0 in
      let quad = of_string_raw s i in
      expect_char s i '/';
      let p = parse_decimal_int s i in
      if p > 32 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      expect_end s i;
      make p quad

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string (pre,sz) = Printf.sprintf "%s/%d" (to_string pre) sz

    let to_buffer buf (pre,sz) = Printf.bprintf buf "%a/%d" to_buffer pre sz

    let mem ip (pre,sz) = let host = 32 - sz in (ip >|> host) = (pre >|> host)

    let global    = make  0 (ip   0_l   0_l 0_l 0_l)
    let relative  = make  8 (ip   0_l   0_l 0_l 0_l)
    let loopback  = make  8 (ip 127_l   0_l 0_l 0_l)
    let link      = make 16 (ip 169_l 254_l 0_l 0_l)
    let multicast = make  4 (ip 224_l   0_l 0_l 0_l)

    let private_10  = make 8  (ip 10_l    0_l 0_l 0_l)
    let private_172 = make 12 (ip 172_l  16_l 0_l 0_l)
    let private_192 = make 16 (ip 192_l 168_l 0_l 0_l)

    let private_blocks = [
      loopback ; link ; private_10 ; private_172 ; private_192
    ]

    let broadcast (pre,sz) = pre ||| (0x0_FF_FF_FF_FF_l >|> sz)
    let network (pre,sz) = pre

    let bits (pre,sz) = sz
  end

  let is_private i = List.exists (Prefix.mem i) Prefix.private_blocks
end

module B128 = struct
  type t = int32 * int32 * int32 * int32

  let of_int64 (a, b) =
    Int64.(
      to_int32 (shift_right_logical a 32),
      to_int32 a,
      to_int32 (shift_right_logical b 32),
      to_int32 b)
  let to_int64 (a,b,c,d) =
    Int64.(
      logor (shift_left (of_int32 a) 32) (of_int32 b),
      logor (shift_left (of_int32 c) 32) (of_int32 d))

  let of_int32 x = x
  let to_int32 x = x

  let of_int16 (a, b, c, d, e, f, g, h) =
    V4.of_int16 (a,b),
    V4.of_int16 (c,d),
    V4.of_int16 (e,f),
    V4.of_int16 (g,h)

  let to_int16 (x,y,z,t) =
    let a,b = V4.to_int16 x
    and c,d = V4.to_int16 y
    and e,f = V4.to_int16 z
    and g,h = V4.to_int16 t
    in
    (a,b,c,d,e,f,g,h)

  let to_bytes_raw (a,b,c,d) byte o =
    V4.to_bytes_raw a byte (o+0);
    V4.to_bytes_raw b byte (o+4);
    V4.to_bytes_raw c byte (o+8);
    V4.to_bytes_raw d byte (o+12)

  let of_bytes_exn bs = (* TODO : from cstruct *)
    let len = String.length bs in
    if len > 16 then raise (too_much bs);
    if len < 16 then raise (need_more bs);
    let hihi = V4.of_bytes_raw bs 0 in
    let hilo = V4.of_bytes_raw bs 4 in
    let lohi = V4.of_bytes_raw bs 8 in
    let lolo = V4.of_bytes_raw bs 12 in
    of_int32 (hihi, hilo, lohi, lolo)

  let compare (a1,b1,c1,d1) (a2,b2,c2,d2) =
    match V4.compare a1 a2 with
      | 0 -> begin
        match V4.compare b1 b2 with
          | 0 -> begin
            match V4.compare c1 c2 with
              | 0 -> V4.compare d1 d2
              | n -> n end
          | n -> n end
      | n -> n

  let logand (a1,b1,c1,d1) (a2,b2,c2,d2) =
    Int32.(logand a1 a2,logand b1 b2,logand c1 c2,logand d1 d2)

end


module V6 = struct

  include B128

  let make a b c d e f g h = B128.of_int16 (a,b,c,d,e,f,g,h)

  (* parsing *)
  let parse_ipv6 s i =
    let compressed = ref false in (* :: *)
    let len = String.length s in
    if len < 2 then (raise (need_more s));
    let use_bracket = s.[!i] = '[';  in
    if use_bracket then incr i;
    (* check if it starts with :: *)
    let l =
      if s.[!i] = ':' then begin
        incr i;
        if s.[!i] = ':' then begin
          compressed := true;
          incr i;
          [-1]
        end
        else
          raise (bad_char !i s);
      end
      else [] in

    let rec loop nb acc =
      if nb >= 8 then acc
      else if !i >= len
      then acc
      else
        try
          let pos = !i in
          let x = parse_hexa_int s i in
          if nb = 7
            then x::acc
            else if !i < len && s.[!i] = ':'
            then
              if !i + 1 < len && s.[!i+1] = ':'
              then
                if !compressed
                then raise (bad_char (!i+1) s)
                else begin compressed:=true; incr i; incr i; loop (nb + 2) (-1::x::acc) end
              else begin incr i; loop (nb+1) (x::acc) end
            else if !i < len && s.[!i] = '.'
            then begin
              i:= pos;
              let v4 = V4.of_string_raw s i in
              let (hi,lo) = V4.to_int16 v4 in
              Int32.(to_int lo:: to_int hi::acc)
            end
            else x::acc
          with Parse_error _ -> acc in

    let res = loop (List.length l) l in
    let res_len = List.length res in
    if res_len > 8
    then raise (Parse_error ("too many component",s))
    else if res_len = 0
    then raise (need_more s)
    else
      let a = Array.create 8 0l in
      let missing =
        if !compressed
        then 8 - (res_len - 1)
        else if res_len <> 8
        then
          if !i < len
          then raise (bad_char !i s)
          else raise (need_more s)
        else 0

      in
      let _ = List.fold_left (fun i x ->
        if x = -1
        then i-missing
        else begin
          a.(i) <- Int32.of_int x;
          pred i
        end
      ) 7 res in
      if use_bracket
      then expect_char s i ']';
      a

  (* string conversion *)

  let of_string_raw s offset =
    let a = parse_ipv6 s offset in
    make a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)

  let of_string_exn s =
    let o = ref 0 in
    let x = of_string_raw s o in
    expect_end s o;
    x

  let of_string s = try Some (of_string_exn s) with _ -> None


  (* http://tools.ietf.org/html/rfc5952 *)
  let to_buffer ?(v4=false) buf i =

    let (a,b,c,d,e,f,g,h) as comp = B128.to_int16 i in
    let comp_rev = [h;g;f;e;d;c;b;a] in

    let rec loop min (zeros : int32) acc = function
      | 0l :: xs -> loop min (Int32.pred zeros) acc xs
      | n :: xs when zeros = 0l-> loop min 0l (n::acc) xs
      | n :: xs ->
        let min = if min < zeros then min else zeros in
        loop min 0l (n::zeros::acc) xs
      | [] ->
        if zeros = 0l
        then (if min < -1l then Some min else None), acc
        else let min = if min < zeros then min else zeros in
             (if min < -1l then Some min else None), zeros::acc in

    let v4 = match comp with
      | (0l,0l,0l,0l,0l,0xFFFF_l,_,_) -> true
      | _ -> v4 in

    let min,l = loop 0l 0l [] comp_rev in
    assert(match min with Some x when x < -8l -> false | _ -> true);

    let rec cons_zeros l x =
      if x >= 0l then l
      else cons_zeros (Some 0l::l) (Int32.succ x) in

    let _,lrev = List.fold_left (fun (patt, l) x ->
      if Some x = patt
      then (None, (None::l))
      else if x < 0l
      then (patt, (cons_zeros l x))
      else (patt, ((Some x)::l))
    ) (min, []) l in


    let rec fill : int32 option list -> unit = function
      | [Some hi;Some lo] when v4 ->
        let i = V4.of_int16 (hi, lo) in
        V4.to_buffer buf i
      | None::xs -> Buffer.add_string buf "::"; fill xs
      | [Some n] -> Printf.bprintf buf "%lx" n
      | (Some n)::None::xs -> Printf.bprintf buf "%lx::" n ; fill xs
      | (Some n)::xs -> Printf.bprintf buf "%lx:" n; fill xs
      | [] -> ()
    in fill (List.rev lrev)

  let to_string ?v4 l =
    let buf = Buffer.create 39 in
    to_buffer ?v4 buf l;
    Buffer.contents buf

  (* byte conversion *)

  let of_bytes_raw bs o = (* TODO : from cstruct *)
    let hihi = V4.of_bytes_raw bs (o + 0) in
    let hilo = V4.of_bytes_raw bs (o + 4) in
    let lohi = V4.of_bytes_raw bs (o + 8) in
    let lolo = V4.of_bytes_raw bs (o + 12) in
    B128.of_int32 (hihi, hilo, lohi, lolo)

  let of_bytes_exn bs = (* TODO : from cstruct *)
    let len = String.length bs in
    if len > 16 then raise (too_much bs);
    if len < 16 then raise (need_more bs);
    of_bytes_raw bs 0

  let of_bytes bs = try Some (of_bytes_exn bs) with _ -> None
  let to_bytes i =
    let bs = String.create 16 in
    B128.to_bytes_raw i bs 0;
    bs

  (* int64 conversion *)
  let of_int64 = B128.of_int64
  let to_int64 = B128.to_int64

 (* constant *)

  let unspecified = make 0l 0l 0l 0l 0l 0l 0l 0l
  let localhost = make 0l 0l 0l 0l 0l 0l 0l 1l

  module Prefix = struct
    type addr = t
    type t = addr * int

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Pervasives.compare sz sz' else c

    let ip = make

    let full =
      let f = 0x0_FFFF_FFFF_l in
      f,f,f,f

    let mask sz =
      V4.Prefix.mask (sz - 0),
      V4.Prefix.mask (sz - 32),
      V4.Prefix.mask (sz - 64),
      V4.Prefix.mask (sz - 96)

    let make sz pre = (B128.logand pre (mask sz),sz)

    let of_string_exn s =
      let i = ref 0 in
      let v6 = of_string_raw s i in
      expect_char s i '/';
      let p = parse_decimal_int s i in
      if p > 128 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      expect_end s i;
      make p v6

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string (pre,sz) = Printf.sprintf "%s/%d" (to_string pre) sz

    let to_buffer buf (pre,sz) = Printf.bprintf buf "%a/%d" (to_buffer ?v4:(Some false)) pre sz

    let mem ip (pre,sz) =
      let host = 128 - sz in
      let m = mask host in
      B128.(logand ip m = logand pre m)

    let global_unicast  = make 3 (ip 0x2000_l 0l 0l 0l 0l 0l 0l 0l)
    let link_local = make 10 (ip 0xFE80_l 0l 0l 0l 0l 0l 0l 0l)
    let unique_local  = make 7  (ip 0xfc00_l 0l 0l 0l 0l 0l 0l 0l)
    let multicast  = make 8 (ip 0xFF00_l 0l 0l 0l 0l 0l 0l 0l)
    let ipv4_mapped = make 96 (ip 0l 0l 0l 0l 0l 0xFFFF_l 0l 0l)


    let network (pre,sz) = pre

    let bits (pre,sz) = sz
  end
end

type t = [ `ipv4 of V4.t | `ipv6 of V6.t ]

let compare a b = match a,b with
  | `ipv4 a, `ipv4 b -> V4.compare a b
  | `ipv6 a, `ipv6 b -> V6.compare a b
  | `ipv4 _, `ipv6 _ -> -1
  | `ipv6 _, `ipv4 _ -> 1

let to_string = function
  | `ipv4 x -> V4.to_string x
  | `ipv6 x -> V6.to_string x

let to_buffer buf = function
  | `ipv4 x -> V4.to_buffer buf x
  | `ipv6 x -> V6.to_buffer buf x

let of_string_raw s offset =
  let len = String.length s in
  if len < !offset + 1 then raise (need_more s);
  match s.[0] with
    | '[' -> `ipv6 (V6.of_string_raw s offset)
    | _ ->
      let pos = !offset in
      try
        `ipv4 (V4.of_string_raw s offset)
      with Parse_error _ ->
        offset := pos;
        `ipv6 (V6.of_string_raw s offset)

let of_string_exn s = of_string_raw s (ref 0)

let of_string s = try Some (of_string_exn s) with _ -> None
