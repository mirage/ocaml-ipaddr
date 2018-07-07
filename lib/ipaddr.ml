(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
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

type scope =
| Point
| Interface
| Link
| Admin
| Site
| Organization
| Global

let sexp_of_scope, scope_of_sexp =
  let open Sexplib.Sexp in
  let lst = [
    Point, Atom "Point" ;
    Interface, Atom "Interface";
    Link, Atom "Link" ;
    Admin, Atom "Admin" ;
    Site, Atom "Site" ;
    Organization, Atom "Organization" ;
    Global, Atom "Global"
  ]
  in
  (fun scope -> List.assoc scope lst),
  (fun sexp -> fst (List.find (fun (_, sexp') -> match sexp, sexp' with
       | Atom a, Atom b -> String.equal a b
       | _ -> false) lst))

let (~|) = Int32.of_int
let (|~) = Int32.to_int
let (&&&) x y = Int32.logand x y
let (|||) x y = Int32.logor x y
let (<|<) x y = Int32.shift_left x y
let (>|>) x y = Int32.shift_right_logical x y
let (>!)  x y = (x >|> y) &&& 0xFF_l
let (<!)  x y = (x &&& 0xFF_l) <|< y

let need_more x = Parse_error ("not enough data", x)
let too_much x = Parse_error ("too much data", x)

let char_0 = int_of_char '0'
let char_a = int_of_char 'a'
let char_A = int_of_char 'A'

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
  let rec next prev =
    let j = !i in
    if j >= len then prev
    else let c = s.[j] in
         let k = int_of_char c in
         if is_number base k
         then (incr i; next (prev*base + k))
         else prev
  in
  let i = !i in
  if i < len
  then if is_number base (int_of_char s.[i])
    then next 0
    else raise (bad_char i s)
  else raise (need_more s)

let parse_dec_int s i = parse_int 10 s i
let parse_hex_int s i = parse_int 16 s i
let expect_char s i c =
  if !i < String.length s
  then if s.[!i] <> c then raise (bad_char !i s) else incr i
  else raise (need_more s)
let expect_end s i =
  if String.length s <= !i
  then ()
  else raise (bad_char !i s)

let hex_char_of_int = function
  |  0 -> '0'
  |  1 -> '1'
  |  2 -> '2'
  |  3 -> '3'
  |  4 -> '4'
  |  5 -> '5'
  |  6 -> '6'
  |  7 -> '7'
  |  8 -> '8'
  |  9 -> '9'
  | 10 -> 'a'
  | 11 -> 'b'
  | 12 -> 'c'
  | 13 -> 'd'
  | 14 -> 'e'
  | 15 -> 'f'
  |  _ -> raise (Invalid_argument "not a hex int")

let hex_string_of_int32 i = String.make 1 (hex_char_of_int (Int32.to_int i))

module V4 = struct
  type t = int32

  let compare a b = (* ignore the sign *)
    let c = Int32.compare (a >|> 1) (b >|> 1) in
    if c = 0 then Int32.compare (a &&& 1l) (b &&& 1l) else c

  let make a b c d =
    ((~| a <! 24) ||| (~| b <! 16)) ||| ((~| c <! 8) ||| (~| d <! 0))

  (* parsing *)

  let parse_dotted_quad s i =
    let a = parse_dec_int s i in
    expect_char s i '.';
    let b = parse_dec_int s i in
    expect_char s i '.';
    let c = parse_dec_int s i in
    expect_char s i '.';
    let d = parse_dec_int s i in
    let valid a = a land 0xff <> a in
    if valid a
    then raise (Parse_error ("first octet out of bounds", s))
    else if valid b
    then raise (Parse_error ("second octet out of bounds", s))
    else if valid c
    then raise (Parse_error ("third octet out of bounds", s))
    else if valid d
    then raise (Parse_error ("fourth octet out of bounds", s))
    else make a b c d

  (* string conversion *)

  let of_string_raw = parse_dotted_quad

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

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  let pp_hum = pp

  let sexp_of_t i =
    Sexplib.Sexp.Atom (to_string i)

  let t_of_sexp i =
    match i with
    | Sexplib.Sexp.Atom i -> of_string_exn i
    | _ -> raise (Failure "Ipaddr.V4.t: Unexpected non-atom in sexp")

  (* Byte conversion *)

  let of_bytes_raw bs o =
    make
      (Char.code bs.[0 + o])
      (Char.code bs.[1 + o])
      (Char.code bs.[2 + o])
      (Char.code bs.[3 + o])

  let of_bytes_exn bs =
    let len = String.length bs in
    if len > 4 then raise (too_much bs);
    if len < 4 then raise (need_more bs);
    of_bytes_raw bs 0

  let of_bytes bs = try Some (of_bytes_exn bs) with _ -> None

  let to_bytes_raw i b o =
    Bytes.set b (0 + o) (Char.chr ((|~) (i >! 24)));
    Bytes.set b (1 + o) (Char.chr ((|~) (i >! 16)));
    Bytes.set b (2 + o) (Char.chr ((|~) (i >!  8)));
    Bytes.set b (3 + o) (Char.chr ((|~) (i >!  0)))

  let to_bytes i =
    let b = Bytes.create 4 in
    to_bytes_raw i b 0;
    Bytes.to_string b

  (* Int32*)
  let of_int32 i = i
  let to_int32 i = i

  (* Int16 *)
  let of_int16 (a,b) = (~| a <|< 16) ||| (~| b)
  let to_int16 a = ((|~) (a >|> 16), (|~) (a &&& 0xFF_FF_l))

  (** MAC *)
  (** {{:http://tools.ietf.org/html/rfc1112#section-6.2}RFC 1112}. *)
  let multicast_to_mac i =
    let macb = Bytes.create 6 in
    Bytes.set macb 0 (Char.chr 0x01);
    Bytes.set macb 1 (Char.chr 0x00);
    Bytes.set macb 2 (Char.chr 0x5E);
    Bytes.set macb 3 (Char.chr ((|~) (i >|> 16 &&& 0x7F_l)));
    Bytes.set macb 4 (Char.chr ((|~) (i >! 8)));
    Bytes.set macb 5 (Char.chr ((|~) (i >! 0)));
    Macaddr.of_bytes_exn (Bytes.to_string macb)

  (* Host *)
  let to_domain_name i = [
    Int32.to_string (i >!  0);
    Int32.to_string (i >!  8);
    Int32.to_string (i >! 16);
    Int32.to_string (i >! 24);
    "in-addr";
    "arpa";
    "";
  ]

  (* constant *)

  let any         = make   0   0   0   0
  let unspecified = make   0   0   0   0
  let broadcast   = make 255 255 255 255
  let localhost   = make 127   0   0   1
  let nodes       = make 224   0   0   1
  let routers     = make 224   0   0   2

  module Prefix = struct
    type addr = t
    type t = addr * int

    let ip = make

    let mask sz =
      if sz <= 0 then 0_l
      else if sz >= 32 then 0x0_FF_FF_FF_FF_l
      else 0x0_FF_FF_FF_FF_l <|< (32 - sz)

    let make sz pre = (pre &&& (mask sz),sz)

    let sexp_of_t (ip, prefix) =
      Sexplib.Sexp.List [ sexp_of_t ip ; Sexplib.Conv.sexp_of_int prefix ]

    let t_of_sexp = function
      | Sexplib.Sexp.List [ ip ; prefix ] ->
        let prefix' = Sexplib.Conv.int_of_sexp prefix in
        if prefix' < 0 || prefix' > 32 then
          raise (Parse_error ("invalid prefix size", string_of_int prefix'));
        make prefix' (t_of_sexp ip)
      | _ -> raise (Failure "Ipaddr.V4.Prefix.t: Unexpected sexp")

    let compare (pre,sz) (pre',sz') =
      let c = compare pre pre' in
      if c = 0 then Pervasives.compare sz sz' else c

    let network_address (pre,sz) addr =
      pre ||| (addr &&& Int32.lognot (mask sz))

    (* string conversion *)

    let _of_string_raw s i =
      let quad = of_string_raw s i in
      expect_char s i '/';
      let p = parse_dec_int s i in
      if p > 32 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      (p,quad)

    let of_string_raw s i =
      let (p,quad) = _of_string_raw s i in
      make p quad

    let _of_string_exn s =
      let i = ref 0 in
      let res = _of_string_raw s i in
      expect_end s i;
      res

    let of_string_exn s = let (p,quad) = _of_string_exn s in make p quad

    let of_string s = try Some (of_string_exn s) with _ -> None

    let of_address_string_exn s =
      let (p,quad) = _of_string_exn s in (make p quad, quad)

    let of_address_string s = try Some (of_address_string_exn s) with _ -> None

    let of_netmask nm addr =
      let rec find_greatest_one bits i =
        if bits = 0_l then i-1 else find_greatest_one (bits >|> 1) (i+1)
      in
      let one = nm &&& (Int32.neg nm) in
      let sz = 32 - (find_greatest_one one (if one = 0_l then 33 else 0)) in
      if nm <> (mask sz)
      then raise (Parse_error ("invalid netmask",to_string nm))
      else make sz addr

    let to_buffer buf (pre,sz) = Printf.bprintf buf "%a/%d" to_buffer pre sz

    let to_string subnet =
      let b = Buffer.create 18 in
      to_buffer b subnet;
      Buffer.contents b

    let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

    let pp_hum = pp

    let to_address_buffer buf ((_,sz) as subnet) addr =
      to_buffer buf (network_address subnet addr,sz)

    let to_address_string subnet addr =
      let b = Buffer.create 18 in
      to_address_buffer b subnet addr;
      Buffer.contents b

    let mem ip (pre,sz) = let host = 32 - sz in (ip >|> host) = (pre >|> host)

    let subset ~subnet:(pre1,sz1) ~network:(pre2,sz2) =
      sz1 >= sz2 && mem pre1 (pre2,sz2)

    let of_addr ip = make 32 ip

    let global          = make  0 (ip   0   0 0 0)
    let relative        = make  8 (ip   0   0 0 0)
    let loopback        = make  8 (ip 127   0 0 0)
    let link            = make 16 (ip 169 254 0 0)
    let multicast       = make  4 (ip 224   0 0 0)
    let multicast_org   = make 14 (ip 239 192 0 0)
    let multicast_admin = make 16 (ip 239 255 0 0)
    let multicast_link  = make 24 (ip 224   0 0 0)
    (* http://tools.ietf.org/html/rfc2365 *)

    let private_10  = make 8  (ip 10    0 0 0)
    let private_172 = make 12 (ip 172  16 0 0)
    let private_192 = make 16 (ip 192 168 0 0)

    let private_blocks = [
      loopback ; link ; private_10 ; private_172 ; private_192
    ]

    let broadcast (pre,sz) = pre ||| (0x0_FF_FF_FF_FF_l >|> sz)
    let network (pre,sz) = pre
    let bits (pre,sz) = sz
    let netmask subnet = mask (bits subnet)
  end

  (* TODO: this could be optimized with something trie-like *)
  let scope i =
    let mem = Prefix.mem i in
    if mem Prefix.loopback then Interface
    else if mem Prefix.link then Link
    else if List.exists mem Prefix.private_blocks then Organization
    else if i = unspecified then Point
    else if i = broadcast then Admin
    else if mem Prefix.relative then Admin
    else if mem Prefix.multicast
    then (if mem Prefix.multicast_org then Organization
      else if mem Prefix.multicast_admin then Admin
      else if mem Prefix.multicast_link then Link
      else Global)
    else Global

  let is_global i = (scope i) = Global
  let is_multicast i = Prefix.(mem i multicast)
  let is_private i = (scope i) <> Global
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
    (a1 &&& a2, b1 &&& b2, c1 &&& c2, d1 &&& d2)

  let logor (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 ||| a2, b1 ||| b2, c1 ||| c2, d1 ||| d2)

  let lognot (a,b,c,d) = Int32.(lognot a, lognot b, lognot c, lognot d)
end

module V6 = struct
  include B128

  (* TODO: Perhaps represent with bytestring? *)
  let make a b c d e f g h = of_int16 (a,b,c,d,e,f,g,h)

  (* parsing *)
  let parse_ipv6 s i =
    let compressed = ref false in (* :: *)
    let len = String.length s in
    if len < !i + 2 then (raise (need_more s));
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
      else []
    in

    let rec loop nb acc =
      if nb >= 8 then acc
      else if !i >= len
      then acc
      else
        let pos = !i in
        let x = try parse_hex_int s i with _ -> -1 in
        if x < 0 then acc
        else if nb = 7
        then x::acc
        else if !i < len && s.[!i] = ':'
        then begin
          incr i;
          if !i < len
          then if s.[!i] = ':'
            then
              if !compressed then (decr i; x::acc) (* trailing :: *)
              else begin
                compressed:=true;
                incr i;
                loop (nb + 2) (-1::x::acc)
              end
            else begin
              if is_number 16 (int_of_char s.[!i])
              then loop (nb+1) (x::acc)
              else raise (bad_char !i s)
            end
          else raise (need_more s)
        end
        else if !i < len && s.[!i] = '.'
        then begin
          i:= pos;
          let v4 = V4.of_string_raw s i in
          let (hi,lo) = V4.to_int16 v4 in
          lo :: hi :: acc
        end
        else x::acc
    in

    let res = loop (List.length l) l in
    let res_len = List.length res in
    if res_len > 8
    then raise (Parse_error ("too many components",s))
    else if res_len = 0
    then raise (need_more s)
    else
      let a = Array.make 8 0 in
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
        then i - missing
        else begin
          if x land 0xffff <> x
          then raise (Parse_error
                        (Printf.sprintf "component %d out of bounds" i, s));
          a.(i) <- x;
          i - 1
        end
      ) 7 res in
      (if use_bracket then expect_char s i ']');
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
  let to_buffer ?(v4=false) buf addr =

    let (a,b,c,d,e,f,g,h) as comp = to_int16 addr in

    let v4 = match comp with
      | (0,0,0,0,0,0xffff,_,_) -> true
      | _ -> v4
    in

    let rec loop elide zeros acc = function
      | 0 :: xs -> loop elide (zeros - 1) acc xs
      | n :: xs when zeros = 0 -> loop elide 0 (n::acc) xs
      | n :: xs -> loop (min elide zeros) 0 (n::zeros::acc) xs
      | [] ->
        let elide = min elide zeros in
        (if elide < -1 then Some elide else None),
        (if zeros = 0 then acc else zeros::acc)
    in

    let elide,l = loop 0 0 [] [h;g;f;e;d;c;b;a] in
    assert(match elide with Some x when x < -8 -> false | _ -> true);

    let rec cons_zeros l x =
      if x >= 0 then l else cons_zeros (Some 0::l) (x+1)
    in

    let _,lrev = List.fold_left (fun (patt, l) x ->
      if Some x = patt
      then (None, (None::l))
      else if x < 0
      then (patt, (cons_zeros l x))
      else (patt, ((Some x)::l))
    ) (elide, []) l in

    let rec fill = function
      | [Some hi;Some lo] when v4 ->
        let addr = V4.of_int16 (hi, lo) in
        V4.to_buffer buf addr
      | None::xs -> Buffer.add_string buf "::"; fill xs
      | [Some n] -> Printf.bprintf buf "%x" n
      | (Some n)::None::xs -> Printf.bprintf buf "%x::" n; fill xs
      | (Some n)::xs -> Printf.bprintf buf "%x:" n; fill xs
      | [] -> ()
    in fill (List.rev lrev)

  let to_string ?v4 l =
    let buf = Buffer.create 39 in
    to_buffer ?v4 buf l;
    Buffer.contents buf

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  let pp_hum = pp

  let sexp_of_t i =
    Sexplib.Sexp.Atom (to_string i)

  let t_of_sexp i =
    match i with
    | Sexplib.Sexp.Atom i -> of_string_exn i
    | _ -> raise (Failure "Ipaddr.V6.t: Unexpected non-atom in sexp")

  (* byte conversion *)

  let of_bytes_raw bs o = (* TODO : from cstruct *)
    let hihi = V4.of_bytes_raw bs (o + 0) in
    let hilo = V4.of_bytes_raw bs (o + 4) in
    let lohi = V4.of_bytes_raw bs (o + 8) in
    let lolo = V4.of_bytes_raw bs (o + 12) in
    of_int32 (hihi, hilo, lohi, lolo)

  let of_bytes_exn bs = (* TODO : from cstruct *)
    let len = String.length bs in
    if len > 16 then raise (too_much bs);
    if len < 16 then raise (need_more bs);
    of_bytes_raw bs 0

  let of_bytes bs = try Some (of_bytes_exn bs) with _ -> None
  let to_bytes i =
    let bs = Bytes.create 16 in
    to_bytes_raw i bs 0;
    Bytes.to_string bs

  (** MAC *)
  (** {{:https://tools.ietf.org/html/rfc2464#section-7}RFC 2464}. *)
  let multicast_to_mac i =
    let (_,_,_,i) = to_int32 i in
    let macb = Bytes.create 6 in
    Bytes.set macb 0 (Char.chr 0x33);
    Bytes.set macb 1 (Char.chr 0x33);
    Bytes.set macb 2 (Char.chr ((|~) (i >! 24)));
    Bytes.set macb 3 (Char.chr ((|~) (i >! 16)));
    Bytes.set macb 4 (Char.chr ((|~) (i >! 8)));
    Bytes.set macb 5 (Char.chr ((|~) (i >! 0)));
    Macaddr.of_bytes_exn (Bytes.to_string macb)

  (* Host *)
  let to_domain_name (a,b,c,d) = Printf.([
    hex_string_of_int32 ((d >|>  0) &&& 0xF_l);
    hex_string_of_int32 ((d >|>  4) &&& 0xF_l);
    hex_string_of_int32 ((d >|>  8) &&& 0xF_l);
    hex_string_of_int32 ((d >|> 12) &&& 0xF_l);
    hex_string_of_int32 ((d >|> 16) &&& 0xF_l);
    hex_string_of_int32 ((d >|> 20) &&& 0xF_l);
    hex_string_of_int32 ((d >|> 24) &&& 0xF_l);
    hex_string_of_int32 ((d >|> 28) &&& 0xF_l);
    hex_string_of_int32 ((c >|>  0) &&& 0xF_l);
    hex_string_of_int32 ((c >|>  4) &&& 0xF_l);
    hex_string_of_int32 ((c >|>  8) &&& 0xF_l);
    hex_string_of_int32 ((c >|> 12) &&& 0xF_l);
    hex_string_of_int32 ((c >|> 16) &&& 0xF_l);
    hex_string_of_int32 ((c >|> 20) &&& 0xF_l);
    hex_string_of_int32 ((c >|> 24) &&& 0xF_l);
    hex_string_of_int32 ((c >|> 28) &&& 0xF_l);
    hex_string_of_int32 ((b >|>  0) &&& 0xF_l);
    hex_string_of_int32 ((b >|>  4) &&& 0xF_l);
    hex_string_of_int32 ((b >|>  8) &&& 0xF_l);
    hex_string_of_int32 ((b >|> 12) &&& 0xF_l);
    hex_string_of_int32 ((b >|> 16) &&& 0xF_l);
    hex_string_of_int32 ((b >|> 20) &&& 0xF_l);
    hex_string_of_int32 ((b >|> 24) &&& 0xF_l);
    hex_string_of_int32 ((b >|> 28) &&& 0xF_l);
    hex_string_of_int32 ((a >|>  0) &&& 0xF_l);
    hex_string_of_int32 ((a >|>  4) &&& 0xF_l);
    hex_string_of_int32 ((a >|>  8) &&& 0xF_l);
    hex_string_of_int32 ((a >|> 12) &&& 0xF_l);
    hex_string_of_int32 ((a >|> 16) &&& 0xF_l);
    hex_string_of_int32 ((a >|> 20) &&& 0xF_l);
    hex_string_of_int32 ((a >|> 24) &&& 0xF_l);
    hex_string_of_int32 ((a >|> 28) &&& 0xF_l);
    "ip6";
    "arpa";
    "";
  ])

  (* constant *)

  let unspecified       = make      0 0 0 0 0 0 0 0
  let localhost         = make      0 0 0 0 0 0 0 1
  let interface_nodes   = make 0xff01 0 0 0 0 0 0 1
  let link_nodes        = make 0xff02 0 0 0 0 0 0 1
  let interface_routers = make 0xff01 0 0 0 0 0 0 2
  let link_routers      = make 0xff02 0 0 0 0 0 0 2
  let site_routers      = make 0xff05 0 0 0 0 0 0 2

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

    let mask sz = V4.Prefix.(
      mask (sz -  0),
      mask (sz - 32),
      mask (sz - 64),
      mask (sz - 96))

    let make sz pre = (logand pre (mask sz),sz)

    let sexp_of_t (ip, prefix) =
      Sexplib.Sexp.List [ sexp_of_t ip ; Sexplib.Conv.sexp_of_int prefix ]

    let t_of_sexp = function
      | Sexplib.Sexp.List [ ip ; prefix ] ->
        let prefix' = Sexplib.Conv.int_of_sexp prefix in
        if prefix' < 0 || prefix' > 128 then
          raise (Parse_error ("invalid prefix size", string_of_int prefix'));
        make prefix' (t_of_sexp ip)
      | _ -> raise (Failure "Ipaddr.V6.Prefix.t: Unexpected sexp")

    let network_address (pre,sz) addr =
      logor pre (logand addr (lognot (mask sz)))

    let _of_string_raw s i =
      let v6 = of_string_raw s i in
      expect_char s i '/';
      let p = parse_dec_int s i in
      if p > 128 || p < 0
      then raise (Parse_error ("invalid prefix size", s));
      (p, v6)

    let of_string_raw s i =
      let (p,v6) = _of_string_raw s i in
      make p v6

    let _of_string_exn s =
      let i = ref 0 in
      let res = _of_string_raw s i in
      expect_end s i;
      res

    let of_string_exn s = let (p,v6) = _of_string_exn s in make p v6

    let of_string s = try Some (of_string_exn s) with _ -> None

    let of_address_string_exn s =
      let (p,v6) = _of_string_exn s in (make p v6, v6)

    let of_address_string s = try Some (of_address_string_exn s) with _ -> None

    let of_netmask nm addr =
      make (match nm with
      | (0_l,0_l,0_l,0_l) -> 0
      | (lsw ,0_l ,0_l ,0_l) -> V4.Prefix.(bits (of_netmask lsw V4.any))
      | (-1_l,lsw ,0_l ,0_l) -> V4.Prefix.(bits (of_netmask lsw V4.any)) + 32
      | (-1_l,-1_l,lsw ,0_l) -> V4.Prefix.(bits (of_netmask lsw V4.any)) + 64
      | (-1_l,-1_l,-1_l,lsw) -> V4.Prefix.(bits (of_netmask lsw V4.any)) + 96
      | _ -> raise (Parse_error ("invalid netmask", to_string nm))
      ) addr

    let to_buffer buf (pre,sz) =
      Printf.bprintf buf "%a/%d" (to_buffer ~v4:false) pre sz

    let to_string subnet =
      let buf = Buffer.create 43 in
      to_buffer buf subnet;
      Buffer.contents buf

    let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

    let pp_hum = pp

    let to_address_buffer buf ((_,sz) as subnet) addr =
      to_buffer buf (network_address subnet addr,sz)

    let to_address_string subnet addr =
      let b = Buffer.create 43 in
      to_address_buffer b subnet addr;
      Buffer.contents b

    let mem ip (pre,sz) =
      let m = mask sz in
      logand ip m = logand pre m

    let subset ~subnet:(pre1,sz1) ~network:(pre2,sz2) =
      sz1 >= sz2 && mem pre1 (pre2,sz2)

    let of_addr ip = make 128 ip

    let global_unicast_001  = make   3 (ip 0x2000 0 0 0 0 0 0 0)
    let link                = make  64 (ip 0xfe80 0 0 0 0 0 0 0)
    let unique_local        = make   7 (ip 0xfc00 0 0 0 0 0 0 0)
    let multicast           = make   8 (ip 0xff00 0 0 0 0 0 0 0)
    let ipv4_mapped         = make  96 (ip 0 0 0 0 0 0xffff 0 0)
    let noneui64_interface  = make   3 (ip 0x0000 0 0 0 0 0 0 0)
    let solicited_node      = make 104 (ip 0xff02 0 0 0 0 1 0xff00 0)

    let network (pre,sz) = pre
    let bits (pre,sz) = sz
    let netmask subnet = mask (bits subnet)
  end

  (* TODO: This could be optimized with something trie-like *)
  let scope i =
    let mem = Prefix.mem i in
    if mem Prefix.global_unicast_001 then Global
    else if mem Prefix.ipv4_mapped
    (* rfc says they are technically global but... *)
    then V4.scope (let (_,_,_,v4) = to_int32 i in V4.of_int32 v4)
    else if mem Prefix.multicast then
      let (x,_,_,_,_,_,_,_) = to_int16 i in
      match x land 0xf with
      | 0 -> Point
      | 1 -> Interface
      | 2 | 3 -> Link
      | 4 -> Admin
      | 5 | 6 | 7 -> Site
      | 8 | 9 | 10 | 11 | 12 | 13 -> Organization
      | 14 | 15 -> Global
      | _ -> assert false
    else if mem Prefix.link then Link
    else if mem Prefix.unique_local then Global
    else if i = localhost then Interface
    else if i = unspecified then Point
    else Global

  let link_address_of_mac =
    let c b i = Char.code (String.get b i) in
    fun mac ->
      let bmac = Macaddr.to_bytes mac in
      let c_0 = c bmac 0 lxor 2 in
      let addr = make 0 0 0 0
        (c_0      lsl 8 + c bmac 1)
        (c bmac 2 lsl 8 + 0xff    )
        (0xfe00         + c bmac 3)
        (c bmac 4 lsl 8 + c bmac 5)
      in
      Prefix.(network_address link addr)

  let is_global i = (scope i) = Global
  let is_multicast i = Prefix.(mem i multicast)
  let is_private i = (scope i) <> Global
end

type ('v4,'v6) v4v6 = V4 of 'v4 | V6 of 'v6
type t = (V4.t,V6.t) v4v6
let sexp_of_t = function
  | V4 v4 -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "V4" ; V4.sexp_of_t v4 ]
  | V6 v6 -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "V6" ; V6.sexp_of_t v6 ]

let t_of_sexp = function
  | Sexplib.Sexp.List [ Sexplib.Sexp.Atom v ; ip ] ->
    begin match v with
      | "V4" -> V4 (V4.t_of_sexp ip)
      | "V6" -> V6 (V6.t_of_sexp ip)
      | _ -> raise (Failure "Ipaddr.t: unexpected version")
    end
  | _ -> raise  (Failure "Ipaddr.t: Unexpected sexp")

let compare a b = match a,b with
  | V4 a, V4 b -> V4.compare a b
  | V6 a, V6 b -> V6.compare a b
  | V4 _, V6 _ -> -1
  | V6 _, V4 _ -> 1

let to_string = function
  | V4 x -> V4.to_string x
  | V6 x -> V6.to_string x

let to_buffer buf = function
  | V4 x -> V4.to_buffer buf x
  | V6 x -> V6.to_buffer buf x

let pp ppf i =
      Format.fprintf ppf "%s" (to_string i)

let pp_hum = pp

let of_string_raw s offset =
  let len = String.length s in
  if len < !offset + 1 then raise (need_more s);
  match s.[0] with
    | '[' -> V6 (V6.of_string_raw s offset)
    | _ ->
      let pos = !offset in
      try V4 (V4.of_string_raw s offset)
      with Parse_error (v4_msg,_) ->
        offset := pos;
        try V6 (V6.of_string_raw s offset)
        with Parse_error(v6_msg,s) ->
          let msg = Printf.sprintf
            "not an IPv4 address: %s\nnot an IPv6 address: %s"
            v4_msg v6_msg
          in raise (Parse_error (msg,s))

let of_string_exn s = of_string_raw s (ref 0)

let of_string s = try Some (of_string_exn s) with _ -> None

let v6_of_v4 v4 =
  V6.(Prefix.(network_address ipv4_mapped (of_int32 (0l,0l,0l,v4))))

let v4_of_v6 v6 =
  if V6.Prefix.(mem v6 ipv4_mapped)
  then let (_,_,_,v4) = V6.to_int32 v6 in Some V4.(of_int32 v4)
  else None

let to_v4 = function V4 v4 -> Some v4 | V6 v6 -> v4_of_v6 v6

let to_v6 = function V4 v4 -> v6_of_v4 v4 | V6 v6 -> v6

let scope = function V4 v4 -> V4.scope v4 | V6 v6 -> V6.scope v6

let is_global = function
  | V4 v4 -> V4.is_global v4
  | V6 v6 -> V6.is_global v6

let is_multicast = function
  | V4 v4 -> V4.is_multicast v4
  | V6 v6 -> V6.is_multicast v6

let is_private = function
  | V4 v4 -> V4.is_private v4
  | V6 v6 -> V6.is_private v6

let multicast_to_mac = function
  | V4 v4 -> V4.multicast_to_mac v4
  | V6 v6 -> V6.multicast_to_mac v6

let to_domain_name = function
  | V4 v4 -> V4.to_domain_name v4
  | V6 v6 -> V6.to_domain_name v6

module Prefix = struct
  module Addr = struct
    let to_v6 = to_v6
  end

  type addr = t
  type t = (V4.Prefix.t,V6.Prefix.t) v4v6

  let sexp_of_t = function
    | V4 t -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "V4" ; V4.Prefix.sexp_of_t t ]
    | V6 t -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "V6" ; V6.Prefix.sexp_of_t t ]

  let t_of_sexp = function
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom v ; prefix ] ->
    begin match v with
      | "V4" -> V4 (V4.Prefix.t_of_sexp prefix)
      | "V6" -> V6 (V6.Prefix.t_of_sexp prefix)
      | _ -> raise (Failure "Ipaddr.Prefix.t: unexpected version")
    end
  | _ -> raise  (Failure "Ipaddr.Prefix.t: Unexpected sexp")

  let compare a b = match a,b with
    | V4 a , V4 b -> V4.Prefix.compare a b
    | V6 a , V6 b -> V6.Prefix.compare a b
    | V4 _ , V6 _ -> -1
    | V6 _ , V4 _ -> 1

  let of_string_raw s offset =
    let len = String.length s in
    if len < !offset + 1 then raise (need_more s);
    match s.[0] with
      | '[' -> V6 (V6.Prefix.of_string_raw s offset)
      | _ ->
        let pos = !offset in
        try V4 (V4.Prefix.of_string_raw s offset)
        with Parse_error (v4_msg,_) ->
          offset := pos;
          try V6 (V6.Prefix.of_string_raw s offset)
          with Parse_error(v6_msg,s) ->
            let msg = Printf.sprintf
                "not an IPv4 prefix: %s\nnot an IPv6 prefix: %s"
                v4_msg v6_msg
            in raise (Parse_error (msg,s))

  let of_string_exn s = of_string_raw s (ref 0)

  let of_string s = try Some (of_string_exn s) with _ -> None

  let v6_of_v4 v4 = V6.Prefix.make
    (96 + V4.Prefix.bits v4)
    (v6_of_v4 (V4.Prefix.network v4))

  let v4_of_v6 v6 = match v4_of_v6 (V6.Prefix.network v6) with
    | Some v4 -> Some (V4.Prefix.make (V6.Prefix.bits v6 - 96) v4)
    | None -> None

  let to_v4 = function V4 v4 -> Some v4 | V6 v6 -> v4_of_v6 v6

  let to_v6 = function V4 v4 -> v6_of_v4 v4 | V6 v6 -> v6

  let mem ip prefix = V6.Prefix.mem (Addr.to_v6 ip) (to_v6 prefix)

  let subset ~subnet ~network =
    V6.Prefix.subset ~subnet:(to_v6 subnet) ~network:(to_v6 network)

  let of_addr = function
    | V4 p -> V4 (V4.Prefix.of_addr p)
    | V6 p -> V6 (V6.Prefix.of_addr p)

  let to_string = function
    | V4 p -> V4.Prefix.to_string p
    | V6 p -> V6.Prefix.to_string p

  let to_buffer buf = function
    | V4 p -> V4.Prefix.to_buffer buf p
    | V6 p -> V6.Prefix.to_buffer buf p

  let network = function
    | V4 p -> V4 (V4.Prefix.network p)
    | V6 p -> V6 (V6.Prefix.network p)

  let netmask = function
    | V4 p -> V4 (V4.Prefix.netmask p)
    | V6 p -> V6 (V6.Prefix.netmask p)

  let pp ppf i =
    Format.fprintf ppf "%s" (to_string i)

  let pp_hum = pp
end
