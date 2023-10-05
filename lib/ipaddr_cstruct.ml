(*
 * Copyright (c) 2019 Anil Madhavapeddy
 * Copyright (c) 2014 Nicolás Ojeda Bär
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

let need_more x = Ipaddr.Parse_error ("not enough data", x)

let try_with_result fn a =
  try Ok (fn a)
  with Ipaddr.Parse_error (msg, _) -> Error (`Msg ("Ipaddr: " ^ msg))

module V4 = struct
  let of_cstruct_exn cs =
    let len = Cstruct.length cs in
    if len < 4 then raise (need_more (Cstruct.to_string cs));
    Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 cs 0)

  let of_cstruct cs = try_with_result of_cstruct_exn cs

  let write_cstruct_exn i cs =
    let len = Cstruct.length cs in
    if len < 4 then raise (need_more (Cstruct.to_string cs));
    Cstruct.BE.set_uint32 cs 0 (Ipaddr.V4.to_int32 i)

  let to_cstruct ?(allocator = Cstruct.create) i =
    let cs = allocator 4 in
    write_cstruct_exn i cs;
    cs
end

module V6 = struct
  open Ipaddr.V6

  let of_cstruct_exn cs =
    let len = Cstruct.length cs in
    if len < 16 then raise (need_more (Cstruct.to_string cs));
    of_octets_exn (Cstruct.to_string ~len:16 cs)

  let of_cstruct cs = try_with_result of_cstruct_exn cs

  let write_cstruct_exn i cs =
    let len = Cstruct.length cs in
    if len < 16 then raise (need_more (Cstruct.to_string cs));
    Cstruct.blit_from_string (to_octets i) 0 cs 0 16

  let to_cstruct ?(allocator = Cstruct.create) i =
    let cs = allocator 16 in
    write_cstruct_exn i cs;
    cs
end
