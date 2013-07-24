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

(** A library for manipulation of IP address representations. *)

(** Raised when parsing of IP address syntax fails. *)
exception Parse_error of string

(** A collection of functions for IPv4 addresses. *)
module V4 : sig
  type t
  val compare : t -> t -> int

  (** Converts the low bytes of four int32 values into an abstract {! V4.t } *)
  val make : int32 -> int32 -> int32 -> int32 -> t

  val of_string_exn : string -> t
  val of_string : string -> t option
  val of_bytes_exn : string -> t
  val of_bytes : string -> t option
  val of_int32 : int32 -> t

  val to_string : t -> string
  val to_bytes : t -> string
  val to_int32 : t -> int32

  module Prefix : sig
    type addr = t
    type t
    val compare : t -> t -> int

    val mask : int -> addr
    val make : int -> addr -> t

    val of_string_exn : string -> t
    val of_string : string -> t option

    val to_string : t -> string

    val mem : addr -> t -> bool

    val loopback  : t
    val link      : t
    val relative  : t
    val multicast : t

    val private_10  : t
    val private_172 : t
    val private_192 : t
    val private_blocks : t list

    val broadcast : t -> addr

    include Map.OrderedType with type t := t
  end

  val is_private : t -> bool

  include Map.OrderedType with type t := t
end
