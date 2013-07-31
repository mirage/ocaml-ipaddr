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
exception Parse_error of string * string

(** A collection of functions for IPv4 addresses. *)
module V4 : sig
  (** Type of the internet protocol address of a host *)
  type t
  val compare : t -> t -> int

  (** Converts the low bytes of four int32 values into an abstract {! V4.t } *)
  val make : int32 -> int32 -> int32 -> int32 -> t

  (** [of_string_exn ipv4_string] is the network address represented
      by [ipv4_string]. Raises [Parse_error] if [ipv4_string] is not a
      valid representation of an IPv4 address. *)
  val of_string_exn : string -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_string : string -> t option

  (** [of_bytes_exn ipv4_octets] is the network address represented
      by [ipv4_octets]. Raises [Parse_error] if [ipv4_octets] is not a
      valid representation of an IPv4 address. *)
  val of_bytes_exn : string -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_bytes : string -> t option

  (** [of_int32 ipv4_packed] is the network address represented by
      [ipv4_packed].*)
  val of_int32 : int32 -> t

  (** [to_string ipv4] is the dotted decimal string representation
      of [ipv4], i.e. XXX.XX.X.XXX. *)
  val to_string : t -> string

  (** [to_bytes ipv4] is a string of size 4 encoding [ipv4]. *)
  val to_bytes : t -> string

  (** [to_int32 ipv4] is the 32-bit packed encoding of [ipv4]. *)
  val to_int32 : t -> int32

  (** A module for manipulating IPv4 network prefixes. *)
  module Prefix : sig
    type addr = t

    (** Type of a internet protocol subnet *)
    type t
    val compare : t -> t -> int

    (** [mask n] is the pseudo-address of an [n] bit subnet mask. *)
    val mask : int -> addr

    (** [make n addr] is the [n] bit subnet prefix to which [addr] belongs. *)
    val make : int -> addr -> t

    (** [of_string_exn cidr] is the subnet prefix represented by the CIDR
        string, [cidr]. Raises [Parse_error] if [cidr] is not a valid
        representation of a CIDR notation routing prefix. *)
    val of_string_exn : string -> t

    (** Same as above but returns an option type instead of raising
        an exception. *)
    val of_string : string -> t option

    (** [to_string prefix] is the CIDR notation string representation
        of [prefix], i.e. XXX.XX.X.XXX/XX. *)
    val to_string : t -> string

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** The host loopback network, 127.0.0.0/8. *)
    val loopback  : t

    (** The local-link network, 169.254.0.0/16. *)
    val link      : t

    (** The relative addressing network, 0.0.0.0/8. *)
    val relative  : t

    (** The multicast network, 224.0.0.0/4. *)
    val multicast : t

    (** The private subnet with 10 as first octet, 10.0.0.0/8. *)
    val private_10  : t

    (** The private subnet with 172 as first octet, 172.16.0.0/12. *)
    val private_172 : t

    (** The private subnet with 192 as first octet, 192.168.0.0/16. *)
    val private_192 : t

    (** The privately addressable networks: [loopback], [link],
        [private_10], [private_172], [private_192]. *)
    val private_blocks : t list

    (** [broadcast subnet] is the broadcast address for [subnet]. *)
    val broadcast : t -> addr

    include Map.OrderedType with type t := t
  end

  (** [is_private ipv4] is a predicate indicating whether [ipv4] represents
      a private endpoint. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end
