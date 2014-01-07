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

type bytes = string

(** A collection of functions for IPv4 addresses. *)
module V4 : sig
  (** Type of the internet protocol address of a host *)
  type t
  val compare : t -> t -> int

  (** Converts the low bytes of four int32 values into an abstract {! V4.t } *)
  val make : int32 -> int32 -> int32 -> int32 -> t


  (** String convertion **)

  (** [of_string_exn ipv4_string] is the network address represented
      by [ipv4_string]. Raises [Parse_error] if [ipv4_string] is not a
      valid representation of an IPv4 address. *)
  val of_string_exn : string -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_string : string -> t option

  (** Same as [of_string_exn] but take as an extra argument the offset
      of the string to read from *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv4] is the dotted decimal string representation
      of [ipv4], i.e. XXX.XX.X.XXX. *)
  val to_string : t -> string

  (** [to_buffer buf ipv4] output the string representation
      of [ipv4] to the buffer [buf]. *)
  val to_buffer : Buffer.t -> t -> unit

  (** Bytes convertion **)

  (** [of_bytes_exn ipv4_octets] is the network address represented
      by [ipv4_octets]. Raises [Parse_error] if [ipv4_octets] is not a
      valid representation of an IPv4 address. *)
  val of_bytes_exn : bytes -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_bytes : bytes -> t option

  (** Same as above but take an extra paramenter, the offset to read the bytes from. *)
  val of_bytes_raw : bytes -> int -> t

  (** [to_bytes ipv4] is a string of size 4 encoding [ipv4]. *)
  val to_bytes : t -> bytes

  (** [to_bytes_raw ipv4 bytes offset] ouput the encoding of [ipv4] into [bytes]
      at offset [offset]. *)
  val to_bytes_raw : t -> bytes -> int -> unit


  (** Int convertion **)

  (** [of_int32 ipv4_packed] is the network address represented by
      [ipv4_packed].*)
  val of_int32 : int32 -> t

  (** [to_int32 ipv4] is the 32-bit packed encoding of [ipv4]. *)
  val to_int32 : t -> int32

  (** [of_int16 ipv4_packed] is the network address represented by
      [ipv4_packed].*)
  val of_int16 : (int32 * int32) -> t

  (** [to_int16 ipv4] is the 16-bit packed encoding of [ipv4]. *)
  val to_int16 : t -> int32 * int32



  (** [any] is 0.0.0.0. *)
  val any : t

  (** [broadcast] is 255.255.255.255. *)
  val broadcast : t

  (** [localhost] is 127.0.0.1. *)
  val localhost : t

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

    (** [to_buffer buf prefix] output the string representation
        of [prefix] to the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** The default route, all addresses in IPv4-space, 0.0.0.0/0. *)
    val global    : t

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

    (** [network subnet] is the network address for [subnet]. *)
    val network : t -> addr

    (** [bits subnet] is the bit size of the [subnet] prefix. *)
    val bits : t -> int

    include Map.OrderedType with type t := t
  end

  (** [is_private ipv4] is a predicate indicating whether [ipv4] represents
      a private endpoint. *)
  val is_private : t -> bool

  include Map.OrderedType with type t := t
end



(** A collection of functions for IPv6 addresses. *)
module V6 : sig
  (** Type of the internet protocol address of a host *)
  type t
  val compare : t -> t -> int

  (** Converts the low bytes of eight int32 values into an abstract {! V6.t } *)
  val make : int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> int32 -> t

  (** String convertion **)

  (** [of_string_exn ipv6_string] is the network address represented
      by [ipv6_string]. Raises [Parse_error] if [ipv6_string] is not a
      valid representation of an IPv6 address. *)
  val of_string_exn : string -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_string : string -> t option

  (** Same as [of_string_exn] but take as an extra argument the offset
      of the string to read from *)
  val of_string_raw : string -> int ref -> t

  (** [to_string ipv6] is the string representation
      of [ipv6], i.e. XXX:XX:X::XXX:XX. *)
  val to_string : ?v4:bool -> t -> string

  (** [to_buffer buf ipv6] output the string representation
      of [ipv6] to the buffer [buf]. *)
  val to_buffer : ?v4:bool -> Buffer.t -> t -> unit

  (** Bytes convertion **)

  (** [of_bytes_exn ipv6_octets] is the network address represented
      by [ipv6_octets]. Raises [Parse_error] if [ipv6_octets] is not a
      valid representation of an IPv6 address. *)
  val of_bytes_exn : bytes -> t

  (** Same as above but returns an option type instead of raising
      an exception. *)
  val of_bytes : bytes -> t option

  (** Same as above but take an extra paramenter, the offset to read the bytes from. *)
  val of_bytes_raw : bytes -> int -> t

  (** [to_bytes ipv6] is a string of size 16 encoding [ipv6]. *)
  val to_bytes : t -> bytes

  (** [to_bytes_raw ipv6 bytes offset] ouput the encoding of [ipv6] into [bytes]
      at offset [offset]. *)
  val to_bytes_raw : t -> bytes -> int -> unit

  (** Int convertion **)

  (** [of_int64 (ho, lo)] is the ipv6 computed from the two int64. *)
  val of_int64 : int64 * int64 -> t
  (** [to_int64 ipv6] is the 128bit packed encoding of [ipv6]. *)
  val to_int64 : t -> int64 * int64

  (** [of_int32 (ho, lo)] is the ipv6 computed from the two int32. *)
  val of_int32 : int32 * int32 * int32 * int32 -> t
  (** [to_int32 ipv6] is the 128bit packed encoding of [ipv6]. *)
  val to_int32 : t -> int32 * int32 * int32 * int32

  (** [of_int16 (ho, lo)] is the ipv6 computed from the two int16. *)
  val of_int16 : int32 * int32 * int32 * int32 * int32 * int32 * int32 * int32 -> t
  (** [to_int16 ipv6] is the 128bit packed encoding of [ipv6]. *)
  val to_int16 : t -> int32 * int32 * int32 * int32 * int32 * int32 * int32 * int32


  (** [unspecified] is :: *)
  val unspecified : t

  (** [localhost] is ::1 *)
  val localhost : t

  (** A module for manipulating IPv6 network prefixes. *)
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

    (** [to_buffer buf prefix] output the string representation
        of [prefix] to the buffer [buf]. *)
    val to_buffer : Buffer.t -> t -> unit

    (** [mem ip subnet] checks whether [ip] is found within [subnet]. *)
    val mem : addr -> t -> bool

    (** Global Unicast, 2000::/3. *)
    val global_unicast    : t

    (** The Unique Local Unicast (ULA), fc00::/7. *)
    val unique_local : t

    (** Link-Scoped Unicast, fe80::/10 *)
    val link_local : t

    (** The multicast network, ff00/8. *)
    val multicast : t

    (** IPv4 mapped addresses, ::ffff/96 *)
    val ipv4_mapped : t

    (** [network subnet] is the network address for [subnet]. *)
    val network : t -> addr

    (** [bits subnet] is the bit size of the [subnet] prefix. *)
    val bits : t -> int

    include Map.OrderedType with type t := t
  end

  include Map.OrderedType with type t := t
end

type t = [ `ipv4 of V4.t | `ipv6 of V6.t ]
val compare : t -> t -> int
val to_string : t -> string
val to_buffer : Buffer.t -> t -> unit
val of_string_exn : string -> t
val of_string : string -> t option
