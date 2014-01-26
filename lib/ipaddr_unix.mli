(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Convert to and from [Unix] to [Ipaddr] representations *)

(** [to_inet_addr ip] converts an IPv4 or IPv6 address into an
    equivalent [Unix.inet_addr] *)
val to_inet_addr : Ipaddr.t -> Unix.inet_addr

(** [of_inet_addr ip] converts a [Unix.inet_addr] into an
    equivalent IPv4 or IPv6 address. *)
val of_inet_addr : Unix.inet_addr -> Ipaddr.t

module V4 : sig

  (** [to_inet_addr ip] converts an IPv4 address into an
    equivalent [Unix.inet_addr] *)
  val to_inet_addr : Ipaddr.V4.t -> Unix.inet_addr

  (** [of_inet_addr ip] converts a {! Unix.inet_addr} into an
      equivalent IPv4 address.  If the {! Unix.inet_addr} is
      not a valid IPv4 address, then [None] will be returned. *)
  val of_inet_addr : Unix.inet_addr -> Ipaddr.V4.t option

  (** [of_inet_addr_exn ip] converts a {!Unix.inet_addr} into an
      equivalent IPv4 address.  If the {!Unix.inet_addr} is
      not a valid IPv4 address, then {!Failure} will be raised. *)
  val of_inet_addr_exn : Unix.inet_addr -> Ipaddr.V4.t
end

module V6 : sig

  (** [to_inet_addr ip] converts an IPv6 address into an
    equivalent [Unix.inet_addr] *)
  val to_inet_addr : Ipaddr.V6.t -> Unix.inet_addr

  (** [of_inet_addr ip] converts a {!Unix.inet_addr} into an
      equivalent IPv6 address.  If the {!Unix.inet_addr} is
      not a valid IPv6 address, then [None] will be returned. *)
  val of_inet_addr : Unix.inet_addr -> Ipaddr.V6.t option

  (** [of_inet_addr_exn ip] converts a {!Unix.inet_addr} into an
      equivalent IPv6 address.  If the {!Unix.inet_addr} is
      not a valid IPv6 address, then {!Failure} will be raised. *)
  val of_inet_addr_exn : Unix.inet_addr -> Ipaddr.V6.t

end
