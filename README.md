# ocaml-ipaddr

A library for manipulation of IP (and MAC) address representations.

Features:

 * No dependencies
 * oUnit-based tests
 * IPv4 and IPv6 support
 * IPv4 and IPv6 CIDR prefix support
 * IPv4 and IPv6 [CIDR-scoped address](http://tools.ietf.org/html/rfc4291#section-2.3) support
 * `Ipaddr.V4` and `Ipaddr.V4.Prefix` modules are `Map.OrderedType`
 * `Ipaddr.V6` and `Ipaddr.V6.Prefix` modules are `Map.OrderedType`
 * `Ipaddr` and `Ipaddr.Prefix` modules are `Map.OrderedType`
 * IP address scope classification
 * IPv4-mapped addresses in IPv6 (::ffff:0:0/96) are an embedding of IPv4
 * MAC-48 (Ethernet) address support
 * `Macaddr` is a `Map.OrderedType`
