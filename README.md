# ocaml-ipaddr

A library for manipulation of IP (and MAC) address representations.

Features:

 * No dependencies
 * oUnit-based tests
 * IPv4 and IPv6 support
 * IPv4 and IPv6 CIDR prefix support
 * IPv4 and IPv6 CIDR-scoped address support
 * `Ipaddr.V4` and `Ipaddr.V4.Prefix` modules are `Map.OrderedType`
 * `Ipaddr.V6` and `Ipaddr.V6.Prefix` modules are `Map.OrderedType`
 * `Ipaddr` is a `Map.OrderedType`
 * IP address scope classification
 * MAC-48 (Ethernet) address support
 * `Macaddr` is a `Map.OrderedType`
