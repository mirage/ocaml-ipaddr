type t
(** Type of the hardware address (MAC) of an ethernet interface. *)

(** Functions converting MAC addresses to bytes/string and vice
    versa. *)

val of_bytes_exn : string -> t
(** [of_bytes_exn buf] is the hardware address extracted from
    [buf]. Raise [Invalid_argument] if [buf] has not length 6. *)

val of_bytes : string -> t option
(** Same as above but return an option type instead of raising an
    exception. *)

val of_string_exn : string -> t
(** [of_string mac_string] is the hardware address represented by
    [mac_string]. Raise [Invalid_argument] if [mac_string] is not a
    valid representation of a MAC address. *)

val of_string : string -> t option
(** Same as above but return an option type instead of raising an
    exception. *)

val to_bytes : t -> string
(** [to_bytes mac_addr] is a string of size 6 encoding [mac_addr]. *)

val to_string : t -> string
(** [to_string mac_addr] is the colon spearated string representation
    of [mac_addr], i.e. xx:xx:xx:xx:xx:xx. *)

val broadcast : t
(** [broadcast] is ff:ff:ff:ff:ff:ff. *)
