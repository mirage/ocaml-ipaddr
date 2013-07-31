type t = string (* length 6 only *)

(* Raw MAC address off the wire (network endian) *)
let of_bytes_exn x =
  if String.length x <> 6 then raise (Invalid_argument x) else x

let of_bytes x = try Some (of_bytes_exn x) with _ -> None

(* Read a MAC address colon-separated string *)
let of_string_exn x =
  let s = String.create 6 in
  Scanf.sscanf x "%2x:%2x:%2x:%2x:%2x:%2x"
    (fun a b c d e f ->
       s.[0] <- Char.chr a;
       s.[1] <- Char.chr b;
       s.[2] <- Char.chr c;
       s.[3] <- Char.chr d;
       s.[4] <- Char.chr e;
       s.[5] <- Char.chr f;
    );
  s

let of_string x = try Some (of_string_exn x) with _ -> None

let to_string x =
    let chri i = Char.code x.[i] in
    Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
       (chri 0) (chri 1) (chri 2) (chri 3) (chri 4) (chri 5)

let to_bytes x = x

let broadcast = String.make 6 '\255'
