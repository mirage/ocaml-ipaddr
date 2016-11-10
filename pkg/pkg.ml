#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opams =
  let lint_deps_excluding = Some ["ounit"; "oUnit"] in
  [Pkg.opam_file ~lint_deps_excluding "opam"]

let unix = Conf.with_pkg ~default:true "base-unix"

let () =
  Pkg.describe ~opams "ipaddr" @@ fun c ->
    let unix = Conf.value c unix in
    Ok [ Pkg.mllib "lib/ipaddr.mllib";
         Pkg.mllib ~cond:unix "lib/ipaddr_unix.mllib";
         Pkg.mllib ~cond:unix "top/ipaddr_top.mllib";
         Pkg.test "lib_test/test_ipaddr";
         Pkg.test "lib_test/test_macaddr";
    ]
