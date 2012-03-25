(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
 *)

open Lwt
open Printf
open OUnit

let dprintf fmt =
  let xfn ch = fprintf ch fmt in
  kfprintf xfn stderr "[%d] " (Unix.getpid())

(* Simple dummy protocol to do RPCs with. The RFoo int
 * will be set to 100x the input. *)
type req =
  |Foo of int |Bar of string
type res =
  |RFoo of int |RBar of string

let dreq = function
  |Foo x -> dprintf "req foo %d\n%!" x
  |Bar x -> dprintf "req bar %s\n%!" x

(* Check that dreq = dres *)
let dres req res =
  match req,res with
  |Foo x, RFoo x' -> "dres foo eq" @? ((x*100) = x')
  |Bar x, RBar x' -> "dres bar eq" @? (("XXX"^x) = x')
  |_ -> assert_failure "dreq != dres"

let listen_t iters fd name =
  lwt fd, sa = Lwt_unix.accept fd in
  let chan = Lwt_rpc_unix.Unix_transport.make fd in
  let n = ref 0 in
  let th,u = Lwt.task () in
  let rpc = Lwt_rpc_unix.RPC.Server.server chan 
    (fun metadata req ->
       let res =
         match req with
         |Foo x -> RFoo (x*100)
         |Bar x -> RBar ("XXX"^x)
       in
       dres req res;
       incr n;
       if !n > iters then Lwt.wakeup u ();
       return (res,[])
    )
  in
  th

let connect_t iters fd name =
  let chan = Lwt_rpc_unix.Unix_transport.make fd in
  let rpc = Lwt_rpc_unix.RPC.Client.client chan in
  for_lwt i = 0 to iters do
    let req = Foo i in
    match_lwt Lwt_rpc_unix.RPC.Client.send rpc req [] with
    |None -> dprintf "None\n%!"; return ()
    |Some (res,mfds) -> dres req res; return ()
  done >>
  Lwt_rpc_unix.RPC.Client.close rpc

let rpc_ping =
  let open Lwt_ounit_unix in
  let iters = 100 in
  let cs_fd = (connect_t iters, listen_t iters) in
  List.map (fun ty ->
    let cs = with_domain_socket ~ty "rpc_ping" cs_fd in
    let test_name = sprintf "rpc_ping_%s"
      (match ty with
       |Lwt_unix.SOCK_STREAM -> "stream"
       |Lwt_unix.SOCK_DGRAM -> "dgram"
       |_ -> "???")
    in
    test_name >:: Lwt_ounit_unix.run_client_server test_name cs
  ) [Lwt_unix.SOCK_DGRAM; Lwt_unix.SOCK_STREAM]

let _ =
  let tests = rpc_ping in
  Lwt_ounit.main ~suite_name:"unix_rpc_test" ~tests;
  eprintf "DONE\n%!"
