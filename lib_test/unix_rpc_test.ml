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
open Lwt_ounit
open Lwt_ounit_unix

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

(* Parameters: number of clients to accept, the 
 * fd and sockaddr to listen/accept on, and the
 * number of messages each client is expected to
 * send per connection.
 * The thread will do an efficient accept_n to
 * grab as many connections simultaneously as it can.
 *)
let server num_clients fd sockaddr iters_per_client =
  let listen_q = 10 in
  Lwt_unix.listen fd listen_q;
  (* For a single fd, process the RPCs *)
  let process fd =
    let chan = Lwt_rpc_unix.Unix_transport.make fd in
    let n = ref 0 in
    let th,u = Lwt.task () in
    let _ = Lwt_rpc_unix.RPC.Server.server chan 
      (fun metadata req ->
        let res = match req with
         |Foo x -> RFoo (x*100)
         |Bar x -> RBar ("XXX"^x)
        in
        incr n;
        if !n >= iters_per_client then Lwt.wakeup u ();
        return (res,[])
      )
    in
    th
  in
  (* We need to accept a total of num_clients exactly *)
  let rec accept_all acc remaining =
    match_lwt Lwt_unix.accept_n fd listen_q with
    |_, Some exn -> fail exn
    |fds, None -> begin
      let acc_p = List.map (fun (fd,sa) -> process fd) fds in
      let acc = acc_p @ acc in
      match remaining - (List.length fds) with
      |0 -> return acc
      |n when n < 0 -> assert_failure "too many connections"
      |n -> accept_all acc n
    end  
 in
 lwt processing_threads = accept_all [] num_clients in
 Lwt.join processing_threads

(* The client needs to connect to fd and send exactly [iters]
 * RPCs to the server.
 *)
let client fd sockaddr iters =
  let chan = Lwt_rpc_unix.Unix_transport.make fd in
  let rpc = Lwt_rpc_unix.RPC.Client.client chan in
  for_lwt i = 0 to iters - 1 do
    let req = Foo i in
    match_lwt Lwt_rpc_unix.RPC.Client.send rpc req [] with
    |None -> dprintf "None\n%!"; return ()
    |Some (res,mfds) -> dres req res; return ()
  done >>
  Lwt_rpc_unix.RPC.Client.close rpc

(* Run a smoke test to make sure things work, but without the performance
 * counters (which are relatively slow). *)
let rpc_ping_smoke ~rpc_iters =
  let open Lwt_ounit_unix in
  let clients = [ client; client; client ] in
  let ps = { server = server (List.length clients); clients } in
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let sockaddr = make_unix_sockaddr ~name:"rpc_ping" () in
  "rpc_ping_smoke" >::= test_procset_p ~name:"rpc_ping" ~iters:rpc_iters ps sockaddr

let _ =
  let tests = rpc_ping_smoke ~rpc_iters:1000 20 in
  Lwt_ounit.main ~suite_name:"unix_rpc_test" ~tests
