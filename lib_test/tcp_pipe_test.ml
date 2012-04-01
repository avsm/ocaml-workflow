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
  kfprintf xfn stderr "[%d] " (Unix.getpid ())

let server num_clients fd sa client_iters =
  let listen_stream = Tcp_pipe.listen fd in
  let th,u = Lwt.task () in
  let n = ref 0 in
  (* Handle a new connection *)
  let process h =
    let ch = Tcp_pipe.make_flow h in
    lwt () =
      for_lwt i = 0 to client_iters - 1 do
        let data = sprintf "%d*\n%!" i in
        match_lwt Lwt_flow.TX.send ch data with
        |true -> return ()
        |false -> eprintf "TCP write failed\n%!"; exit (-1)
      done
    in
    Lwt_flow.close ch
  in
  let listen_t =
    Lwt_stream.iter_s (fun h ->
      lwt () = process h in
      incr n;
      if !n >= num_clients then Lwt.wakeup u ();
      return ()
    ) listen_stream in
  lwt () = th in
  Lwt.cancel listen_t;
  return ()

let client fd sockaddr num_iters =
  lwt h = Tcp_pipe.connect fd in
  let ch = Tcp_pipe.make_flow h in
  Lwt_flow.RX.recv
    (fun ext ->
      Lwt_flow.RX.release ch ext
    ) ch

let tcp_pipe_test ~rpc_iters =
  let open Lwt_ounit_unix in
  let clients = [ client; ] in
  let ps = { server = server (List.length clients); clients } in
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let sockaddr = make_tcp_sockaddr () in
  "rpc_tcp" >::= test_procset_p ~name:"rpc_tcp"
    ~ty:Lwt_unix.SOCK_STREAM ~iters:rpc_iters ps sockaddr

let _ =
  let tests = tcp_pipe_test ~rpc_iters:100000 2 in
  Lwt_ounit.main ~suite_name:"rpc_tcp" ~tests
