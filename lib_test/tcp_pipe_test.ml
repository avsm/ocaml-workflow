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

let server ~num_clients ~client_iters ~data_size fd sa arg =
  let listen_stream = Tcp_pipe.listen fd in
  let th,u = Lwt.task () in
  let n = ref 0 in
  (* Handle a new connection *)
  let process h =
    let ch = Tcp_pipe.make_flow h in
    lwt () =
      for_lwt i = 0 to client_iters - 1 do
        let data = sprintf "%d*\n%!" i in
        lwt buf = Lwt_flow.TX.alloc ch data_size in
        String.blit data 0 buf 0 (String.length data);
        match_lwt Lwt_flow.TX.send ch buf with
        |true -> return ()
        |false -> assert_failure "shm write failed!\n%!"
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

let client_fun ~client_iters ~data_size fd sockaddr client_iters =
  lwt h = Tcp_pipe.connect fd in
  let ch = Tcp_pipe.make_flow h in
  Lwt_flow.RX.recv
    (fun ext ->
      Lwt_flow.RX.release ch ext
    ) ch

let tests ~num_clients ~client_iters ~data_size ~test_reps =
  let open Lwt_ounit_unix in
  (* Generate the clients based on the number of parallel needed *)
  let clients = repeat (client_fun ~client_iters ~data_size) num_clients in
  (* Generate the server tester *)
  let server = server ~num_clients ~client_iters ~data_size in
  (* The client server pair *)
  let ps = { server; clients } in
  let name = sprintf "tcp_pipe_%d_clients_%d_iters_%d_dsize" num_clients client_iters data_size in
  let sockaddr = make_unix_sockaddr ~name () in
  (name >::= test_procset_p ~name ~ty:Lwt_unix.SOCK_STREAM ~iters:client_iters ps sockaddr) test_reps

let _ = main_perf ~suite_name:"tcp_pipe" ~tests
