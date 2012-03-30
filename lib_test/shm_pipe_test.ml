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
  let listen_stream = Shm_pipe.listen fd in
  let th,u = Lwt.task () in
  let n = ref 0 in
  (* Handle a new connection *)
  let process h =
    let ch = Shm_pipe.make_flow h in
    lwt () =
      for_lwt i = 0 to client_iters - 1 do
        let data = sprintf "%d*\n%!" i in
        lwt ext = Lwt_flow.TX.alloc ch (String.length data) in
        let buf = Simplex.buffer ext in
        Lwt_bytes.blit_string_bytes data 0 buf 0 (String.length data);
        match_lwt Lwt_flow.TX.send ch ext with
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

let client fd sockaddr num_iters =
  lwt h = Shm_pipe.connect fd in
  let ch = Shm_pipe.make_flow h in
  Lwt_flow.RX.recv
    (fun ext ->
      let _ = Simplex.buffer ext in
      Lwt_flow.RX.release ch ext
    ) ch

let shm_pipe_test ~rpc_iters =
  let open Lwt_ounit_unix in
  let clients = [ client; ] in
  let ps = { server = server (List.length clients); clients } in
  (* Generate a random sockpath, and do not use tempfile, as 
   * that may be a no-exec mount point *) 
  let sockaddr = make_unix_sockaddr ~name:"shm_pipe" () in
  "rpc_ping_smoke" >::= test_procset_p ~name:"rpc_ping"
    ~ty:Lwt_unix.SOCK_DGRAM ~iters:rpc_iters ps sockaddr

let _ =
  let tests = shm_pipe_test ~rpc_iters:10000 10 in
  Lwt_ounit.main ~suite_name:"shm_pipe_test" ~tests
